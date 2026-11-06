/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.thrift.transport;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.concurrent.atomic.AtomicInteger;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

/**
 * Exercises how {@link TNonblockingSSLSocket} sizes its unwrap buffers when the SSL session reports
 * larger packet or application buffer sizes than it did at construction. The mocked {@link
 * SSLEngine} only decodes a record once the whole record is present and asks for a larger buffer
 * otherwise, as SunJSSE does when the session buffer sizes grow.
 */
public class TestTNonblockingSSLBufferSizing {
  private TNonblockingSSLSocket socket;
  private SSLEngine engine;
  private SocketChannel channel;
  private SocketChannel tcpChannel;
  private Selector selector;
  private SelectionKey key;

  private final AtomicInteger packetSize = new AtomicInteger();
  private final AtomicInteger appSize = new AtomicInteger();
  private final AtomicInteger delivered = new AtomicInteger();

  @AfterEach
  public void close() throws Exception {
    if (socket != null) socket.close();
    if (selector != null) selector.close();
  }

  @Test
  public void testResizesPacketBufferForARecordThatDoesNotFit() throws Exception {
    // netUnwrap starts at 128; the 200-byte record is only decodable after it grows to 256.
    openSocket(128, 256, 64, 64, 200, new byte[] {1, 2, 3, 4, 5}, true, true);
    ByteBuffer out = ByteBuffer.allocate(5);
    assertEquals(5, socket.read(out));
    assertArrayEquals(new byte[] {1, 2, 3, 4, 5}, out.array());
    // The record genuinely exceeded the initial packet buffer, so the pass is not vacuous.
    assertTrue(delivered.get() > 128, "record should have exceeded the initial 128-byte buffer");
  }

  @Test
  public void testFailsWhenThePacketBufferCannotGrow() throws Exception {
    // The session never reports a larger packet size, so the 200-byte record stays unassembled.
    openSocket(128, 128, 64, 64, 200, new byte[] {1, 2, 3, 4, 5}, false, true);
    assertThrows(TTransportException.class, () -> socket.read(ByteBuffer.allocate(5)));
  }

  @Test
  public void testResizesApplicationBufferForPlaintextThatDoesNotFit() throws Exception {
    // appUnwrap starts at 8; the 20-byte plaintext is only deliverable after it grows to 64.
    byte[] plain = new byte[20];
    for (int i = 0; i < plain.length; i++) plain[i] = (byte) (i + 1);
    openSocket(128, 128, 8, 64, 30, plain, true, true);
    ByteBuffer out = ByteBuffer.allocate(20);
    assertEquals(20, socket.read(out));
    assertArrayEquals(plain, out.array());
  }

  @Test
  public void testFailsWhenTheApplicationBufferCannotGrow() throws Exception {
    byte[] plain = new byte[20];
    for (int i = 0; i < plain.length; i++) plain[i] = (byte) (i + 1);
    // appUnwrap stays at 8 while the plaintext needs 20, so no growth can satisfy it.
    openSocket(128, 128, 8, 8, 30, plain, true, false);
    assertThrows(TTransportException.class, () -> socket.read(ByteBuffer.allocate(20)));
  }

  private void openSocket(
      int initialPacket,
      int grownPacket,
      int initialApp,
      int grownApp,
      int recordLen,
      byte[] plaintext,
      boolean mayGrowPacket,
      boolean mayGrowApp)
      throws Exception {
    packetSize.set(initialPacket);
    appSize.set(initialApp);
    SSLContext context = mock(SSLContext.class);
    engine = mock(SSLEngine.class);
    SSLSession session = mock(SSLSession.class);
    when(context.createSSLEngine("localhost", 0)).thenReturn(engine);
    when(engine.getSSLParameters()).thenReturn(new SSLParameters());
    when(engine.getSession()).thenReturn(session);
    when(session.getApplicationBufferSize()).thenAnswer(c -> appSize.get());
    when(session.getPacketBufferSize()).thenAnswer(c -> packetSize.get());
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer src = call.getArgument(0), dst = call.getArgument(1);
              if (src.remaining() < recordLen) {
                // Not the whole record yet: ask for a larger packet buffer, as SunJSSE does.
                if (src.remaining() > 0 && mayGrowPacket) {
                  packetSize.set(grownPacket);
                }
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW,
                    SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                    0,
                    0);
              }
              if (dst.remaining() < plaintext.length) {
                // Whole record present but no room for the plaintext: ask for a larger app buffer.
                if (mayGrowApp) {
                  appSize.set(grownApp);
                }
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_OVERFLOW,
                    SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                    0,
                    0);
              }
              src.position(src.position() + recordLen);
              dst.put(plaintext);
              return new SSLEngineResult(
                  SSLEngineResult.Status.OK,
                  SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                  recordLen,
                  plaintext.length);
            });
    byte[] wire = new byte[recordLen];
    AtomicInteger offset = new AtomicInteger(0);
    channel = mock(SocketChannel.class);
    when(channel.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer dst = call.getArgument(0);
              int left = recordLen - offset.get();
              if (left <= 0) {
                return 0;
              }
              int n = Math.min(dst.remaining(), left);
              dst.put(wire, offset.get(), n);
              offset.addAndGet(n);
              delivered.addAndGet(n);
              return n;
            });
    socket =
        new TNonblockingSSLSocket("localhost", 0, 0, context) {
          {
            tcpChannel = super.getSocketChannel();
          }

          @Override
          public SocketChannel getSocketChannel() {
            return channel;
          }
        };
    selector = Selector.open();
    key = socket.registerSelector(selector, SelectionKey.OP_READ);
  }
}
