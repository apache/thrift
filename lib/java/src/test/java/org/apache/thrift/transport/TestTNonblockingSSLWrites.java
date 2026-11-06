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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicReference;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

public class TestTNonblockingSSLWrites {
  private TNonblockingSSLSocket socket;
  private SSLEngine engine;
  private final ByteArrayOutputStream sent = new ByteArrayOutputStream();

  @AfterEach
  public void close() {
    if (socket != null) socket.close();
  }

  @Test
  public void testPartialAndZeroWritesPreserveRecordOrder() throws Exception {
    openSocket(2, 0, 2, 32);
    ByteBuffer first = ByteBuffer.wrap(new byte[] {1, 2, 3});
    ByteBuffer second = ByteBuffer.wrap(new byte[] {4, 5});
    assertEquals(3, socket.write(first));
    assertArrayEquals(new byte[] {3, 1}, sent.toByteArray());
    assertEquals(0, socket.write(second));
    assertEquals(0, second.position());
    socket.write(second);
    socket.write(ByteBuffer.allocate(0));
    assertEquals(2, second.position());
    assertArrayEquals(new byte[] {3, 1, 2, 3, 2, 4, 5}, sent.toByteArray());
  }

  @Test
  public void testEmptyWriteDrainsLastRecord() throws Exception {
    openSocket(0, 0, 32);
    assertEquals(2, socket.write(ByteBuffer.wrap(new byte[] {8, 9})));
    assertEquals(0, sent.size());
    assertEquals(0, socket.write(ByteBuffer.allocate(0)));
    assertEquals(0, sent.size());
    assertEquals(0, socket.write(ByteBuffer.allocate(0)));
    assertArrayEquals(new byte[] {2, 8, 9}, sent.toByteArray());
  }

  @Test
  public void testLargeWriteYieldsWithPlaintextRemaining() throws Exception {
    openSocket(0, 32, 32);
    ByteBuffer data = ByteBuffer.wrap(new byte[] {1, 2, 3, 4, 5});
    assertEquals(3, socket.write(data));
    assertEquals(2, data.remaining());
    socket.write(data);
    socket.write(ByteBuffer.allocate(0));
    assertArrayEquals(new byte[] {3, 1, 2, 3, 2, 4, 5}, sent.toByteArray());
  }

  @Test
  public void testHandshakeDrainsOutputBeforeCompleting() throws Exception {
    try (ServerSocket listener = new ServerSocket(0);
        Selector selector = Selector.open()) {
      listener.setSoTimeout(5000);
      openSocketAtPort(listener.getLocalPort(), 0, 1, 32);
      AtomicReference<SSLEngineResult.HandshakeStatus> status =
          new AtomicReference<>(SSLEngineResult.HandshakeStatus.NEED_WRAP);
      when(engine.getHandshakeStatus()).thenAnswer(call -> status.get());
      when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
          .thenAnswer(
              call -> {
                ByteBuffer output = call.getArgument(1);
                output.put(new byte[] {42, 43, 44});
                status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
                return new SSLEngineResult(
                    SSLEngineResult.Status.OK, SSLEngineResult.HandshakeStatus.FINISHED, 0, 3);
              });
      socket.registerSelector(selector, SelectionKey.OP_CONNECT);
      boolean connected = socket.startConnect();
      try (Socket peer = listener.accept()) {
        for (int attempt = 0; !connected && attempt < 4; attempt++) {
          connected = socket.finishConnect();
        }
      }
      assertTrue(connected);
      assertTrue(socket.isOpen());
      assertArrayEquals(new byte[] {42, 43, 44}, sent.toByteArray());
    }
  }

  private void openSocket(Integer... capacities) throws Exception {
    openSocketAtPort(0, capacities);
  }

  private void openSocketAtPort(int port, Integer... capacities) throws Exception {
    SSLContext context = mock(SSLContext.class);
    engine = mock(SSLEngine.class);
    SSLSession session = mock(SSLSession.class);
    when(context.createSSLEngine("localhost", port)).thenReturn(engine);
    when(engine.getSSLParameters()).thenReturn(new SSLParameters());
    when(engine.getSession()).thenReturn(session);
    when(session.getApplicationBufferSize()).thenReturn(8);
    when(session.getPacketBufferSize()).thenReturn(32);
    // A small record encoder makes partial-write boundaries deterministic.
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0);
              ByteBuffer output = call.getArgument(1);
              int size = Math.min(3, input.remaining());
              if (size == 0)
                return new SSLEngineResult(
                    SSLEngineResult.Status.OK,
                    SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                    0,
                    0);
              output.put((byte) size);
              for (int i = 0; i < size; i++) output.put(input.get());
              return new SSLEngineResult(
                  SSLEngineResult.Status.OK,
                  SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                  size,
                  size + 1);
            });
    Queue<Integer> writes = new ArrayDeque<>(Arrays.asList(capacities));
    SocketChannel channel = mock(SocketChannel.class);
    when(channel.write(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0);
              int size = Math.min(input.remaining(), writes.isEmpty() ? 32 : writes.remove());
              for (int i = 0; i < size; i++) sent.write(input.get());
              return size;
            });
    socket =
        new TNonblockingSSLSocket("localhost", port, 0, context) {
          @Override
          public SocketChannel getSocketChannel() {
            return channel;
          }
        };
  }
}
