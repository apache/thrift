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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Queue;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

public class TestTNonblockingSSLSocket {
  private TNonblockingSSLSocket socket;
  private Selector selector;

  @AfterEach
  public void tearDown() throws Exception {
    if (socket != null) {
      socket.close();
    }
    if (selector != null) {
      selector.close();
    }
  }

  @Test
  public void testReadFullApplicationBufferInParts() throws Exception {
    openSocket(record("ABCDEFGH"), record("ijkl"));

    assertRead("ABC");
    assertRead("DEFGH");
    assertRead("ijkl");
  }

  @Test
  public void testReadBufferedRecordsBeforeEndOfStream() throws Exception {
    byte[] first = record("abc");
    byte[] second = record("defg");
    ByteBuffer records = ByteBuffer.allocate(first.length + second.length);
    records.put(first).put(second);
    openSocket(records.array());

    assertRead("ab");
    assertRead("cdefg");
    assertThrows(TTransportException.class, () -> socket.read(ByteBuffer.allocate(1)));
  }

  @Test
  public void testReadRecordSplitAcrossSocketReads() throws Exception {
    byte[] record = record("abcdef");
    openSocket(Arrays.copyOfRange(record, 0, 3), Arrays.copyOfRange(record, 3, record.length));

    assertRead("abcdef");
  }

  @Test
  public void testReadAfterRecordWithoutApplicationData() throws Exception {
    openSocket(record(""), record("abc"));

    assertRead("abc");
  }

  @Test
  public void testEndOfStreamWithIncompleteRecord() throws Exception {
    openSocket(new byte[] {3, 'a'});

    assertThrows(TTransportException.class, () -> socket.read(ByteBuffer.allocate(3)));
  }

  private void assertRead(String expected) throws Exception {
    byte[] bytes = expected.getBytes(StandardCharsets.US_ASCII);
    ByteBuffer buffer = ByteBuffer.allocate(bytes.length);
    assertEquals(bytes.length, socket.read(buffer));
    assertEquals(bytes.length, buffer.position());
    assertArrayEquals(bytes, buffer.array());
  }

  private static byte[] record(String payload) {
    byte[] bytes = payload.getBytes(StandardCharsets.US_ASCII);
    ByteBuffer record = ByteBuffer.allocate(1 + bytes.length);
    record.put((byte) bytes.length).put(bytes);
    return record.array();
  }

  private void openSocket(byte[]... chunks) throws Exception {
    SSLContext context = mock(SSLContext.class);
    SSLEngine engine = mock(SSLEngine.class);
    SSLSession session = mock(SSLSession.class);
    when(context.createSSLEngine("localhost", 0)).thenReturn(engine);
    when(engine.getSSLParameters()).thenReturn(new SSLParameters());
    when(engine.getSession()).thenReturn(session);
    when(session.getApplicationBufferSize()).thenReturn(8);
    when(session.getPacketBufferSize()).thenReturn(32);
    // A length-prefixed record decoder makes buffer boundaries independent of the TLS provider.
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            invocation -> {
              ByteBuffer input = invocation.getArgument(0);
              ByteBuffer output = invocation.getArgument(1);
              if (!input.hasRemaining() || input.remaining() < 1 + input.get(input.position())) {
                return new SSLEngineResult(
                    Status.BUFFER_UNDERFLOW, HandshakeStatus.NOT_HANDSHAKING, 0, 0);
              }
              int size = input.get();
              for (int i = 0; i < size; i++) {
                output.put(input.get());
              }
              return new SSLEngineResult(
                  Status.OK, HandshakeStatus.NOT_HANDSHAKING, 1 + size, size);
            });
    SocketChannel channel = mock(SocketChannel.class);
    Queue<byte[]> reads = new ArrayDeque<>(Arrays.asList(chunks));
    when(channel.read(any(ByteBuffer.class)))
        .thenAnswer(
            invocation -> {
              if (reads.isEmpty()) {
                return -1;
              }
              byte[] chunk = reads.remove();
              ((ByteBuffer) invocation.getArgument(0)).put(chunk);
              return chunk.length;
            });
    socket =
        new TNonblockingSSLSocket("localhost", 0, 0, context) {
          @Override
          public SocketChannel getSocketChannel() {
            return channel;
          }
        };
    selector = Selector.open();
    socket.registerSelector(selector, SelectionKey.OP_READ);
  }
}
