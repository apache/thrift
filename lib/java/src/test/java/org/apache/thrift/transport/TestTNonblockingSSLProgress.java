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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

public class TestTNonblockingSSLProgress {
  private TNonblockingSSLSocket socket;
  private SSLEngine engine;
  private SocketChannel channel;
  private SocketChannel tcpChannel;
  private Selector selector;
  private SelectionKey key;
  private final CountDownLatch releaseTask = new CountDownLatch(1);

  @AfterEach
  public void close() throws Exception {
    releaseTask.countDown();
    if (socket != null) socket.close();
    if (selector != null) selector.close();
  }

  @Test
  public void testEmptyReadDoesNotTouchSocket() throws Exception {
    openSocket(0);
    when(channel.read(any(ByteBuffer.class))).thenThrow(new IOException("Unexpected read"));
    assertEquals(0, socket.read(ByteBuffer.allocate(0)));
    verify(channel, never()).read(any(ByteBuffer.class));
  }

  @Test
  public void testHandshakeEofIsAnError() throws Exception {
    connectSocket();
    when(engine.getHandshakeStatus()).thenReturn(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(channel.read(any(ByteBuffer.class))).thenReturn(-1);
    assertThrows(IOException.class, () -> socket.finishConnect());
  }

  @Test
  public void testReadYieldsWhenSocketHasNoData() throws Exception {
    openSocket(0);
    when(channel.read(any(ByteBuffer.class)))
        .thenReturn(0)
        .thenThrow(new IOException("Read retried without readiness"));
    assertEquals(0, socket.read(ByteBuffer.allocate(8)));
    assertEquals(SelectionKey.OP_READ, key.interestOps());
  }

  @Test
  public void testReadReturnsPartialPlaintextAndResumes() throws Exception {
    openSocket(0);
    when(channel.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ((ByteBuffer) call.getArgument(0)).put(new byte[] {3, 1, 2, 3});
              return 4;
            })
        .thenReturn(0)
        .thenAnswer(
            call -> {
              ((ByteBuffer) call.getArgument(0)).put(new byte[] {5, 4, 5, 6, 7, 8});
              return 6;
            })
        .thenThrow(new IOException("Unexpected read"));
    ByteBuffer data = ByteBuffer.allocate(8);
    assertEquals(3, socket.read(data));
    assertEquals(SelectionKey.OP_READ, key.interestOps());
    assertEquals(5, socket.read(data));
    assertArrayEquals(new byte[] {1, 2, 3, 4, 5, 6, 7, 8}, data.array());
  }

  @Test
  public void testHandshakeYieldsWhenSocketHasNoData() throws Exception {
    connectSocket();
    when(engine.getHandshakeStatus()).thenReturn(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(channel.read(any(ByteBuffer.class)))
        .thenReturn(0)
        .thenThrow(new IOException("Handshake retried without readiness"));
    assertFalse(socket.finishConnect());
    assertEquals(SelectionKey.OP_READ, key.interestOps());
    assertFalse(socket.isOpen());
  }

  @Test
  public void testDelegatedTaskWakesSelectorWithoutPolling() throws Exception {
    connectSocket();
    AtomicReference<SSLEngineResult.HandshakeStatus> status =
        new AtomicReference<>(SSLEngineResult.HandshakeStatus.NEED_TASK);
    AtomicInteger checks = new AtomicInteger();
    when(engine.getHandshakeStatus())
        .thenAnswer(
            call -> {
              if (checks.incrementAndGet() > 20)
                throw new IOException("Busy polling delegated task");
              return status.get();
            });
    when(engine.getDelegatedTask())
        .thenReturn(
            () -> {
              try {
                if (!releaseTask.await(5, TimeUnit.SECONDS))
                  throw new AssertionError("Task not released");
                status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
              } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
              }
            })
        .thenReturn(null);
    when(channel.read(any(ByteBuffer.class)))
        .thenReturn(0)
        .thenThrow(new IOException("Handshake retried without readiness"));
    assertFalse(socket.finishConnect());
    assertEquals(0, key.interestOps());
    releaseTask.countDown();
    awaitTaskWakeup();
    assertFalse(socket.finishConnect());
    assertEquals(SelectionKey.OP_READ, key.interestOps());
    verify(engine, times(2)).getDelegatedTask();
  }

  @Test
  public void testDelegatedTaskFailureIsReported() throws Exception {
    connectSocket();
    AtomicInteger checks = new AtomicInteger();
    when(engine.getHandshakeStatus())
        .thenAnswer(
            call -> {
              if (checks.incrementAndGet() > 20)
                throw new IllegalStateException("Task failure was ignored");
              return SSLEngineResult.HandshakeStatus.NEED_TASK;
            });
    when(engine.getDelegatedTask())
        .thenReturn(
            () -> {
              throw new IllegalArgumentException("task failed");
            })
        .thenReturn(null);
    assertFalse(socket.finishConnect());
    awaitTaskWakeup();
    IOException error = assertThrows(IOException.class, () -> socket.finishConnect());
    assertTrue(error.getCause() instanceof IllegalArgumentException);
  }

  private void awaitTaskWakeup() throws Exception {
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
    while (key.interestOps() == 0 && System.nanoTime() < deadline) {
      selector.select(2000);
    }
    assertEquals(SelectionKey.OP_WRITE, key.interestOps());
  }

  private void connectSocket() throws Exception {
    try (ServerSocket listener = new ServerSocket(0)) {
      listener.setSoTimeout(5000);
      openSocket(listener.getLocalPort());
      tcpChannel.connect(new InetSocketAddress("localhost", listener.getLocalPort()));
      try (Socket peer = listener.accept()) {
        assertTrue(tcpChannel.finishConnect());
      }
    }
  }

  private void openSocket(int port) throws Exception {
    SSLContext context = mock(SSLContext.class);
    engine = mock(SSLEngine.class);
    SSLSession session = mock(SSLSession.class);
    when(context.createSSLEngine("localhost", port)).thenReturn(engine);
    when(engine.getSSLParameters()).thenReturn(new SSLParameters());
    when(engine.getSession()).thenReturn(session);
    when(session.getApplicationBufferSize()).thenReturn(64);
    when(session.getPacketBufferSize()).thenReturn(128);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              if (!input.hasRemaining())
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW,
                    SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                    0,
                    0);
              int size = input.get();
              for (int i = 0; i < size; i++) output.put(input.get());
              return new SSLEngineResult(
                  SSLEngineResult.Status.OK,
                  SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING,
                  size + 1,
                  size);
            });
    channel = mock(SocketChannel.class);
    socket =
        new TNonblockingSSLSocket("localhost", port, 0, context) {
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
