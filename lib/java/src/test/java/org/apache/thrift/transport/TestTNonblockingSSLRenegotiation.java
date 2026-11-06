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
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

public class TestTNonblockingSSLRenegotiation {
  private TNonblockingSSLSocket socket;
  private Selector selector;
  private SSLEngine engine;
  private SocketChannel channel;
  private final AtomicReference<SSLEngineResult.HandshakeStatus> status =
      new AtomicReference<>(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
  private final AtomicBoolean taskRan = new AtomicBoolean();
  private final ByteArrayOutputStream sent = new ByteArrayOutputStream();

  @AfterEach
  public void close() throws Exception {
    if (socket != null) socket.close();
    if (selector != null) selector.close();
  }

  @Test
  public void testReadProcessesTasksAndHandshakeOutput() throws Exception {
    openSocket(new byte[] {1, 2});
    when(engine.getDelegatedTask())
        .thenReturn(
            () -> {
              taskRan.set(true);
              status.set(SSLEngineResult.HandshakeStatus.NEED_WRAP);
            })
        .thenReturn(null);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (!((ByteBuffer) call.getArgument(0)).hasRemaining()) {
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              }
              if (status.get() == SSLEngineResult.HandshakeStatus.NEED_TASK)
                throw new SSLException("Delegated task must run before unwrapping again");
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              byte record = input.get();
              if (record == 1) {
                status.set(SSLEngineResult.HandshakeStatus.NEED_TASK);
                return result(1, 0);
              }
              assertEquals(SSLEngineResult.HandshakeStatus.NEED_UNWRAP, status.get());
              output.put(new byte[] {1, 2, 3});
              status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
              return result(1, 3);
            });
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              assertFalse(((ByteBuffer) call.getArgument(0)).hasRemaining());
              ((ByteBuffer) call.getArgument(1)).put((byte) 42);
              status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
              return result(0, 1);
            });
    ByteBuffer response = ByteBuffer.allocate(3);
    assertEquals(3, readFully(response));
    assertArrayEquals(new byte[] {1, 2, 3}, response.array());
    assertTrue(taskRan.get());
    assertArrayEquals(new byte[] {42}, sent.toByteArray());
  }

  @Test
  public void testWriteProcessesHandshakeInputBeforeApplicationData() throws Exception {
    openSocket(new byte[] {1});
    status.set(SSLEngineResult.HandshakeStatus.NEED_TASK);
    when(engine.getDelegatedTask())
        .thenReturn(
            () -> {
              taskRan.set(true);
              status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
            })
        .thenReturn(null);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (!((ByteBuffer) call.getArgument(0)).hasRemaining()) {
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              }
              ((ByteBuffer) call.getArgument(0)).get();
              status.set(SSLEngineResult.HandshakeStatus.NEED_WRAP);
              return result(1, 0);
            });
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (status.get() == SSLEngineResult.HandshakeStatus.NEED_TASK)
                throw new SSLException("Delegated task must run before wrapping");
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              if (status.get() == SSLEngineResult.HandshakeStatus.NEED_WRAP) {
                assertFalse(input.hasRemaining());
                output.put((byte) 42);
                status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
                return result(0, 1);
              }
              int count = input.remaining();
              output.put(input);
              return result(count, count);
            });
    assertEquals(3, writeFully(ByteBuffer.wrap(new byte[] {1, 2, 3})));
    assertEquals(SelectionKey.OP_WRITE, selector.keys().iterator().next().interestOps());
    assertTrue(taskRan.get());
    assertArrayEquals(new byte[] {42, 1, 2, 3}, sent.toByteArray());
  }

  @Test
  public void testHandshakeInputDuringWriteRemainsReadable() throws Exception {
    openSocket(new byte[] {1});
    status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (!((ByteBuffer) call.getArgument(0)).hasRemaining()) {
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              }
              ((ByteBuffer) call.getArgument(0)).get();
              ((ByteBuffer) call.getArgument(1)).put(new byte[] {4, 5});
              status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
              return result(1, 2);
            });
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (status.get() != SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING)
                throw new SSLException("Handshake input must be processed first");
              ByteBuffer input = call.getArgument(0);
              int count = input.remaining();
              ((ByteBuffer) call.getArgument(1)).put(input);
              return result(count, count);
            });
    assertEquals(1, socket.write(ByteBuffer.wrap(new byte[] {7})));
    ByteBuffer response = ByteBuffer.allocate(2);
    assertEquals(2, socket.read(response));
    assertArrayEquals(new byte[] {4, 5}, response.array());
  }

  @Test
  public void testReadPreservesFullApplicationBufferBetweenHandshakeRecords() throws Exception {
    openSocket(new byte[] {1, 2});
    status.set(SSLEngineResult.HandshakeStatus.NEED_TASK);
    when(engine.getDelegatedTask())
        .thenReturn(() -> status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP))
        .thenReturn(null);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (!((ByteBuffer) call.getArgument(0)).hasRemaining()) {
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              }
              if (status.get() == SSLEngineResult.HandshakeStatus.NEED_TASK)
                throw new SSLException("Delegated task must run first");
              byte record = ((ByteBuffer) call.getArgument(0)).get();
              ByteBuffer output = call.getArgument(1);
              if (record == 1) {
                for (int i = 1; i <= 16; i++) output.put((byte) i);
                return result(1, 16);
              }
              output.put(new byte[] {17, 18});
              status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
              return result(1, 2);
            });
    ByteBuffer response = ByteBuffer.allocate(18);
    assertEquals(18, readFully(response));
    byte[] expected = new byte[18];
    for (int i = 0; i < expected.length; i++) expected[i] = (byte) (i + 1);
    assertArrayEquals(expected, response.array());
  }

  @Test
  public void testWritePreservesApplicationDataUntilLaterRead() throws Exception {
    openSocket(new byte[] {1, 2});
    status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (!((ByteBuffer) call.getArgument(0)).hasRemaining()) {
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              }
              byte record = ((ByteBuffer) call.getArgument(0)).get();
              if (record == 1) {
                ((ByteBuffer) call.getArgument(1)).put((byte) 9);
                return result(1, 1);
              }
              status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
              return result(1, 0);
            });
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (status.get() != SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING)
                throw new SSLException("Handshake input must be processed first");
              ByteBuffer input = call.getArgument(0);
              int count = input.remaining();
              ((ByteBuffer) call.getArgument(1)).put(input);
              return result(count, count);
            });
    ByteBuffer request = ByteBuffer.wrap(new byte[] {7});
    assertEquals(1, writeFully(request));
    ByteBuffer response = ByteBuffer.allocate(1);
    assertEquals(1, socket.read(response));
    assertEquals(9, response.get(0));
    assertArrayEquals(new byte[] {7}, sent.toByteArray());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  public void testRenegotiationEofIsReported(boolean reading) throws Exception {
    openSocket(new byte[0]);
    status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenReturn(
            new SSLEngineResult(SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0));
    when(channel.read(any(ByteBuffer.class))).thenReturn(-1);
    assertThrows(TTransportException.class, () -> applicationIo(reading));
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  public void testRenegotiationClosedUnwrapIsReported(boolean reading) throws Exception {
    openSocket(new byte[0]);
    status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenReturn(new SSLEngineResult(SSLEngineResult.Status.CLOSED, status.get(), 0, 0));
    assertThrows(TTransportException.class, () -> applicationIo(reading));
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  public void testRenegotiationClosedWrapIsReported(boolean reading) throws Exception {
    openSocket(new byte[0]);
    status.set(SSLEngineResult.HandshakeStatus.NEED_WRAP);
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenReturn(new SSLEngineResult(SSLEngineResult.Status.CLOSED, status.get(), 0, 0));
    assertThrows(TTransportException.class, () -> applicationIo(reading));
  }

  @Test
  public void testInterleavedPlaintextRespectsMessageSizeLimit() throws Exception {
    openSocket(new byte[] {1, 2, 3});
    socket.setMaxMessageSize(16);
    status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              if (!input.hasRemaining())
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              input.get();
              output.put(new byte[16]);
              return result(1, 16);
            });
    TTransportException error =
        assertThrows(
            TTransportException.class, () -> socket.write(ByteBuffer.wrap(new byte[] {7})));
    assertEquals(TTransportException.MESSAGE_SIZE_LIMIT, error.getType());
  }

  @Test
  public void testCloseDropsBufferedApplicationData() throws Exception {
    openSocket(new byte[] {1});
    status.set(SSLEngineResult.HandshakeStatus.NEED_UNWRAP);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              if (!input.hasRemaining())
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              input.get();
              output.put((byte) 9);
              return result(1, 1);
            });
    assertEquals(0, socket.write(ByteBuffer.wrap(new byte[] {7})));
    assertTrue(socket.hasPendingReads());
    socket.close();
    assertFalse(socket.hasPendingReads());
  }

  @ParameterizedTest
  @CsvSource({"true,false", "false,false", "true,true"})
  public void testTaskFailureIsReportedAfterStatusChanges(boolean reading, boolean buffered)
      throws Exception {
    openSocket(new byte[] {1});
    status.set(
        buffered
            ? SSLEngineResult.HandshakeStatus.NEED_UNWRAP
            : SSLEngineResult.HandshakeStatus.NEED_TASK);
    IllegalArgumentException failure =
        new IllegalArgumentException("task failed after changing status");
    when(engine.getDelegatedTask())
        .thenReturn(
            () -> {
              status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
              throw failure;
            })
        .thenReturn(null);
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              if (!buffered || !input.hasRemaining())
                return new SSLEngineResult(
                    SSLEngineResult.Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              input.get();
              output.put((byte) 9);
              status.set(SSLEngineResult.HandshakeStatus.NEED_TASK);
              return result(1, 1);
            });
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              int size = input.remaining();
              output.put(input);
              return result(size, size);
            });
    assertEquals(0, applicationIo(buffered ? false : reading));
    awaitTaskWakeup();
    TTransportException error =
        assertThrows(TTransportException.class, () -> applicationIo(reading));
    assertSame(failure, error.getCause().getCause());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  public void testApplicationIoWaitsWithoutQueryingActiveEngine(boolean reading) throws Exception {
    openSocket(new byte[0]);
    status.set(SSLEngineResult.HandshakeStatus.NEED_TASK);
    CountDownLatch started = new CountDownLatch(1), release = new CountDownLatch(1);
    AtomicBoolean active = new AtomicBoolean();
    when(engine.getHandshakeStatus())
        .thenAnswer(
            call -> {
              assertFalse(active.get(), "Queried the engine while its delegated task was running");
              return status.get();
            });
    when(engine.getDelegatedTask())
        .thenReturn(
            () -> {
              active.set(true);
              started.countDown();
              try {
                assertTrue(release.await(5, TimeUnit.SECONDS));
                status.set(SSLEngineResult.HandshakeStatus.NOT_HANDSHAKING);
              } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
              } finally {
                active.set(false);
              }
            })
        .thenReturn(null);
    try {
      assertEquals(0, applicationIo(reading));
      assertTrue(started.await(2, TimeUnit.SECONDS));
      assertEquals(0, applicationIo(reading));
    } finally {
      release.countDown();
    }
  }

  private void awaitTaskWakeup() throws Exception {
    SelectionKey key = selector.keys().iterator().next();
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
    while (key.interestOps() == 0 && System.nanoTime() < deadline) {
      selector.select(20);
    }
    assertEquals(SelectionKey.OP_WRITE, key.interestOps());
  }

  private int applicationIo(boolean reading) throws Exception {
    ByteBuffer data = ByteBuffer.allocate(1);
    return reading ? socket.read(data) : socket.write(data);
  }

  private int readFully(ByteBuffer buffer) throws Exception {
    return transferFully(buffer, true);
  }

  private int writeFully(ByteBuffer buffer) throws Exception {
    return transferFully(buffer, false);
  }

  private int transferFully(ByteBuffer buffer, boolean read) throws Exception {
    int total = 0;
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
    while (buffer.hasRemaining()) {
      assertTrue(System.nanoTime() < deadline, "TLS operation did not finish");
      int position = buffer.position();
      int count = read ? socket.read(buffer) : socket.write(buffer);
      assertEquals(buffer.position() - position, count);
      total += count;
      if (count == 0) {
        selector.select(20);
        selector.selectedKeys().clear();
      }
    }
    return total;
  }

  private SSLEngineResult result(int consumed, int produced) {
    return new SSLEngineResult(SSLEngineResult.Status.OK, status.get(), consumed, produced);
  }

  private void openSocket(byte[] records) throws Exception {
    SSLContext context = mock(SSLContext.class);
    engine = mock(SSLEngine.class);
    SSLSession session = mock(SSLSession.class);
    when(context.createSSLEngine("localhost", 0)).thenReturn(engine);
    when(engine.getSSLParameters()).thenReturn(new SSLParameters());
    when(engine.getSession()).thenReturn(session);
    when(engine.getHandshakeStatus()).thenAnswer(call -> status.get());
    when(session.getApplicationBufferSize()).thenReturn(16);
    when(session.getPacketBufferSize()).thenReturn(64);
    channel = mock(SocketChannel.class);
    when(channel.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ((ByteBuffer) call.getArgument(0)).put(records);
              return records.length;
            })
        .thenReturn(0);
    when(channel.write(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer data = call.getArgument(0);
              int count = data.remaining();
              while (data.hasRemaining()) sent.write(data.get());
              return count;
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
