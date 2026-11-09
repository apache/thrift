/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.thrift.transport;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.SocketException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

public class TestTSocket {
  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  public void testCloseDoesNotFlushOrCloseSocketStreams(boolean flushBeforeClose) throws Exception {
    SocketStreams streams = new SocketStreams();
    TSocket transport = new TSocket(streams.socket);
    byte[] request = {1, 2, 3};
    transport.write(request);
    if (flushBeforeClose) {
      transport.flush();
    }
    clearInvocations(streams.input, streams.output);

    transport.close();

    assertArrayEquals(flushBeforeClose ? request : new byte[0], streams.sent.toByteArray());
    verifyNoInteractions(streams.input, streams.output);
    verify(streams.socket).close();
    assertTrue(streams.closed);
    assertClosedAndIdempotent(transport, streams);
  }

  @Test
  public void testCloseAfterExplicitFlushFailureStillClosesSocket() throws Exception {
    SocketStreams streams = new SocketStreams();
    TSocket transport = new TSocket(streams.socket);
    IOException failure = new IOException("Output flush failure");
    doThrow(failure).when(streams.output).flush();

    assertSame(failure, assertThrows(TTransportException.class, transport::flush).getCause());
    clearInvocations(streams.input, streams.output);
    transport.close();

    verifyNoInteractions(streams.input, streams.output);
    assertTrue(streams.closed);
    assertClosedAndIdempotent(transport, streams);
  }

  @Test
  public void testCheckedSocketCloseFailureClearsReferences() throws Exception {
    SocketStreams streams = new SocketStreams();
    TSocket transport = new TSocket(streams.socket);
    doThrow(new IOException("Socket close failure")).when(streams.socket).close();

    transport.close();

    verify(streams.socket).close();
    assertClosedAndIdempotent(transport, streams);
  }

  @Test
  public void testUncheckedSocketCloseFailureCanBeRetried() throws Exception {
    SocketStreams streams = new SocketStreams();
    TSocket transport = new TSocket(streams.socket);
    IllegalStateException failure = new IllegalStateException("Socket close failure");
    AtomicInteger attempts = new AtomicInteger();
    doAnswer(
            invocation -> {
              if (attempts.getAndIncrement() == 0) {
                throw failure;
              }
              streams.closed = true;
              return null;
            })
        .when(streams.socket)
        .close();

    assertSame(failure, assertThrows(IllegalStateException.class, transport::close));
    assertNull(transport.inputStream_);
    assertNull(transport.outputStream_);

    transport.close();

    verify(streams.socket, times(2)).close();
    assertTrue(streams.closed);
    assertClosedAndIdempotent(transport, streams);
  }

  @Test
  public void testCloseInterruptsBlockedWrite() throws Exception {
    SocketStreams streams = new SocketStreams();
    TSocket transport = new TSocket(streams.socket);
    CountDownLatch writeEntered = new CountDownLatch(1);
    CountDownLatch socketClosed = new CountDownLatch(1);
    CountDownLatch writerDone = new CountDownLatch(1);
    CountDownLatch closerDone = new CountDownLatch(1);
    AtomicReference<Throwable> writeFailure = new AtomicReference<>();
    AtomicReference<Throwable> closeFailure = new AtomicReference<>();
    doAnswer(
            invocation -> {
              // BufferedOutputStream holds its lock while invoking this underlying write.
              writeEntered.countDown();
              if (!socketClosed.await(5, TimeUnit.SECONDS)) {
                throw new IOException("Test cleanup did not close the socket");
              }
              throw new SocketException("Socket closed");
            })
        .when(streams.output)
        .write(any(byte[].class), anyInt(), anyInt());
    doAnswer(
            invocation -> {
              streams.closed = true;
              socketClosed.countDown();
              return null;
            })
        .when(streams.socket)
        .close();
    Thread writer =
        new Thread(
            () -> {
              try {
                transport.write(new byte[64 * 1024]);
              } catch (Throwable failure) {
                writeFailure.set(failure);
              } finally {
                writerDone.countDown();
              }
            },
            "blocked-socket-writer");
    Thread closer =
        new Thread(
            () -> {
              try {
                transport.close();
              } catch (Throwable failure) {
                closeFailure.set(failure);
              } finally {
                closerDone.countDown();
              }
            },
            "socket-closer");
    writer.setDaemon(true);
    closer.setDaemon(true);
    try {
      writer.start();
      assertTrue(writeEntered.await(2, TimeUnit.SECONDS), "Writer did not enter socket write");
      closer.start();

      assertTrue(closerDone.await(2, TimeUnit.SECONDS), "close() waited for the blocked writer");
      assertTrue(
          writerDone.await(2, TimeUnit.SECONDS), "Socket close did not interrupt the writer");
      assertNull(closeFailure.get());
      assertInstanceOf(TTransportException.class, writeFailure.get());
      assertInstanceOf(SocketException.class, writeFailure.get().getCause());
    } finally {
      // Release both workers even when testing an implementation with the regression.
      streams.socket.close();
      writer.join(1000);
      closer.join(1000);
    }
  }

  private static void assertClosedAndIdempotent(TSocket transport, SocketStreams streams) {
    assertFalse(transport.isOpen());
    assertNull(transport.inputStream_);
    assertNull(transport.outputStream_);

    clearInvocations(streams.socket, streams.input, streams.output);
    transport.close();
    verifyNoInteractions(streams.socket, streams.input, streams.output);
  }

  private static class SocketStreams {
    final Socket socket = mock(Socket.class);
    final InputStream input = mock(InputStream.class);
    final OutputStream output = mock(OutputStream.class);
    final ByteArrayOutputStream sent = new ByteArrayOutputStream();
    boolean closed;

    SocketStreams() throws IOException {
      when(socket.isConnected()).thenReturn(true);
      when(socket.getInputStream()).thenReturn(input);
      when(socket.getOutputStream()).thenReturn(output);
      doAnswer(
              invocation -> {
                closed = true;
                return null;
              })
          .when(socket)
          .close();
      doAnswer(
              invocation -> {
                socket.close();
                return null;
              })
          .when(input)
          .close();
      doAnswer(
              invocation -> {
                socket.close();
                return null;
              })
          .when(output)
          .close();
      doAnswer(
              invocation -> {
                checkOpen();
                sent.write(
                    invocation.getArgument(0),
                    invocation.getArgument(1),
                    invocation.getArgument(2));
                return null;
              })
          .when(output)
          .write(any(byte[].class), anyInt(), anyInt());
      doAnswer(
              invocation -> {
                checkOpen();
                return null;
              })
          .when(output)
          .flush();
    }

    private void checkOpen() throws SocketException {
      if (closed) {
        throw new SocketException("Socket is closed");
      }
    }
  }
}
