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

package org.apache.thrift.server;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TSaslClientTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.transport.TestTSaslTransports;
import org.apache.thrift.transport.TestTSaslTransports.TestSaslCallbackHandler;
import org.apache.thrift.transport.sasl.NegotiationStatus;
import org.apache.thrift.transport.sasl.ServerSaslPeer;
import org.apache.thrift.transport.sasl.TSaslServerFactory;
import org.junit.jupiter.api.Test;

/** A failure while the network thread handles one connection must not affect the others. */
public class TestSaslNonblockingServerConnectionErrors {

  private static final String HOST = "localhost";
  private static final int TIMEOUT_MS = 5000;

  // A negotiation frame with a status byte the server does not know: it answers with an error.
  private static final byte[] INVALID_STATUS_FRAME = {(byte) 0xFF, 0, 0, 0, 0};
  private static final String INVALID_STATUS_ANSWER = "PROTOCOL_ERROR: Invalid status -1";

  /** Fails every negotiation start, which runs on the network thread, with the given error. */
  private static class FailingSaslServerFactory extends TSaslServerFactory {
    private final Supplier<Throwable> failure;

    FailingSaslServerFactory(Supplier<Throwable> failure) {
      this.failure = failure;
    }

    @Override
    public ServerSaslPeer getSaslPeer(String mechanism) {
      Throwable e = failure.get();
      if (e instanceof RuntimeException) {
        throw (RuntimeException) e;
      }
      throw (Error) e;
    }
  }

  @Test
  public void testRuntimeExceptionClosesOnlyItsConnection() throws Exception {
    checkFailureClosesOnlyItsConnection(() -> new IllegalStateException("test failure"));
  }

  @Test
  public void testOutOfMemoryErrorClosesOnlyItsConnection() throws Exception {
    checkFailureClosesOnlyItsConnection(() -> new OutOfMemoryError("test failure"));
  }

  @Test
  public void testServerStopsWhenNetworkThreadEnds() throws Exception {
    TNonblockingServerSocket serverSocket = newServerSocket();
    int port = serverSocket.getPort();
    TSaslNonblockingServer server =
        startServer(serverSocket, () -> new InternalError("test failure"));
    try {
      try (Socket failing = connect(port)) {
        failing.getOutputStream().write(startFrame());
        assertEquals(0, readUntilClosed(failing).length);
      }
      long deadline = System.currentTimeMillis() + TIMEOUT_MS;
      while (server.isServing() && System.currentTimeMillis() < deadline) {
        Thread.sleep(10);
      }
      boolean serving = server.isServing();
      String newConnection = tryNewConnection(port);
      assertAll(
          () -> assertFalse(serving, "server still reports serving"),
          () -> assertEquals("closed", newConnection, "new connection after the thread ended"));
    } finally {
      server.shutdown();
    }
  }

  @Test
  public void testEventHandlerFailureClosesOnlyItsConnection() throws Exception {
    TNonblockingServerSocket serverSocket = newServerSocket();
    int port = serverSocket.getPort();
    TSaslNonblockingServer server =
        new TSaslNonblockingServer(
            new TSaslNonblockingServer.Args(serverSocket)
                .networkThreads(1)
                .processor(
                    (in, out) -> {
                      throw new TException("test failure");
                    })
                .addSaslMechanism(
                    TestTSaslTransports.WRAPPED_MECHANISM,
                    TestTSaslTransports.SERVICE,
                    TestTSaslTransports.HOST,
                    TestTSaslTransports.WRAPPED_PROPS,
                    new TestSaslCallbackHandler(TestTSaslTransports.PASSWORD)));
    AtomicInteger deletedContexts = new AtomicInteger();
    // The failed request closes its connection, and closing it runs deleteContext.
    server.setServerEventHandler(
        new TServerEventHandler() {
          @Override
          public void preServe() {}

          @Override
          public ServerContext createContext(TProtocol input, TProtocol output) {
            return new ServerContext() {
              @Override
              public <T> T unwrap(Class<T> iface) {
                throw new IllegalArgumentException("not a wrapper");
              }

              @Override
              public boolean isWrapperFor(Class<?> iface) {
                return false;
              }
            };
          }

          @Override
          public void deleteContext(ServerContext context, TProtocol input, TProtocol output) {
            deletedContexts.incrementAndGet();
            throw new IllegalStateException("test failure");
          }

          @Override
          public void processContext(ServerContext context, TTransport in, TTransport out) {}
        });
    server.serve();
    try {
      try (TSaslClientTransport client =
          new TSaslClientTransport(
              TestTSaslTransports.WRAPPED_MECHANISM,
              TestTSaslTransports.PRINCIPAL,
              TestTSaslTransports.SERVICE,
              TestTSaslTransports.HOST,
              TestTSaslTransports.WRAPPED_PROPS,
              new TestSaslCallbackHandler(TestTSaslTransports.PASSWORD),
              new TSocket(HOST, port, TIMEOUT_MS))) {
        client.open();
        client.write(new byte[] {1, 2, 3});
        client.flush();
        TTransportException closed =
            assertThrows(TTransportException.class, () -> client.readAll(new byte[1], 0, 1));
        assertEquals(TTransportException.END_OF_FILE, closed.getType());
      }
      assertAnswersInvalidStatus(port);
      assertEquals(1, deletedContexts.get());
      assertTrue(server.isServing());
    } finally {
      server.shutdown();
    }
  }

  private static void checkFailureClosesOnlyItsConnection(Supplier<Throwable> failure)
      throws Exception {
    TNonblockingServerSocket serverSocket = newServerSocket();
    int port = serverSocket.getPort();
    TSaslNonblockingServer server = startServer(serverSocket, failure);
    try {
      try (Socket failing = connect(port)) {
        failing.getOutputStream().write(startFrame());
        assertEquals(0, readUntilClosed(failing).length);
      }
      assertAnswersInvalidStatus(port);
      assertTrue(server.isServing());
    } finally {
      server.shutdown();
    }
  }

  /** A new connection sending an unknown status gets the error answer, then gets closed. */
  private static void assertAnswersInvalidStatus(int port) throws IOException {
    try (Socket socket = connect(port)) {
      socket.getOutputStream().write(INVALID_STATUS_FRAME);
      byte[] answer = readUntilClosed(socket);
      assertEquals(5 + INVALID_STATUS_ANSWER.length(), answer.length);
      assertEquals(NegotiationStatus.ERROR.getValue(), answer[0]);
      assertEquals(INVALID_STATUS_ANSWER.length(), ByteBuffer.wrap(answer, 1, 4).getInt());
      assertEquals(
          INVALID_STATUS_ANSWER, new String(answer, 5, answer.length - 5, StandardCharsets.UTF_8));
    }
  }

  private static TNonblockingServerSocket newServerSocket() throws Exception {
    return new TNonblockingServerSocket(
        new TNonblockingServerSocket.NonblockingAbstractServerSocketArgs().port(0));
  }

  private static TSaslNonblockingServer startServer(
      TNonblockingServerSocket serverSocket, Supplier<Throwable> failure) throws Exception {
    TSaslNonblockingServer server =
        new TSaslNonblockingServer(
            new TSaslNonblockingServer.Args(serverSocket)
                .networkThreads(1)
                .processor((in, out) -> {})
                .saslServerFactory(new FailingSaslServerFactory(failure)));
    server.serve();
    return server;
  }

  private static byte[] startFrame() {
    byte[] mechanism = "ANY".getBytes(StandardCharsets.UTF_8);
    return ByteBuffer.allocate(5 + mechanism.length)
        .put(NegotiationStatus.START.getValue())
        .putInt(mechanism.length)
        .put(mechanism)
        .array();
  }

  private static Socket connect(int port) throws IOException {
    Socket socket = new Socket();
    socket.connect(new InetSocketAddress(HOST, port), TIMEOUT_MS);
    socket.setSoTimeout(TIMEOUT_MS);
    return socket;
  }

  /** Reads everything the server sends until it closes the connection. */
  private static byte[] readUntilClosed(Socket socket) throws IOException {
    ByteArrayOutputStream received = new ByteArrayOutputStream();
    InputStream in = socket.getInputStream();
    byte[] buffer = new byte[256];
    try {
      int n;
      while ((n = in.read(buffer)) >= 0) {
        received.write(buffer, 0, n);
      }
    } catch (SocketTimeoutException e) {
      throw new AssertionError(
          "connection not closed within "
              + TIMEOUT_MS
              + " ms, received "
              + received.size()
              + " bytes",
          e);
    }
    return received.toByteArray();
  }

  /**
   * Opens a new connection and sends a frame the server would answer.
   *
   * @return "answered", "closed" (refused, reset or closed by the server) or "left open".
   */
  private static String tryNewConnection(int port) {
    try (Socket socket = connect(port)) {
      socket.getOutputStream().write(INVALID_STATUS_FRAME);
      return socket.getInputStream().read() < 0 ? "closed" : "answered";
    } catch (SocketTimeoutException e) {
      return "left open";
    } catch (IOException e) {
      return "closed";
    }
  }
}
