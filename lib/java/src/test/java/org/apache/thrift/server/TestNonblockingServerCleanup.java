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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TTransport;
import org.junit.jupiter.api.Test;

/**
 * When the select thread of a nonblocking server ends with an error, the connections it still holds
 * are closed and their contexts deleted, as when the server is stopped.
 */
public class TestNonblockingServerCleanup {

  private static final int TIMEOUT_MS = 5000;

  /** The connections that get a context; the next one fails in createContext. */
  private static final int CONNECTIONS = 2;

  /** Counts the contexts, fails the creation of one more, and optionally the first deletion. */
  private static class FailingContexts implements TServerEventHandler {
    final boolean failFirstDeletion;
    final AtomicInteger created = new AtomicInteger();
    final AtomicInteger deleted = new AtomicInteger();

    FailingContexts(boolean failFirstDeletion) {
      this.failFirstDeletion = failFirstDeletion;
    }

    @Override
    public void preServe() {}

    @Override
    public ServerContext createContext(TProtocol input, TProtocol output) {
      if (created.incrementAndGet() > CONNECTIONS) {
        throw new IllegalStateException("createContext failed (test)");
      }
      return new ServerContext() {
        @Override
        public <T> T unwrap(Class<T> iface) {
          return null;
        }

        @Override
        public boolean isWrapperFor(Class<?> iface) {
          return false;
        }
      };
    }

    @Override
    public void deleteContext(ServerContext serverContext, TProtocol input, TProtocol output) {
      if (deleted.incrementAndGet() == 1 && failFirstDeletion) {
        throw new Error("deleteContext failed (test)");
      }
    }

    @Override
    public void processContext(
        ServerContext serverContext, TTransport inputTransport, TTransport outputTransport) {}
  }

  private static boolean waitFor(BooleanSupplier condition) throws InterruptedException {
    long deadline = System.currentTimeMillis() + TIMEOUT_MS;
    while (!condition.getAsBoolean()) {
      if (System.currentTimeMillis() > deadline) {
        return false;
      }
      Thread.sleep(10);
    }
    return true;
  }

  /** Returns -1 if the server has closed the connection, -2 if it is still open. */
  private static int readFromServer(Socket socket) throws IOException {
    socket.setSoTimeout(TIMEOUT_MS);
    try {
      return socket.getInputStream().read();
    } catch (SocketTimeoutException e) {
      return -2;
    }
  }

  private static final TProcessor PROCESSOR = (in, out) -> {};

  private static void assertConnectionsClosedOnError(
      Function<TNonblockingServerSocket, TServer> newServer, boolean failFirstDeletion)
      throws Exception {
    TNonblockingServerSocket serverSocket = new TNonblockingServerSocket(0);
    TServer server = newServer.apply(serverSocket);
    FailingContexts handler = new FailingContexts(failFirstDeletion);
    server.setServerEventHandler(handler);
    Thread serving = new Thread(server::serve, "serve");
    serving.setDaemon(true);
    serving.start();
    List<Socket> clients = new ArrayList<>();
    try {
      assertTrue(waitFor(server::isServing), "the server starts");
      int port = serverSocket.getPort();
      for (int i = 1; i <= CONNECTIONS; i++) {
        clients.add(new Socket("localhost", port));
        int registered = i;
        assertTrue(
            waitFor(() -> handler.created.get() == registered),
            "connection " + i + " is registered");
      }
      // The context of this one cannot be created, which ends the select thread.
      clients.add(new Socket("localhost", port));

      serving.join(TIMEOUT_MS);
      assertFalse(serving.isAlive(), "serve() returns once the select thread has ended");
      for (int i = 0; i < clients.size(); i++) {
        assertEquals(
            -1,
            readFromServer(clients.get(i)),
            "the server closed connection " + (i + 1) + " (-2: still open)");
      }
      assertEquals(
          CONNECTIONS, handler.deleted.get(), "the context of every connection was deleted");
    } finally {
      server.stop();
      for (Socket client : clients) {
        client.close();
      }
    }
  }

  private static TServer nonblockingServer(TNonblockingServerSocket socket) {
    return new TNonblockingServer(new TNonblockingServer.Args(socket).processor(PROCESSOR));
  }

  private static TServer threadedSelectorServer(TNonblockingServerSocket socket) {
    return new TThreadedSelectorServer(
        new TThreadedSelectorServer.Args(socket).processor(PROCESSOR).selectorThreads(1));
  }

  @Test
  public void testNonblockingServerClosesConnectionsWhenTheSelectThreadFails() throws Exception {
    assertConnectionsClosedOnError(TestNonblockingServerCleanup::nonblockingServer, false);
  }

  @Test
  public void testThreadedSelectorServerClosesConnectionsWhenASelectorThreadFails()
      throws Exception {
    assertConnectionsClosedOnError(TestNonblockingServerCleanup::threadedSelectorServer, false);
  }

  @Test
  public void testNonblockingServerClosesTheOtherConnectionsWhenOneDeleteContextFails()
      throws Exception {
    assertConnectionsClosedOnError(TestNonblockingServerCleanup::nonblockingServer, true);
  }

  @Test
  public void testThreadedSelectorServerClosesTheOtherConnectionsWhenOneDeleteContextFails()
      throws Exception {
    assertConnectionsClosedOnError(TestNonblockingServerCleanup::threadedSelectorServer, true);
  }
}
