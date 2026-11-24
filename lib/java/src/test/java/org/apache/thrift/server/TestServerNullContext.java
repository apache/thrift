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

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;
import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TSaslClientTransport;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TestTSaslTransports;
import org.apache.thrift.transport.TestTSaslTransports.TestSaslCallbackHandler;
import org.apache.thrift.transport.layered.TFramedTransport;
import org.junit.jupiter.api.Test;
import thrift.test.ThriftTest;

/**
 * A TServerEventHandler may keep no context for a connection and return null from createContext.
 * The servers then serve the connection as usual and hand the null context to processContext and
 * deleteContext.
 */
public class TestServerNullContext {

  private static final String HOST = "localhost";
  private static final int TIMEOUT_MS = 5000;
  private static final int CONNECTIONS = 2;

  /** Keeps no context for its connections, and counts the calls it gets. */
  private static class NoContexts implements TServerEventHandler {
    final AtomicInteger created = new AtomicInteger();
    final AtomicInteger processed = new AtomicInteger();
    final AtomicInteger deleted = new AtomicInteger();

    @Override
    public void preServe() {}

    @Override
    public ServerContext createContext(TProtocol input, TProtocol output) {
      created.incrementAndGet();
      return null;
    }

    @Override
    public void deleteContext(ServerContext serverContext, TProtocol input, TProtocol output) {
      deleted.incrementAndGet();
    }

    @Override
    public void processContext(
        ServerContext serverContext, TTransport inputTransport, TTransport outputTransport) {
      processed.incrementAndGet();
    }
  }

  /** Opens a client transport to the server on the given port. */
  private interface Connector {
    TTransport connect(int port) throws Exception;
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

  private static TProcessor processor() {
    return new ThriftTest.Processor<>(new ServerTestBase.TestHandler());
  }

  private static TTransport plain(int port) throws Exception {
    TSocket socket = new TSocket(HOST, port, TIMEOUT_MS);
    socket.open();
    return socket;
  }

  private static TTransport framed(int port) throws Exception {
    TTransport transport = new TFramedTransport(new TSocket(HOST, port, TIMEOUT_MS));
    transport.open();
    return transport;
  }

  private static TTransport sasl(int port) throws Exception {
    TTransport transport =
        new TSaslClientTransport(
            TestTSaslTransports.WRAPPED_MECHANISM,
            TestTSaslTransports.PRINCIPAL,
            TestTSaslTransports.SERVICE,
            TestTSaslTransports.HOST,
            TestTSaslTransports.WRAPPED_PROPS,
            new TestSaslCallbackHandler(TestTSaslTransports.PASSWORD),
            new TSocket(HOST, port, TIMEOUT_MS));
    transport.open();
    return transport;
  }

  /** Calls the server over CONNECTIONS connections, one after the other. */
  private static void assertServesWithoutContexts(TServer server, int port, Connector connector)
      throws Exception {
    NoContexts handler = new NoContexts();
    server.setServerEventHandler(handler);
    Thread serving = new Thread(server::serve, "serve");
    serving.setDaemon(true);
    serving.start();
    try {
      assertTrue(waitFor(server::isServing), "the server starts");
      for (int i = 1; i <= CONNECTIONS; i++) {
        String text = "connection " + i;
        try (TTransport transport = connector.connect(port)) {
          ThriftTest.Client client = new ThriftTest.Client(new TBinaryProtocol(transport));
          assertEquals(
              text,
              assertDoesNotThrow(() -> client.testString(text), "the server answers on " + text),
              "the answer on " + text);
        }
        int closed = i;
        assertTrue(
            waitFor(() -> handler.deleted.get() == closed),
            "deleteContext is called once " + text + " is closed");
      }
      assertEquals(CONNECTIONS, handler.created.get(), "createContext is called per connection");
      assertTrue(
          handler.processed.get() >= CONNECTIONS, "processContext is called for the requests");
      assertTrue(server.isServing(), "the server is still serving");
    } finally {
      server.stop();
      serving.join(TIMEOUT_MS);
    }
  }

  private static TNonblockingServerSocket nonblockingServerSocket() throws Exception {
    return new TNonblockingServerSocket(
        new TNonblockingServerSocket.NonblockingAbstractServerSocketArgs().port(0));
  }

  @Test
  public void testSimpleServer() throws Exception {
    TServerSocket serverSocket = new TServerSocket(0);
    int port = serverSocket.getServerSocket().getLocalPort();
    assertServesWithoutContexts(
        new TSimpleServer(new TServer.Args(serverSocket).processor(processor())),
        port,
        TestServerNullContext::plain);
  }

  @Test
  public void testThreadPoolServer() throws Exception {
    TServerSocket serverSocket = new TServerSocket(0);
    int port = serverSocket.getServerSocket().getLocalPort();
    assertServesWithoutContexts(
        new TThreadPoolServer(new TThreadPoolServer.Args(serverSocket).processor(processor())),
        port,
        TestServerNullContext::plain);
  }

  @Test
  public void testNonblockingServer() throws Exception {
    TNonblockingServerSocket serverSocket = nonblockingServerSocket();
    assertServesWithoutContexts(
        new TNonblockingServer(new TNonblockingServer.Args(serverSocket).processor(processor())),
        serverSocket.getPort(),
        TestServerNullContext::framed);
  }

  @Test
  public void testThreadedSelectorServer() throws Exception {
    TNonblockingServerSocket serverSocket = nonblockingServerSocket();
    assertServesWithoutContexts(
        new TThreadedSelectorServer(
            new TThreadedSelectorServer.Args(serverSocket).processor(processor())),
        serverSocket.getPort(),
        TestServerNullContext::framed);
  }

  @Test
  public void testSaslNonblockingServer() throws Exception {
    TNonblockingServerSocket serverSocket = nonblockingServerSocket();
    assertServesWithoutContexts(
        new TSaslNonblockingServer(
            new TSaslNonblockingServer.Args(serverSocket)
                .processor(processor())
                .addSaslMechanism(
                    TestTSaslTransports.WRAPPED_MECHANISM,
                    TestTSaslTransports.SERVICE,
                    TestTSaslTransports.HOST,
                    TestTSaslTransports.WRAPPED_PROPS,
                    new TestSaslCallbackHandler(TestTSaslTransports.PASSWORD))),
        serverSocket.getPort(),
        TestServerNullContext::sasl);
  }
}
