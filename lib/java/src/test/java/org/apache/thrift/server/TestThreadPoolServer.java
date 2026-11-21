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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TServerTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.api.Test;
import thrift.test.ThriftTest;

public class TestThreadPoolServer {

  /** Test server is shut down properly even with some open clients. */
  @Test
  public void testStopServerWithOpenClient() throws Exception {
    AtomicReference<TSocket> ref = new AtomicReference<>();
    TServerSocket serverSocket =
        new TServerSocket(
            new TServerSocket.ServerSocketTransportArgs()
                .port(0)
                .clientTimeout(3000)
                .maxMessageSize(51200)) {
          @Override
          public TSocket accept() throws TTransportException {
            TSocket socket = super.accept();
            ref.set(socket);
            return socket;
          }
        };
    TThreadPoolServer server = buildServer(serverSocket);
    Thread serverThread = new Thread(server::serve);
    serverThread.start();
    try (TSocket client = new TSocket("localhost", serverSocket.getServerSocket().getLocalPort())) {
      client.open();
      Thread.sleep(1000);
      // There is a thread listening to the client
      assertEquals(1, ((ThreadPoolExecutor) server.getExecutorService()).getActiveCount());
      assertEquals(51200, ref.get().getConfiguration().getMaxMessageSize());

      // Trigger the server to stop, but it does not wait
      server.stop();
      assertTrue(server.waitForShutdown());

      // After server is stopped, the executor thread pool should be shut down
      assertTrue(
          server.getExecutorService().isTerminated(), "Server thread pool should be terminated");

      // TODO: The socket is actually closed (timeout) but the client code
      // ignores the timeout Exception and maintains the socket open state
      assertTrue(client.isOpen(), "Client should be closed after server shutdown");
    }
  }

  /** Delegates to a pool, except that its first execution fails with the given Error. */
  private static class FirstExecutionFails extends AbstractExecutorService {
    private final ExecutorService pool;
    private final Error failure;
    private final AtomicBoolean failed = new AtomicBoolean();

    FirstExecutionFails(ExecutorService pool, Error failure) {
      this.pool = pool;
      this.failure = failure;
    }

    @Override
    public void execute(Runnable command) {
      if (failed.compareAndSet(false, true)) {
        throw failure;
      }
      pool.execute(command);
    }

    @Override
    public void shutdown() {
      pool.shutdown();
    }

    @Override
    public List<Runnable> shutdownNow() {
      return pool.shutdownNow();
    }

    @Override
    public boolean isShutdown() {
      return pool.isShutdown();
    }

    @Override
    public boolean isTerminated() {
      return pool.isTerminated();
    }

    @Override
    public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException {
      return pool.awaitTermination(timeout, unit);
    }
  }

  private TThreadPoolServer buildServer(TServerTransport serverSocket, ExecutorService executor) {
    return new TThreadPoolServer(
        new TThreadPoolServer.Args(serverSocket)
            .protocolFactory(new TBinaryProtocol.Factory())
            .processor(new ThriftTest.Processor<>(new ServerTestBase.TestHandler()))
            .executorService(executor));
  }

  /**
   * A worker thread that cannot be started, as when the process has no threads left, costs that one
   * connection: the server closes it and serves the next client.
   */
  @Test
  public void testWorkerThatCannotStartCostsOnlyItsConnection() throws Exception {
    TServerSocket serverSocket =
        new TServerSocket(
            new TServerSocket.ServerSocketTransportArgs().port(0).clientTimeout(5000));
    ExecutorService pool = Executors.newCachedThreadPool();
    TThreadPoolServer server =
        buildServer(
            serverSocket,
            new FirstExecutionFails(
                pool, new OutOfMemoryError("unable to create native thread (simulated)")));
    Thread serverThread = new Thread(server::serve);
    serverThread.start();
    try {
      int port = serverSocket.getServerSocket().getLocalPort();
      try (TSocket first = new TSocket("localhost", port, 5000)) {
        first.open();
        ThriftTest.Client client = new ThriftTest.Client(new TBinaryProtocol(first));
        assertThrows(TException.class, () -> client.testI32(1));
      }
      try (TSocket second = new TSocket("localhost", port, 5000)) {
        second.open();
        ThriftTest.Client client = new ThriftTest.Client(new TBinaryProtocol(second));
        assertEquals(42, client.testI32(42));
      }
      assertTrue(serverThread.isAlive(), "the server stopped");
    } finally {
      server.stop();
      serverThread.join(10000);
      pool.shutdownNow();
    }
  }

  /** An Error that ends serve() still shuts the worker pool down and ends serving. */
  @Test
  public void testServeCleansUpWhenAnErrorEndsIt() throws Exception {
    TServerSocket serverSocket =
        new TServerSocket(new TServerSocket.ServerSocketTransportArgs().port(0));
    ExecutorService pool = Executors.newCachedThreadPool();
    TThreadPoolServer server =
        buildServer(serverSocket, new FirstExecutionFails(pool, new InternalError("simulated")));
    AtomicReference<Throwable> ended = new AtomicReference<>();
    Thread serverThread =
        new Thread(
            () -> {
              try {
                server.serve();
              } catch (Throwable t) {
                ended.set(t);
              }
            });
    serverThread.start();
    try (TSocket client =
        new TSocket("localhost", serverSocket.getServerSocket().getLocalPort(), 5000)) {
      client.open();
      serverThread.join(10000);
    } finally {
      serverSocket.close();
    }
    try {
      assertFalse(serverThread.isAlive(), "serve() did not end");
      assertInstanceOf(InternalError.class, ended.get());
      assertFalse(server.isServing(), "the server still says it is serving");
      assertTrue(pool.isShutdown(), "the worker pool was not shut down");
    } finally {
      pool.shutdownNow();
    }
  }

  private TThreadPoolServer buildServer(TServerTransport serverSocket) {
    TThreadPoolServer.Args args =
        new TThreadPoolServer.Args(serverSocket)
            .protocolFactory(new TBinaryProtocol.Factory())
            .processor(new ThriftTest.Processor<>(new ServerTestBase.TestHandler()));
    return new TThreadPoolServer(args);
  }
}
