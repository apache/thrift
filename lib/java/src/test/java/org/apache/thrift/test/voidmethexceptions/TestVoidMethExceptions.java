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

package org.apache.thrift.test.voidmethexceptions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.thrift.TApplicationException;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.TProcessor;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.async.TAsyncClientManager;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.server.TNonblockingServer;
import org.apache.thrift.server.TServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.layered.TFramedTransport;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import thrift.test.voidmethexceptions.TAppService01;
import thrift.test.voidmethexceptions.TExampleException;

public class TestVoidMethExceptions {

  private static final Logger log = LoggerFactory.getLogger(TestVoidMethExceptions.class);

  private static Stream<TestParameters> provideParameters() throws Exception {
    return Stream.<TestParameters>builder()
        .add(new TestParameters(ServerImplementationType.SYNC_SERVER))
        .add(new TestParameters(ServerImplementationType.ASYNC_SERVER))
        .build();
  }

  public static class TestParameters {
    private static final int TIMEOUT_MILLIS = 5_000;
    private final TServer server;
    private final Thread serverThread;
    private final TNonblockingServerSocket serverTransport;
    private int serverPort;
    private final ServerImplementationType serverImplementationType;
    private final CompletableFuture<Void> futureServerStarted = new CompletableFuture<>();

    TestParameters(ServerImplementationType serverImplementationType) throws Exception {
      this.serverImplementationType = serverImplementationType;
      serverPort = -1;
      serverImplementationType.service.setCancelled(false);
      serverTransport = new TNonblockingServerSocket(0);
      TNonblockingServer.Args args = new TNonblockingServer.Args(serverTransport);
      args.processor(serverImplementationType.processor);
      server =
          new TNonblockingServer(args) {
            @Override
            protected void setServing(boolean serving) {
              super.setServing(serving);

              if (serving) {
                serverPort = serverTransport.getPort();
                futureServerStarted.complete(null);
              }
            }
          };
      serverThread = new Thread(server::serve, "thrift-server");
      serverThread.setDaemon(true);
    }

    public AutoCloseable start() throws Exception {
      serverThread.start();
      futureServerStarted.get(TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);
      return () -> {
        serverImplementationType.service.setCancelled(true);
        server.stop();
        serverThread.join(TIMEOUT_MILLIS);
      };
    }

    private void checkSyncClient(
        String desc,
        String msg,
        boolean throwException,
        String expectedResult,
        Class<? extends Exception> expectedExceptionClass,
        String expectedExceptionMsg,
        SyncCall<TAppService01.Iface, String, Boolean, String> call)
        throws Exception {
      if (log.isInfoEnabled()) {
        log.info(
            "start test checkSyncClient::"
                + desc
                + ", throwException: "
                + throwException
                + ", serverImplementationType: "
                + serverImplementationType);
      }
      assertNotEquals(-1, serverPort);
      try (TTransport clientTransport =
          new TFramedTransport(
              new TSocket(new TConfiguration(), "localhost", serverPort, TIMEOUT_MILLIS))) {
        clientTransport.open();
        TAppService01.Iface client = new TAppService01.Client(new TBinaryProtocol(clientTransport));
        if (throwException && expectedExceptionClass != null) {
          Exception ex =
              assertThrows(
                  expectedExceptionClass,
                  () -> {
                    call.apply(client, msg, throwException);
                  });
          assertEquals(expectedExceptionClass, ex.getClass());
          if (expectedExceptionMsg != null) {
            assertEquals(expectedExceptionMsg, ex.getMessage());
          }
        } else {
          // expected
          String result = call.apply(client, msg, throwException);
          assertEquals(expectedResult, result);
        }
      }
    }

    private <T> void checkAsyncClient(
        String desc,
        String msg,
        boolean throwException,
        T expectedResult,
        Class<? extends Exception> expectedExceptionClass,
        String expectedExceptionMsg,
        AsyncCall<TAppService01.AsyncClient, String, Boolean, AsyncMethodCallback<T>> call)
        throws Throwable {
      if (log.isInfoEnabled()) {
        log.info(
            "start test checkAsyncClient::"
                + desc
                + ", throwException: "
                + throwException
                + ", serverImplementationType: "
                + serverImplementationType);
      }
      assertNotEquals(serverPort, -1);
      try (TNonblockingSocket clientTransportAsync =
          new TNonblockingSocket("localhost", serverPort, TIMEOUT_MILLIS)) {
        TAsyncClientManager asyncClientManager = new TAsyncClientManager();
        try {
          TAppService01.AsyncClient asyncClient =
              new TAppService01.AsyncClient(
                  new TBinaryProtocol.Factory(), asyncClientManager, clientTransportAsync);
          asyncClient.setTimeout(TIMEOUT_MILLIS);

          CompletableFuture<T> futureResult = new CompletableFuture<>();

          call.apply(
              asyncClient,
              msg,
              throwException,
              new AsyncMethodCallback<T>() {

                @Override
                public void onError(Exception exception) {
                  futureResult.completeExceptionally(exception);
                }

                @Override
                public void onComplete(T response) {
                  futureResult.complete(response);
                }
              });
          if (throwException && expectedExceptionClass != null) {
            Exception ex =
                assertThrows(
                    expectedExceptionClass,
                    () -> {
                      try {
                        futureResult.get(TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);
                      } catch (ExecutionException x) {
                        throw x.getCause();
                      }
                    });
            assertEquals(expectedExceptionClass, ex.getClass());
            if (expectedExceptionMsg != null) {
              assertEquals(expectedExceptionMsg, ex.getMessage());
            }
          } else {
            T result;
            try {
              result = futureResult.get(TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);
            } catch (ExecutionException x) {
              throw x.getCause();
            }
            assertEquals(expectedResult, result);
          }
        } finally {
          asyncClientManager.stop();
        }
      }
    }

    public TServer getServer() {
      return server;
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustReturnResultReturnString(TestParameters p) throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnString",
          "sent msg",
          false,
          "sent msg",
          null,
          null,
          TAppService01.Iface::returnString);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustReturnResultReturnVoidThrows(TestParameters p) throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnVoidThrows",
          "sent msg",
          false,
          null,
          null,
          null,
          (client, msg, throwException) -> {
            client.returnVoidThrows(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustReturnResultReturnVoidNoThrowsRuntimeException(TestParameters p)
      throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnVoidNoThrowsRuntimeException",
          "sent msg",
          false,
          null,
          null,
          null,
          (client, msg, throwException) -> {
            client.returnVoidNoThrowsRuntimeException(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustReturnResultReturnVoidNoThrowsTApplicationException(
      TestParameters p) throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnVoidNoThrowsTApplicationException",
          "sent msg",
          false,
          null,
          null,
          null,
          (client, msg, throwException) -> {
            client.returnVoidNoThrowsTApplicationException(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustThrowExceptionReturnString(TestParameters p) throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnString",
          "sent msg",
          true,
          null,
          TExampleException.class,
          "sent msg",
          TAppService01.Iface::returnString);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustThrowExceptionReturnVoidThrows(TestParameters p) throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnVoidThrows",
          "sent msg",
          true,
          null,
          TExampleException.class,
          "sent msg",
          (client, msg, throwException) -> {
            client.returnVoidThrows(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustThrowExceptionReturnVoidNoThrowsRuntimeException(TestParameters p)
      throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnVoidNoThrowsRuntimeException",
          "sent msg",
          true,
          null,
          TApplicationException.class,
          p.serverImplementationType == ServerImplementationType.ASYNC_SERVER
              ? "sent msg"
              : null, // sync server return "Internal error processing
          // returnVoidNoThrowsRuntimeException" message
          (client, msg, throwException) -> {
            client.returnVoidNoThrowsRuntimeException(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientMustThrowExceptionReturnVoidNoThrowsTApplicationException(
      TestParameters p) throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "returnVoidNoThrowsTApplicationException",
          "sent msg",
          true,
          null,
          TApplicationException.class,
          "sent msg",
          (client, msg, throwException) -> {
            client.returnVoidNoThrowsTApplicationException(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustReturnResultReturnString(TestParameters p) throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnString",
          "sent msg",
          false,
          "sent msg",
          null,
          null,
          TAppService01.AsyncClient::returnString);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustReturnResultReturnVoidThrows(TestParameters p) throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnVoidThrows",
          "sent msg",
          false,
          null,
          null,
          null,
          TAppService01.AsyncClient::returnVoidThrows);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustReturnResultReturnVoidNoThrowsRuntimeException(TestParameters p)
      throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnVoidNoThrowsRuntimeException",
          "sent msg",
          false,
          null,
          null,
          null,
          TAppService01.AsyncClient::returnVoidNoThrowsRuntimeException);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustReturnResultReturnVoidNoThrowsTApplicationException(
      TestParameters p) throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnVoidNoThrowsTApplicationException",
          "sent msg",
          false,
          null,
          null,
          null,
          TAppService01.AsyncClient::returnVoidNoThrowsTApplicationException);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustThrowExceptionReturnString(TestParameters p) throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnString",
          "sent msg",
          true,
          null,
          TExampleException.class,
          "sent msg",
          TAppService01.AsyncClient::returnString);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustThrowExceptionReturnVoidThrows(TestParameters p) throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnVoidThrows",
          "sent msg",
          true,
          null,
          TExampleException.class,
          "sent msg",
          TAppService01.AsyncClient::returnVoidThrows);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustThrowExceptionReturnVoidNoThrowsRuntimeException(TestParameters p)
      throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnVoidNoThrowsRuntimeException",
          "sent msg",
          true,
          null,
          TApplicationException.class,
          p.serverImplementationType == ServerImplementationType.ASYNC_SERVER
              ? "sent msg"
              : null, // sync server return "Internal error processing
          // returnVoidNoThrowsRuntimeException" message
          TAppService01.AsyncClient::returnVoidNoThrowsRuntimeException);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientMustThrowExceptionReturnVoidNoThrowsTApplicationException(
      TestParameters p) throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "returnVoidNoThrowsTApplicationException",
          "sent msg",
          true,
          null,
          TApplicationException.class,
          "sent msg",
          TAppService01.AsyncClient::returnVoidNoThrowsTApplicationException);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientNoWaitForResultNoExceptionOnewayVoidNoThrows(TestParameters p)
      throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "onewayVoidNoThrows",
          "sent msg",
          false,
          null,
          null,
          null,
          (client, msg, throwException) -> {
            client.onewayVoidNoThrows(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testSyncClientNoWaitForResultExceptionOnewayVoidNoThrows(TestParameters p)
      throws Exception {
    try (AutoCloseable ignored = p.start()) {
      p.checkSyncClient(
          "onewayVoidNoThrows",
          "sent msg",
          true,
          null,
          null,
          null,
          (client, msg, throwException) -> {
            client.onewayVoidNoThrows(msg, throwException);
            return null;
          });
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientNoWaitForResultNoExceptionOnewayVoidNoThrows(TestParameters p)
      throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "onewayVoidNoThrows",
          "sent msg",
          false,
          null,
          null,
          null,
          TAppService01.AsyncClient::onewayVoidNoThrows);
    }
  }

  @ParameterizedTest
  @MethodSource("provideParameters")
  public void testAsyncClientNoWaitForResultExceptionOnewayVoidNoThrows(TestParameters p)
      throws Throwable {
    try (AutoCloseable ignored = p.start()) {
      p.checkAsyncClient(
          "onewayVoidNoThrows",
          "sent msg",
          true,
          null,
          null,
          null,
          TAppService01.AsyncClient::onewayVoidNoThrows);
    }
  }

  private enum ServerImplementationType {
    SYNC_SERVER(
        () -> {
          ServiceSyncImp service = new ServiceSyncImp();
          return Pair.of(new TAppService01.Processor<>(service), service);
        }),
    ASYNC_SERVER(
        () -> {
          ServiceAsyncImp service = new ServiceAsyncImp();
          return Pair.of(new TAppService01.AsyncProcessor<>(service), service);
        });

    final TProcessor processor;
    final ServiceBase service;

    ServerImplementationType(Supplier<Pair<TProcessor, ServiceBase>> supplier) {
      Pair<TProcessor, ServiceBase> pair = supplier.get();
      this.processor = pair.getLeft();
      this.service = pair.getRight();
    }
  }

  @FunctionalInterface
  private interface SyncCall<T, U, V, R> {
    R apply(T t, U u, V v) throws Exception;
  }

  @FunctionalInterface
  private interface AsyncCall<T, U, V, X> {
    void apply(T t, U u, V v, X x) throws Exception;
  }
}
