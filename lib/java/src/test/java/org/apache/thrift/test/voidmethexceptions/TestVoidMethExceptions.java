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
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import thrift.test.voidmethexceptions.TAppService01;
import thrift.test.voidmethexceptions.TExampleException;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

@RunWith(Parameterized.class)
public class TestVoidMethExceptions {

  private static final Logger log = LoggerFactory.getLogger(TestVoidMethExceptions.class);

  private static final int TIMEOUT_MILLIS = 5_000;

  private final ServerImplementationType serverImplementationType;

  private TServer server;
  private Thread serverThread;
  private int serverPort;


  public TestVoidMethExceptions(ServerImplementationType serverImplementationType) {
    Assert.assertNotNull(serverImplementationType);
    this.serverImplementationType = serverImplementationType;
  }


  @Parameters(name = "serverImplementationType = {0}")
  public static Object[][] parameters() {
    return new Object[][]{{ServerImplementationType.SYNC_SERVER},
        {ServerImplementationType.ASYNC_SERVER}};
  }


  @Before
  public void setUp() throws Exception {
    serverPort = -1;
    serverImplementationType.service.setCancelled(false);
    CompletableFuture<Void> futureServerStarted = new CompletableFuture<>();
    TNonblockingServerSocket serverTransport = new TNonblockingServerSocket(0);
    TNonblockingServer.Args args = new TNonblockingServer.Args(serverTransport);
    args.processor(serverImplementationType.processor);
    server = new TNonblockingServer(args) {

      @Override
      protected void setServing(boolean serving) {
        super.setServing(serving);

        if (serving) {
          serverPort = serverTransport.getPort();
          futureServerStarted.complete(null);
        }
      }

    };

    serverThread = new Thread(() -> {
      server.serve();
    }, "thrift-server");
    serverThread.setDaemon(true);
    serverThread.start();
    futureServerStarted.get(TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);
  }

  @After
  public void tearDown() throws Exception {
    serverImplementationType.service.setCancelled(true);
    server.stop();
    serverThread.join(TIMEOUT_MILLIS);
  }


  @Test
  public void testSyncClientMustReturnResultReturnString() throws Exception {
    checkSyncClient("returnString",
        "sent msg",
        false,
        "sent msg",
        null,
        null,
        (client, msg, throwException) -> {
          return client.returnString(msg, throwException);
        });
  }

  @Test
  public void testSyncClientMustReturnResultReturnVoidThrows() throws Exception {
    checkSyncClient("returnVoidThrows",
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

  @Test
  public void testSyncClientMustReturnResultReturnVoidNoThrowsRuntimeException() throws Exception {
    checkSyncClient("returnVoidNoThrowsRuntimeException",
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

  @Test
  public void testSyncClientMustReturnResultReturnVoidNoThrowsTApplicationException() throws Exception {
    checkSyncClient("returnVoidNoThrowsTApplicationException",
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


  @Test
  public void testSyncClientMustThrowExceptionReturnString() throws Exception {
    checkSyncClient("returnString",
        "sent msg",
        true,
        null,
        TExampleException.class,
        "sent msg",
        (client, msg, throwException) -> {
          return client.returnString(msg, throwException);
        });
  }

  @Test
  public void testSyncClientMustThrowExceptionReturnVoidThrows() throws Exception {
    checkSyncClient("returnVoidThrows",
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

  @Test
  public void testSyncClientMustThrowExceptionReturnVoidNoThrowsRuntimeException() throws Exception {
    checkSyncClient("returnVoidNoThrowsRuntimeException",
        "sent msg",
        true,
        null,
        TApplicationException.class,
        serverImplementationType == ServerImplementationType.ASYNC_SERVER ? "sent msg"
            : null, // sync server return "Internal error processing returnVoidNoThrowsRuntimeException" message
        (client, msg, throwException) -> {
          client.returnVoidNoThrowsRuntimeException(msg, throwException);
          return null;
        });
  }

  @Test
  public void testSyncClientMustThrowExceptionReturnVoidNoThrowsTApplicationException() throws Exception {
    checkSyncClient("returnVoidNoThrowsTApplicationException",
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


  @Test
  public void testAsyncClientMustReturnResultReturnString() throws Throwable {
    checkAsyncClient("returnString",
        "sent msg",
        false,
        "sent msg",
        null,
        null,
        (client, msg, throwException, resultHandler) -> {
          client.returnString(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientMustReturnResultReturnVoidThrows() throws Throwable {
    checkAsyncClient("returnVoidThrows",
        "sent msg",
        false,
        (Void) null,
        null,
        null,
        (client, msg, throwException, resultHandler) -> {
          client.returnVoidThrows(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientMustReturnResultReturnVoidNoThrowsRuntimeException() throws Throwable {
    checkAsyncClient("returnVoidNoThrowsRuntimeException",
        "sent msg",
        false,
        (Void) null,
        null,
        null,
        (client, msg, throwException, resultHandler) -> {
          client.returnVoidNoThrowsRuntimeException(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientMustReturnResultReturnVoidNoThrowsTApplicationException() throws Throwable {
    checkAsyncClient("returnVoidNoThrowsTApplicationException",
        "sent msg",
        false,
        (Void) null,
        null,
        null,
        (client, msg, throwException, resultHandler) -> {
          client.returnVoidNoThrowsTApplicationException(msg, throwException, resultHandler);
        });
  }


  @Test
  public void testAsyncClientMustThrowExceptionReturnString() throws Throwable {
    checkAsyncClient("returnString",
        "sent msg",
        true,
        (String) null,
        TExampleException.class,
        "sent msg",
        (client, msg, throwException, resultHandler) -> {
          client.returnString(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientMustThrowExceptionReturnVoidThrows() throws Throwable {
    checkAsyncClient("returnVoidThrows",
        "sent msg",
        true,
        (Void) null,
        TExampleException.class,
        "sent msg",
        (client, msg, throwException, resultHandler) -> {
          client.returnVoidThrows(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientMustThrowExceptionReturnVoidNoThrowsRuntimeException() throws Throwable {
    checkAsyncClient("returnVoidNoThrowsRuntimeException",
        "sent msg",
        true,
        (Void) null,
        TApplicationException.class,
        serverImplementationType == ServerImplementationType.ASYNC_SERVER ? "sent msg"
            : null, // sync server return "Internal error processing returnVoidNoThrowsRuntimeException" message
        (client, msg, throwException, resultHandler) -> {
          client.returnVoidNoThrowsRuntimeException(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientMustThrowExceptionReturnVoidNoThrowsTApplicationException() throws Throwable {
    checkAsyncClient("returnVoidNoThrowsTApplicationException",
        "sent msg",
        true,
        (Void) null,
        TApplicationException.class,
        "sent msg",
        (client, msg, throwException, resultHandler) -> {
          client.returnVoidNoThrowsTApplicationException(msg, throwException, resultHandler);
        });
  }


  @Test
  public void testSyncClientNoWaitForResultNoExceptionOnewayVoidNoThrows() throws Exception {
    checkSyncClient("onewayVoidNoThrows",
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

  @Test
  public void testSyncClientNoWaitForResultExceptionOnewayVoidNoThrows() throws Exception {
    checkSyncClient("onewayVoidNoThrows",
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

  @Test
  public void testAsyncClientNoWaitForResultNoExceptionOnewayVoidNoThrows() throws Throwable {
    checkAsyncClient("onewayVoidNoThrows",
        "sent msg",
        false,
        (Void) null,
        null,
        null,
        (client, msg, throwException, resultHandler) -> {
          client.onewayVoidNoThrows(msg, throwException, resultHandler);
        });
  }

  @Test
  public void testAsyncClientNoWaitForResultExceptionOnewayVoidNoThrows() throws Throwable {
    checkAsyncClient("onewayVoidNoThrows",
        "sent msg",
        true,
        (Void) null,
        null,
        null,
        (client, msg, throwException, resultHandler) -> {
          client.onewayVoidNoThrows(msg, throwException, resultHandler);
        });
  }


  private void checkSyncClient(String desc,
      String msg,
      boolean throwException,
      String expectedResult,
      Class<?> expectedExceptionClass,
      String expectedExceptionMsg,
      SyncCall<TAppService01.Iface, String, Boolean, String> call) throws Exception {
    if (log.isInfoEnabled()) {
      log.info("start test checkSyncClient::" + desc + ", throwException: " + throwException
          + ", serverImplementationType: "
          + serverImplementationType);
    }
    Assert.assertNotEquals(-1, serverPort);
    try (TTransport clientTransport = new TFramedTransport(new TSocket(new TConfiguration(),
        "localhost",
        serverPort,
        TIMEOUT_MILLIS))) {
      clientTransport.open();
      TAppService01.Iface client = new TAppService01.Client(new TBinaryProtocol(clientTransport));

      try {

        String result = call.apply(client, msg, throwException);

        if (throwException && expectedExceptionClass != null) {
          Assert.fail("No exception, but must!!!");
        } else {
          // expected
          Assert.assertEquals(expectedResult, result);
        }
      } catch (TExampleException | TApplicationException x) {
        if (log.isInfoEnabled()) {
          log.info("Exception: " + x, x);
        }
        if (throwException) {
          // expected
          Assert.assertEquals(expectedExceptionClass, x.getClass());
          if (expectedExceptionMsg != null) {
            Assert.assertEquals(expectedExceptionMsg, x.getMessage());
          }
        } else {
          Assert.fail();
        }
      }
    }
  }

  private <T> void checkAsyncClient(String desc,
      String msg,
      boolean throwException,
      T expectedResult,
      Class<?> expectedExceptionClass,
      String expectedExceptionMsg,
      AsyncCall<TAppService01.AsyncClient, String, Boolean, AsyncMethodCallback<T>> call) throws Throwable {
    if (log.isInfoEnabled()) {
      log.info("start test checkAsyncClient::" + desc + ", throwException: " + throwException
          + ", serverImplementationType: "
          + serverImplementationType);
    }
    Assert.assertNotEquals(serverPort, -1);
    try (TNonblockingSocket clientTransportAsync = new TNonblockingSocket("localhost", serverPort, TIMEOUT_MILLIS)) {
      TAsyncClientManager asyncClientManager = new TAsyncClientManager();
      try {
        TAppService01.AsyncClient asyncClient = new TAppService01.AsyncClient(new TBinaryProtocol.Factory(),
            asyncClientManager,
            clientTransportAsync);
        asyncClient.setTimeout(TIMEOUT_MILLIS);

        CompletableFuture<T> futureResult = new CompletableFuture<>();

        call.apply(asyncClient, msg, throwException, new AsyncMethodCallback<T>() {

          @Override
          public void onError(Exception exception) {
            futureResult.completeExceptionally(exception);
          }

          @Override
          public void onComplete(T response) {
            futureResult.complete(response);
          }

        });

        try {
          T result;
          try {
            result = futureResult.get(TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);
          } catch (ExecutionException x) {
            throw x.getCause();
          }

          if (throwException && expectedExceptionClass != null) {
            Assert.fail("No exception, but must!!!");
          } else {
            // expected
            Assert.assertEquals(expectedResult, result);
          }
        } catch (TExampleException | TApplicationException x) {
          if (log.isInfoEnabled()) {
            log.info("Exception: " + x, x);
          }
          if (throwException) {
            // expected
            Assert.assertEquals(expectedExceptionClass, x.getClass());
            if (expectedExceptionMsg != null) {
              Assert.assertEquals(expectedExceptionMsg, x.getMessage());
            }
          } else {
            Assert.fail();
          }
        }
      } finally {
        asyncClientManager.stop();
      }
    }
  }


  private enum ServerImplementationType {

    SYNC_SERVER(() -> {
      ServiceSyncImp service = new ServiceSyncImp();
      return Pair.of(new TAppService01.Processor<>(service), service);
    }),
    ASYNC_SERVER(() -> {
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
