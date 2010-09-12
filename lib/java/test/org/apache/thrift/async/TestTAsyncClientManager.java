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
package org.apache.thrift.async;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import junit.framework.TestCase;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.server.ServerTestBase;
import org.apache.thrift.server.THsHaServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingSocket;

import thrift.test.CompactProtoTestStruct;
import thrift.test.Srv;
import thrift.test.Srv.Iface;
import thrift.test.Srv.AsyncClient.Janky_call;
import thrift.test.Srv.AsyncClient.onewayMethod_call;
import thrift.test.Srv.AsyncClient.primitiveMethod_call;
import thrift.test.Srv.AsyncClient.voidMethod_call;

public class TestTAsyncClientManager extends TestCase {
  private static void fail(Throwable throwable) {
    StringWriter sink = new StringWriter();
    throwable.printStackTrace(new PrintWriter(sink, true));
    fail("unexpected error " + sink.toString());
  }

  private static abstract class FailureLessCallback<T extends TAsyncMethodCall> implements AsyncMethodCallback<T> {
    @Override
    public void onError(Throwable throwable) {
      fail(throwable);
    }
  }

  public class SrvHandler implements Iface {
    @Override
    public int Janky(int arg) throws TException {
      assertEquals(1, arg);
      return 3;
    }

    @Override
    public void methodWithDefaultArgs(int something) throws TException {
    }

    // Using this method for timeout testing
    @Override
    public int primitiveMethod() throws TException {
      try {
        Thread.sleep(1000);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
      return 0;
    }

    @Override
    public CompactProtoTestStruct structMethod() throws TException {
      return null;
    }

    @Override
    public void voidMethod() throws TException {
    }

    @Override
    public void onewayMethod() throws TException {
    }
  }

  public class JankyRunnable implements Runnable {
    private TAsyncClientManager acm_;
    private int numCalls_;
    private int numSuccesses_ = 0;
    private Srv.AsyncClient client_;
    private TNonblockingSocket clientSocket_;

    public JankyRunnable(TAsyncClientManager acm, int numCalls) throws Exception {
      this.acm_ = acm;
      this.numCalls_ = numCalls;
      this.clientSocket_ = new TNonblockingSocket(ServerTestBase.HOST, ServerTestBase.PORT);
      this.client_ = new Srv.AsyncClient(new TBinaryProtocol.Factory(), acm_, clientSocket_);
      this.client_.setTimeout(20000);
    }

    public int getNumSuccesses() {
      return numSuccesses_;
    }

    public void run() {
      for (int i = 0; i < numCalls_ && !client_.hasError(); i++) {
        final int iteration = i;
        try {
          // connect an async client
          final CountDownLatch latch = new CountDownLatch(1);
          final AtomicBoolean jankyReturned = new AtomicBoolean(false);
          client_.Janky(1, new AsyncMethodCallback<Srv.AsyncClient.Janky_call>() {

            @Override
            public void onComplete(Janky_call response) {
              try {
                assertEquals(3, response.getResult());
                jankyReturned.set(true);
                latch.countDown();
              } catch (TException e) {
                latch.countDown();
                fail(e);
              }
            }

            @Override
            public void onError(Throwable throwable) {
              try {
                StringWriter sink = new StringWriter();
                throwable.printStackTrace(new PrintWriter(sink, true));
                fail("unexpected onError on iteration " + iteration + ": " + sink.toString());
              } finally {
                latch.countDown();
              }
            }
          });

          boolean calledBack = latch.await(30, TimeUnit.SECONDS);
          assertTrue("wasn't called back in time on iteration " + iteration, calledBack);
          assertTrue("onComplete not called on iteration " + iteration, jankyReturned.get());
          this.numSuccesses_++;
        } catch (Exception e) {
          fail(e);
        }
      }
    }
  }

  public void standardCallTest(Srv.AsyncClient client) throws Exception {
    final CountDownLatch latch = new CountDownLatch(1);
    final AtomicBoolean jankyReturned = new AtomicBoolean(false);
    client.Janky(1, new FailureLessCallback<Srv.AsyncClient.Janky_call>() {
      @Override
      public void onComplete(Janky_call response) {
        try {
          assertEquals(3, response.getResult());
          jankyReturned.set(true);
        } catch (TException e) {
          fail(e);
        } finally {
          latch.countDown();
        }
      }
    });

    latch.await(100, TimeUnit.SECONDS);
    assertTrue(jankyReturned.get());
  }

  public void testIt() throws Exception {
    // put up a server
    final THsHaServer s = new THsHaServer(new Srv.Processor(new SrvHandler()),
      new TNonblockingServerSocket(ServerTestBase.PORT));
    new Thread(new Runnable() {
      @Override
      public void run() {
        s.serve();
      }
    }).start();
    Thread.sleep(1000);

    // set up async client manager
    TAsyncClientManager acm = new TAsyncClientManager();

    // connect an async client
    TNonblockingSocket clientSock = new TNonblockingSocket(
      ServerTestBase.HOST, ServerTestBase.PORT);
    Srv.AsyncClient client = new Srv.AsyncClient(new TBinaryProtocol.Factory(), acm, clientSock);

    // make a standard method call
    standardCallTest(client);

    // make a standard method call that succeeds within timeout
    assertFalse(s.isStopped());
    client.setTimeout(5000);
    standardCallTest(client);

    // make a void method call
    assertFalse(s.isStopped());
    final CountDownLatch voidLatch = new CountDownLatch(1);
    final AtomicBoolean voidMethodReturned = new AtomicBoolean(false);
    client.voidMethod(new FailureLessCallback<Srv.AsyncClient.voidMethod_call>() {
      @Override
      public void onComplete(voidMethod_call response) {
        try {
          response.getResult();
          voidMethodReturned.set(true);
        } catch (TException e) {
          fail(e);
        } finally {
          voidLatch.countDown();
        }
      }
    });
    voidLatch.await(1, TimeUnit.SECONDS);
    assertTrue(voidMethodReturned.get());

    // make a oneway method call
    assertFalse(s.isStopped());
    final CountDownLatch onewayLatch = new CountDownLatch(1);
    final AtomicBoolean onewayReturned = new AtomicBoolean(false);
    client.onewayMethod(new FailureLessCallback<onewayMethod_call>() {
      @Override
      public void onComplete(onewayMethod_call response) {
        try {
          response.getResult();
          onewayReturned.set(true);
        } catch (TException e) {
          fail(e);
        } finally {
          onewayLatch.countDown();
        }
      }
    });
    onewayLatch.await(1, TimeUnit.SECONDS);
    assertTrue(onewayReturned.get());

    // make another standard method call
    assertFalse(s.isStopped());
    final CountDownLatch voidAfterOnewayLatch = new CountDownLatch(1);
    final AtomicBoolean voidAfterOnewayReturned = new AtomicBoolean(false);
    client.voidMethod(new FailureLessCallback<voidMethod_call>() {
      @Override
      public void onComplete(voidMethod_call response) {
        try {
          response.getResult();
          voidAfterOnewayReturned.set(true);
        } catch (TException e) {
          fail(e);
        } finally {
          voidAfterOnewayLatch.countDown();
        }
      }
    });
    voidAfterOnewayLatch.await(1, TimeUnit.SECONDS);
    assertTrue(voidAfterOnewayReturned.get());

    // make multiple calls with deserialization in the selector thread (repro Eric's issue)
    assertFalse(s.isStopped());
    int numThreads = 50;
    int numCallsPerThread = 100;
    List<JankyRunnable> runnables = new ArrayList<JankyRunnable>();
    List<Thread> threads = new ArrayList<Thread>();
    for (int i = 0; i < numThreads; i++) {
      JankyRunnable runnable = new JankyRunnable(acm, numCallsPerThread);
      Thread thread = new Thread(runnable);
      thread.start();
      threads.add(thread);
      runnables.add(runnable);
    }
    for (Thread thread : threads) {
      thread.join();
    }
    int numSuccesses = 0;
    for (JankyRunnable runnable : runnables) {
      numSuccesses += runnable.getNumSuccesses();
    }
    assertEquals(numThreads * numCallsPerThread, numSuccesses);

    // check that timeouts work
    assertFalse(s.isStopped());
    assertTrue(clientSock.isOpen());
    final CountDownLatch timeoutLatch = new CountDownLatch(1);
    client.setTimeout(100);
    client.primitiveMethod(new AsyncMethodCallback<primitiveMethod_call>() {

      @Override
      public void onError(Throwable throwable) {
        try {
          if (!(throwable instanceof TimeoutException)) {
            StringWriter sink = new StringWriter();
            throwable.printStackTrace(new PrintWriter(sink, true));
            fail("expected TimeoutException but got " + sink.toString());
          }
        } finally {
          timeoutLatch.countDown();
        }
      }

      @Override
      public void onComplete(primitiveMethod_call response) {
        try {
          fail("should not have finished timed out call.");
        } finally {
          timeoutLatch.countDown();
        }
      }

    });
    timeoutLatch.await(2, TimeUnit.SECONDS);
    assertTrue(client.hasError());
    assertTrue(client.getError() instanceof TimeoutException);

    // error closes socket and make sure isOpen reflects that
    assertFalse(clientSock.isOpen());
  }
}
