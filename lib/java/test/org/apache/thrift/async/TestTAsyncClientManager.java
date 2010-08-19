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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import junit.framework.TestCase;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.server.TNonblockingServer;
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
  private static abstract class FailureLessCallback<T extends TAsyncMethodCall> implements AsyncMethodCallback<T> {
    @Override
    public void onError(Throwable throwable) {
      throwable.printStackTrace();
      fail("unexpected error " + throwable);
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
        // TODO Auto-generated catch block
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
      this.clientSocket_ = new TNonblockingSocket("localhost", 12345);
      this.client_ = new Srv.AsyncClient(new TBinaryProtocol.Factory(), acm_, clientSocket_);
    }

    public int getNumSuccesses() {
      return numSuccesses_;
    }

    public void run() {
      for (int i = 0; i < numCalls_; i++) {
        try {
          // connect an async client
          final Object o = new Object();

          final AtomicBoolean jankyReturned = new AtomicBoolean(false);
          client_.Janky(1, new AsyncMethodCallback<Srv.AsyncClient.Janky_call>() {
            @Override
            public void onComplete(Janky_call response) {
              try {
                assertEquals(3, response.getResult());
                jankyReturned.set(true);
                synchronized(o) {
                  o.notifyAll();
                }
              } catch (TException e) {
                e.printStackTrace();
                synchronized(o) {
                  o.notifyAll();
                }
                fail("unexpected exception: " + e);
              }
            }

            @Override
            public void onError(Throwable throwable) {
              System.out.println(throwable.toString());
              synchronized(o) {
                o.notifyAll();
              }
              fail("unexpected exception: " + throwable);
            }
          });

          synchronized(o) {
            o.wait(1000);
          }

          assertTrue(jankyReturned.get());
          this.numSuccesses_++;
        } catch (Exception e) {
          fail("Unexpected " + e);
        }
      }
    }
  }

  public void standardCallTest(Srv.AsyncClient client) throws Exception {
    final Object o = new Object();
    final AtomicBoolean jankyReturned = new AtomicBoolean(false);
    client.Janky(1, new FailureLessCallback<Srv.AsyncClient.Janky_call>() {
      @Override
      public void onComplete(Janky_call response) {
        try {
          assertEquals(3, response.getResult());
          jankyReturned.set(true);
        } catch (TException e) {
          fail("unexpected exception: " + e);
        }
        synchronized(o) {
          o.notifyAll();
        }
      }
    });

    synchronized(o) {
      o.wait(100000);
    }
    assertTrue(jankyReturned.get());
  }

  public void testIt() throws Exception {
    // put up a server
    final TNonblockingServer s = new TNonblockingServer(new Srv.Processor(new SrvHandler()), new TNonblockingServerSocket(12345));
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
    TNonblockingSocket clientSock = new TNonblockingSocket("localhost", 12345);
    Srv.AsyncClient client = new Srv.AsyncClient(new TBinaryProtocol.Factory(), acm, clientSock);

    final Object o = new Object();

    // make a standard method call
    standardCallTest(client);

    // make a standard method call that succeeds within timeout
    client.setTimeout(5000);
    standardCallTest(client);

    // make a void method call
    final AtomicBoolean voidMethodReturned = new AtomicBoolean(false);
    client.voidMethod(new FailureLessCallback<Srv.AsyncClient.voidMethod_call>() {
      @Override
      public void onComplete(voidMethod_call response) {
        try {
          response.getResult();
          voidMethodReturned.set(true);
        } catch (TException e) {
          fail("unexpected exception " + e);
        }
        synchronized (o) {
          o.notifyAll();
        }
      }
    });

    synchronized(o) {
      o.wait(1000);
    }
    assertTrue(voidMethodReturned.get());
 
    // make a oneway method call
    final AtomicBoolean onewayReturned = new AtomicBoolean(false);
    client.onewayMethod(new FailureLessCallback<onewayMethod_call>() {
      @Override
      public void onComplete(onewayMethod_call response) {
        try {
          response.getResult();
          onewayReturned.set(true);
        } catch (TException e) {
          fail("unexpected exception " + e);
        }
        synchronized(o) {
          o.notifyAll();
        }
      }
    });
    synchronized(o) {
      o.wait(1000);
    }

    assertTrue(onewayReturned.get());

    // make another standard method call
    final AtomicBoolean voidAfterOnewayReturned = new AtomicBoolean(false);
    client.voidMethod(new FailureLessCallback<voidMethod_call>() {
      @Override
      public void onComplete(voidMethod_call response) {
        try {
          response.getResult();
          voidAfterOnewayReturned.set(true);
        } catch (TException e) {
          fail("unexpected exception " + e);
        }
        synchronized(o) {
          o.notifyAll();
        }
      }
    });
    synchronized(o) {
      o.wait(1000);
    }
    assertTrue(voidAfterOnewayReturned.get());

    // make multiple calls with deserialization in the selector thread (repro Eric's issue)
    int numThreads = 500;
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
    assertEquals(numSuccesses, numThreads * numCallsPerThread);

    // check that timeouts work
    client.setTimeout(100);
    client.primitiveMethod(new AsyncMethodCallback<primitiveMethod_call>() {

      @Override
      public void onError(Throwable throwable) {
        if (!(throwable instanceof TimeoutException)) {
          fail("should have received timeout exception");
          synchronized(o) {
            o.notifyAll();
          }
        }
      }

      @Override
      public void onComplete(primitiveMethod_call response) {
        fail("should not have finished timed out call.");
        synchronized(o) {
          o.notifyAll();
        }
      }

    });
    synchronized(o) {
      o.wait(2000);
    }
    assertTrue(client.hasError());
    assertTrue(client.getError() instanceof TimeoutException);
  }
}
