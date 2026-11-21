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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.junit.jupiter.api.Test;

/**
 * An Error while a call is handled on the selector thread, such as an OutOfMemoryError while its
 * response is decoded, ends that call only: the manager goes on serving the other calls.
 */
public class TestTAsyncClientManagerErrors {

  private static final long WAIT_SECONDS = 10;

  /** Answers every request on every connection with a four-byte response frame. */
  private static class ReplyingServer implements Runnable, AutoCloseable {
    private final ServerSocket serverSocket;
    private final Thread thread;

    ReplyingServer() throws IOException {
      this.serverSocket = new ServerSocket(0);
      this.thread = new Thread(this, "replying-server");
      this.thread.setDaemon(true);
      this.thread.start();
    }

    int getPort() {
      return serverSocket.getLocalPort();
    }

    @Override
    public void run() {
      while (!serverSocket.isClosed()) {
        try (Socket socket = serverSocket.accept()) {
          byte[] scratch = new byte[256];
          socket.getInputStream().read(scratch);
          DataOutputStream out = new DataOutputStream(socket.getOutputStream());
          out.writeInt(4);
          out.writeInt(42);
          out.flush();
          // the client closes when it has read the response
          socket.getInputStream().read(scratch);
        } catch (IOException e) {
          // the next connection, or the end of the test
        }
      }
    }

    @Override
    public void close() throws IOException {
      serverSocket.close();
    }
  }

  /** A call whose response handling fails with the given Error, or succeeds without one. */
  private static class Call extends TAsyncMethodCall<Integer> {
    private final Error failure;

    Call(
        TAsyncClient client,
        TNonblockingTransport transport,
        AsyncMethodCallback<Integer> callback,
        Error failure) {
      super(client, new TBinaryProtocol.Factory(), transport, callback, false);
      this.failure = failure;
    }

    @Override
    protected void write_args(TProtocol prot) {}

    @Override
    protected Integer getResult() {
      if (failure != null) {
        throw failure;
      }
      return 42;
    }
  }

  private static class Outcome implements AsyncMethodCallback<Integer> {
    final CountDownLatch done = new CountDownLatch(1);
    final AtomicReference<Integer> result = new AtomicReference<>();
    final AtomicReference<Exception> error = new AtomicReference<>();

    @Override
    public void onComplete(Integer response) {
      result.set(response);
      done.countDown();
    }

    @Override
    public void onError(Exception exception) {
      error.set(exception);
      done.countDown();
    }
  }

  private static Outcome call(TAsyncClientManager manager, int port, Error failure)
      throws Exception {
    TNonblockingTransport transport = new TNonblockingSocket("localhost", port);
    TAsyncClient client = new TAsyncClient(new TBinaryProtocol.Factory(), manager, transport) {};
    Outcome outcome = new Outcome();
    manager.call(new Call(client, transport, outcome, failure));
    return outcome;
  }

  @Test
  public void testErrorWhileHandlingAResponseEndsOnlyThatCall() throws Exception {
    TAsyncClientManager manager = new TAsyncClientManager();
    try (ReplyingServer server = new ReplyingServer()) {
      Outcome failed =
          call(manager, server.getPort(), new OutOfMemoryError("simulated while decoding"));
      assertTrue(
          failed.done.await(WAIT_SECONDS, TimeUnit.SECONDS),
          "the call whose response could not be handled got no callback");
      assertNotNull(failed.error.get(), "the call whose response could not be handled succeeded");
      assertTrue(manager.isRunning(), "the manager stopped after one call failed");

      Outcome next = call(manager, server.getPort(), null);
      assertTrue(next.done.await(WAIT_SECONDS, TimeUnit.SECONDS), "the next call got no callback");
      assertEquals(Integer.valueOf(42), next.result.get(), "the next call failed: " + next.error);
    } finally {
      manager.stop();
    }
  }
}
