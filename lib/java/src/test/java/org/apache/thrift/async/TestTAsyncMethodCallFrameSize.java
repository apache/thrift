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
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * TAsyncMethodCall sizes the response frame from the four bytes the peer sends ahead of it. Its
 * sibling TFramedTransport.readFrame refuses a negative size and one over the configured maximum;
 * this checks that the async client does the same rather than handing the number to
 * ByteBuffer.allocate.
 *
 * <p>What the tests assert is the buffer that was allocated, not just that something went wrong: a
 * response that never arrives ends the call either way.
 */
public class TestTAsyncMethodCallFrameSize {

  private static final int MAX_FRAME_SIZE = 1024;

  /** How long the client is driven before a call that makes no progress is given up on. */
  private static final long CLIENT_DEADLINE_MS = 3000;

  /** Comfortably longer, so that the peer closing can never be mistaken for the answer. */
  private static final long SERVER_HOLD_MS = 30000;

  /** Accepts one connection, swallows the request and answers with a frame size of our choosing. */
  private static class SizeOnlyServer implements Runnable, AutoCloseable {
    private final ServerSocket serverSocket;
    private final int frameSize;
    private final boolean sendBody;
    private final Thread thread;

    SizeOnlyServer(int frameSize, boolean sendBody) throws IOException {
      this.serverSocket = new ServerSocket(0);
      this.frameSize = frameSize;
      this.sendBody = sendBody;
      this.thread = new Thread(this, "size-only-server");
      this.thread.setDaemon(true);
      this.thread.start();
    }

    int getPort() {
      return serverSocket.getLocalPort();
    }

    @Override
    public void run() {
      try (Socket socket = serverSocket.accept()) {
        // The request is of no interest; read whatever the client sends first so
        // that it gets as far as waiting for a response.
        byte[] scratch = new byte[256];
        socket.getInputStream().read(scratch);
        DataOutputStream out = new DataOutputStream(socket.getOutputStream());
        out.writeInt(frameSize);
        if (sendBody) {
          out.write(new byte[frameSize]);
        }
        out.flush();
        // Hold the connection open: the point is what the client does with the
        // size, not what it does when the peer disappears.
        Thread.sleep(SERVER_HOLD_MS);
      } catch (Exception e) {
        // The test has finished with us.
      }
    }

    @Override
    public void close() throws IOException {
      serverSocket.close();
    }
  }

  private static class NoArgsCall extends TAsyncMethodCall<Void> {
    NoArgsCall(
        TAsyncClient client, TNonblockingTransport transport, AsyncMethodCallback<Void> callback) {
      super(client, new TBinaryProtocol.Factory(), transport, callback, false);
    }

    @Override
    protected void write_args(TProtocol prot) {}

    @Override
    protected Void getResult() {
      return null;
    }
  }

  private Selector selector;

  @BeforeEach
  public void setUp() throws Exception {
    selector = Selector.open();
  }

  @AfterEach
  public void tearDown() throws Exception {
    selector.close();
  }

  /** Runs the call to completion or to its first error, and hands back whichever came first. */
  private NoArgsCall drive(
      int declaredFrameSize, boolean sendBody, AtomicReference<Exception> error) throws Exception {
    try (SizeOnlyServer server = new SizeOnlyServer(declaredFrameSize, sendBody)) {
      TNonblockingTransport transport = new TNonblockingSocket("localhost", server.getPort());
      transport.setMaxFrameSize(MAX_FRAME_SIZE);
      TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();
      TAsyncClient client =
          new TAsyncClient(protocolFactory, new TAsyncClientManager(), transport) {};

      NoArgsCall call =
          new NoArgsCall(
              client,
              transport,
              new AsyncMethodCallback<Void>() {
                @Override
                public void onComplete(Void response) {}

                @Override
                public void onError(Exception exception) {
                  error.compareAndSet(null, exception);
                }
              });

      call.prepareMethodCall();
      call.start(selector);
      long deadline = System.currentTimeMillis() + CLIENT_DEADLINE_MS;
      while (error.get() == null && !call.isFinished() && System.currentTimeMillis() < deadline) {
        selector.select(100);
        Set<SelectionKey> keys = selector.selectedKeys();
        for (Iterator<SelectionKey> it = keys.iterator(); it.hasNext(); ) {
          SelectionKey key = it.next();
          it.remove();
          TAsyncMethodCall<?> pending = (TAsyncMethodCall<?>) key.attachment();
          if (pending != null) {
            pending.transition(key);
          }
        }
      }
      transport.close();
      return call;
    }
  }

  @Test
  public void testFrameSizeOverTheConfiguredMaximumIsRefused() throws Exception {
    AtomicReference<Exception> error = new AtomicReference<>();
    NoArgsCall call = drive(64 * 1024, false, error);

    // The buffer is the request's until a response frame replaces it, so what
    // this says is that no buffer over the configured maximum was ever made.
    // Asking only whether the call failed would pass either way: a response
    // body that never arrives ends it in both cases.
    assertTrue(
        call.getFrameBuffer().capacity() <= MAX_FRAME_SIZE,
        "allocated " + call.getFrameBuffer().capacity() + " for a frame that is over the maximum");
    assertNotNull(error.get(), "a frame larger than maxFrameSize has to be refused");
    assertInstanceOf(TTransportException.class, error.get());
    assertTrue(
        error.get().getMessage().contains("larger than max length"),
        "unexpected message: " + error.get().getMessage());
  }

  @Test
  public void testNegativeFrameSizeIsRefused() throws Exception {
    AtomicReference<Exception> error = new AtomicReference<>();
    NoArgsCall call = drive(-1, false, error);

    assertTrue(
        call.getFrameBuffer().capacity() <= MAX_FRAME_SIZE,
        "allocated " + call.getFrameBuffer().capacity() + " for a negative frame size");
    assertNotNull(error.get(), "a negative frame size has to be refused");
    assertInstanceOf(
        TTransportException.class,
        error.get(),
        "a negative size reached ByteBuffer.allocate: " + error.get());
  }

  @Test
  public void testFrameSizeWithinTheMaximumIsAccepted() throws Exception {
    AtomicReference<Exception> error = new AtomicReference<>();
    NoArgsCall call = drive(MAX_FRAME_SIZE, true, error);

    assertNull(error.get(), "a frame of exactly the maximum has to be read: " + error.get());
    assertTrue(call.isFinished(), "the response was not read to the end");
    assertNotNull(call.getFrameBuffer());
    assertEquals(MAX_FRAME_SIZE, call.getFrameBuffer().capacity());
  }
}
