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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.AdditionalAnswers.delegatesTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509ExtendedTrustManager;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingSSLSocket;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.junit.jupiter.api.Test;

public class TestTAsyncSSLProgress {
  @Test
  public void testDelayedHandshakeAllowsOtherCallsAndTimeouts() throws Exception {
    checkStall(true);
  }

  @Test
  public void testPartialResponseAllowsOtherCallsAndTimeouts() throws Exception {
    checkStall(false);
  }

  @Test
  public void testSunJsseTls12TaskDoesNotDelayTimeoutOrOtherCalls() throws Exception {
    checkTaskTimeout("TLSv1.2");
  }

  @Test
  public void testSunJsseTls13TaskDoesNotDelayTimeoutOrOtherCalls() throws Exception {
    checkTaskTimeout("TLSv1.3");
  }

  private void checkTaskTimeout(String protocol) throws Exception {
    assumeTrue(Security.getProvider("SunJSSE") != null);
    assumeTrue(
        Arrays.asList(SSLContext.getDefault().getSupportedSSLParameters().getProtocols())
            .contains(protocol));
    AtomicReference<SSLEngine> engine = new AtomicReference<>();
    CountDownLatch taskStarted = new CountDownLatch(1), releaseTask = new CountDownLatch(1);
    AtomicReference<Thread> taskThread = new AtomicReference<>();
    AtomicReference<Socket> peer = new AtomicReference<>();
    TrustManagerFactory factory =
        TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
    factory.init((KeyStore) null);
    X509ExtendedTrustManager delegate = (X509ExtendedTrustManager) factory.getTrustManagers()[0];
    X509ExtendedTrustManager trust = mock(X509ExtendedTrustManager.class, delegatesTo(delegate));
    doAnswer(
            call -> {
              engine.set(call.getArgument(2));
              taskThread.set(Thread.currentThread());
              taskStarted.countDown();
              assertTrue(releaseTask.await(10, TimeUnit.SECONDS), "TLS task was not released");
              delegate.checkServerTrusted(
                  call.getArgument(0), call.getArgument(1), (SSLEngine) call.getArgument(2));
              return null;
            })
        .when(trust)
        .checkServerTrusted(any(X509Certificate[].class), anyString(), any(SSLEngine.class));
    // Keep the real provider's delegated task and engine locks; only delay certificate validation.
    SSLContext context = SSLContext.getInstance(protocol, "SunJSSE");
    context.init(null, new TrustManager[] {trust}, null);
    try (SSLServerSocket listener =
            (SSLServerSocket)
                SSLContext.getDefault().getServerSocketFactory().createServerSocket(0);
        ServerSocket healthyListener = new ServerSocket(0)) {
      listener.setEnabledProtocols(new String[] {protocol});
      listener.setSoTimeout(5000);
      healthyListener.setSoTimeout(5000);
      CompletableFuture<Void> serverDone =
          serve(
              listener,
              socket -> {
                peer.set(socket);
                try {
                  ((SSLSocket) socket).startHandshake();
                } catch (IOException expected) {
                  /* The client times out during the handshake. */
                }
              });
      CompletableFuture<Void> healthyServer =
          serve(
              healthyListener,
              socket -> {
                readRequest(socket);
                DataOutputStream output = new DataOutputStream(socket.getOutputStream());
                output.writeInt(8);
                output.write("ABCDEFGH".getBytes(StandardCharsets.US_ASCII));
                output.flush();
              });
      TAsyncClientManager manager = new TAsyncClientManager();
      TNonblockingSSLSocket stalled = new ContextSocket(listener.getLocalPort(), context);
      TNonblockingSocket healthy =
          new TNonblockingSocket("localhost", healthyListener.getLocalPort());
      try {
        CompletableFuture<byte[]> result = call(manager, stalled, 1000);
        assertTrue(taskStarted.await(3, TimeUnit.SECONDS));
        assertFalse(taskThread.get().getName().startsWith("TAsyncClientManager"));
        ExecutionException error =
            assertThrows(ExecutionException.class, () -> result.get(3, TimeUnit.SECONDS));
        assertTrue(error.getCause() instanceof TimeoutException);
        assertFalse(stalled.getSocketChannel().isOpen());
        // The task is still blocked when the next call is queued, after the first timeout.
        assertEquals(1L, releaseTask.getCount());
        stalled.close(); // Closing again must not wait for the same provider task either.
        assertArrayEquals(
            "ABCDEFGH".getBytes(StandardCharsets.US_ASCII),
            call(manager, healthy, 2000).get(3, TimeUnit.SECONDS));
        healthyServer.get(2, TimeUnit.SECONDS);
      } finally {
        releaseTask.countDown();
        if (peer.get() != null) peer.get().close();
        manager.stop();
        stalled.close();
        healthy.close();
      }
      serverDone.get(3, TimeUnit.SECONDS);
      assertTimeoutPreemptively(
          Duration.ofSeconds(3),
          () -> {
            SSLEngine closedEngine = engine.get();
            ByteBuffer output =
                ByteBuffer.allocate(closedEngine.getSession().getPacketBufferSize());
            // closeOutbound may leave a closure record to wrap even after the channel is closed.
            while (!closedEngine.isOutboundDone()) {
              closedEngine.wrap(ByteBuffer.allocate(0), output);
              output.clear();
              Thread.sleep(10);
            }
          });
    }
  }

  private static class ContextSocket extends TNonblockingSSLSocket {
    ContextSocket(int port, SSLContext context) throws Exception {
      super("localhost", port, 0, context);
    }
  }

  private void checkStall(boolean handshake) throws Exception {
    CountDownLatch ready = new CountDownLatch(1), release = new CountDownLatch(1);
    AtomicReference<Socket> peer = new AtomicReference<>();
    try (ServerSocket stalledListener =
            handshake
                ? new ServerSocket(0)
                : SSLContext.getDefault().getServerSocketFactory().createServerSocket(0);
        ServerSocket healthyListener = new ServerSocket(0)) {
      stalledListener.setSoTimeout(5000);
      healthyListener.setSoTimeout(5000);
      if (stalledListener instanceof SSLServerSocket) {
        ((SSLServerSocket) stalledListener).setEnabledProtocols(new String[] {"TLSv1.2"});
      }
      CompletableFuture<Void> stalledServer =
          serve(
              stalledListener,
              socket -> {
                peer.set(socket);
                if (handshake) {
                  // Wait for ClientHello so the client really has entered its TLS handshake.
                  assertTrue(socket.getInputStream().read() >= 0);
                } else {
                  ((SSLSocket) socket).startHandshake();
                  readRequest(socket);
                  DataOutputStream output = new DataOutputStream(socket.getOutputStream());
                  output.writeInt(8);
                  output.write(new byte[] {1, 2, 3});
                  output.flush();
                }
                ready.countDown();
                assertTrue(release.await(5, TimeUnit.SECONDS));
              });
      CompletableFuture<Void> healthyServer =
          serve(
              healthyListener,
              socket -> {
                readRequest(socket);
                DataOutputStream output = new DataOutputStream(socket.getOutputStream());
                output.writeInt(8);
                output.write("ABCDEFGH".getBytes(StandardCharsets.US_ASCII));
                output.flush();
              });
      TAsyncClientManager manager = new TAsyncClientManager();
      TNonblockingTransport stalled =
          TSSLTransportFactory.getNonblockingClientSocket(
              "localhost", stalledListener.getLocalPort());
      TNonblockingTransport healthy =
          new TNonblockingSocket("localhost", healthyListener.getLocalPort());
      try {
        CompletableFuture<byte[]> stalledResult = call(manager, stalled, 1000);
        assertTrue(ready.await(3, TimeUnit.SECONDS));
        CompletableFuture<byte[]> healthyResult = call(manager, healthy, 3000);
        assertArrayEquals(
            "ABCDEFGH".getBytes(StandardCharsets.US_ASCII), healthyResult.get(3, TimeUnit.SECONDS));
        ExecutionException error =
            assertThrows(ExecutionException.class, () -> stalledResult.get(2, TimeUnit.SECONDS));
        assertTrue(error.getCause() instanceof TimeoutException);
        healthyServer.get(2, TimeUnit.SECONDS);
      } finally {
        release.countDown();
        // Close the peer first so an old busy-reading implementation can leave read().
        if (peer.get() != null) peer.get().close();
        manager.stop();
        stalled.close();
        healthy.close();
      }
      stalledServer.get(2, TimeUnit.SECONDS);
    }
  }

  private static void readRequest(Socket socket) throws Exception {
    DataInputStream input = new DataInputStream(socket.getInputStream());
    int length = input.readInt();
    assertEquals(1, length);
    assertEquals(7, input.readUnsignedByte());
  }

  private static CompletableFuture<byte[]> call(
      TAsyncClientManager manager, TNonblockingTransport transport, long timeout) throws Exception {
    TBinaryProtocol.Factory factory = new TBinaryProtocol.Factory();
    TAsyncClient client = new TAsyncClient(factory, manager, transport, timeout) {};
    CompletableFuture<byte[]> result = new CompletableFuture<>();
    manager.call(
        new TAsyncMethodCall<byte[]>(
            client,
            factory,
            transport,
            new AsyncMethodCallback<byte[]>() {
              @Override
              public void onComplete(byte[] value) {
                result.complete(value);
              }

              @Override
              public void onError(Exception error) {
                result.completeExceptionally(error);
              }
            },
            false) {
          @Override
          protected void write_args(TProtocol protocol) throws TException {
            protocol.writeByte((byte) 7);
          }

          @Override
          protected byte[] getResult() {
            return getFrameBuffer().array();
          }
        });
    return result;
  }

  private interface Handler {
    void run(Socket socket) throws Exception;
  }

  private static CompletableFuture<Void> serve(ServerSocket listener, Handler handler) {
    CompletableFuture<Void> done = new CompletableFuture<>();
    Thread thread =
        new Thread(
            () -> {
              try (Socket socket = listener.accept()) {
                socket.setSoTimeout(5000);
                handler.run(socket);
                done.complete(null);
              } catch (Throwable error) {
                done.completeExceptionally(error);
              }
            },
            "async-ssl-progress-test-server");
    thread.setDaemon(true);
    thread.start();
    return done;
  }
}
