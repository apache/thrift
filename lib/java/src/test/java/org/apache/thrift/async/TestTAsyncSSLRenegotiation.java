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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.junit.jupiter.api.Test;

public class TestTAsyncSSLRenegotiation {
  @Test
  public void testTls12PeerRenegotiation() throws Exception {
    checkHandshake("TLSv1.2");
  }

  @Test
  public void testTls13KeyUpdate() throws Exception {
    checkHandshake("TLSv1.3");
  }

  private void checkHandshake(String protocol) throws Exception {
    SSLContext context = SSLContext.getDefault();
    assumeTrue(
        Arrays.asList(context.getSupportedSSLParameters().getProtocols()).contains(protocol));
    CountDownLatch release = new CountDownLatch(1);
    AtomicReference<Socket> peer = new AtomicReference<>();
    CompletableFuture<Void> serverDone = new CompletableFuture<>();
    byte[] expected = "ABCDEFGH".getBytes(StandardCharsets.US_ASCII);
    try (SSLServerSocket listener =
        (SSLServerSocket) context.getServerSocketFactory().createServerSocket(0)) {
      listener.setEnabledProtocols(new String[] {protocol});
      Thread server =
          new Thread(
              () -> {
                try (SSLSocket socket = (SSLSocket) listener.accept()) {
                  peer.set(socket);
                  socket.setSoTimeout(5000);
                  socket.startHandshake();
                  DataInputStream input = new DataInputStream(socket.getInputStream());
                  DataOutputStream output = new DataOutputStream(socket.getOutputStream());
                  for (int request = 0; request < 2; request++) {
                    assertEquals(1, input.readInt());
                    assertEquals(7, input.readUnsignedByte());
                    if (request == 0) socket.startHandshake();
                    output.writeInt(expected.length);
                    output.write(expected);
                    output.flush();
                  }
                  assertTrue(release.await(8, TimeUnit.SECONDS));
                  serverDone.complete(null);
                } catch (Throwable error) {
                  serverDone.completeExceptionally(error);
                }
              },
              "async-ssl-renegotiation-test-server");
      server.setDaemon(true);
      server.start();
      TAsyncClientManager manager = new TAsyncClientManager();
      TNonblockingTransport transport =
          TSSLTransportFactory.getNonblockingClientSocket("localhost", listener.getLocalPort());
      try {
        TBinaryProtocol.Factory factory = new TBinaryProtocol.Factory();
        TAsyncClient client = new TAsyncClient(factory, manager, transport, 3000) {};
        for (int request = 0; request < 2; request++) {
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
                protected void write_args(TProtocol p) throws TException {
                  p.writeByte((byte) 7);
                }

                @Override
                protected byte[] getResult() {
                  return getFrameBuffer().array();
                }
              });
          assertArrayEquals(expected, result.get(5, TimeUnit.SECONDS));
        }
        release.countDown();
        serverDone.get(2, TimeUnit.SECONDS);
      } finally {
        release.countDown();
        if (peer.get() != null) peer.get().close();
        manager.stop();
        transport.close();
        server.join(2000);
      }
    }
  }
}
