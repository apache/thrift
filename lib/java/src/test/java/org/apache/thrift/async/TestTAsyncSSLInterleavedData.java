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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLSession;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingSSLSocket;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class TestTAsyncSSLInterleavedData {
  @ParameterizedTest
  @CsvSource({"false,false", "true,false", "false,true", "true,true"})
  public void testBufferedResponseCompletesAfterWriteHandshake(
      boolean duringBody, boolean backpressure) throws Exception {
    byte[] expected = new byte[20];
    for (int i = 0; i < expected.length; i++) expected[i] = (byte) (i + 1);
    ByteBuffer response = ByteBuffer.allocate(24).putInt(expected.length).put(expected);
    response.flip();
    ByteBuffer records = ByteBuffer.wrap(new byte[] {1, 2, 3});
    ByteArrayOutputStream sent = new ByteArrayOutputStream();
    AtomicReference<HandshakeStatus> status =
        new AtomicReference<>(
            duringBody ? HandshakeStatus.NOT_HANDSHAKING : HandshakeStatus.NEED_UNWRAP);
    SSLContext context = mock(SSLContext.class);
    SSLEngine engine = mock(SSLEngine.class);
    SSLSession session = mock(SSLSession.class);
    when(context.createSSLEngine(any(String.class), anyInt())).thenReturn(engine);
    when(engine.getSSLParameters()).thenReturn(new SSLParameters());
    when(engine.getSession()).thenReturn(session);
    when(session.getApplicationBufferSize()).thenReturn(16);
    when(session.getPacketBufferSize()).thenReturn(64);
    when(engine.getHandshakeStatus()).thenAnswer(call -> status.get());
    // Script two application records followed by the handshake completion record.
    // The first record fills the plaintext buffer exactly.
    when(engine.unwrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              if (!input.hasRemaining())
                return new SSLEngineResult(Status.BUFFER_UNDERFLOW, status.get(), 0, 0);
              if (input.get() == 3) {
                status.set(HandshakeStatus.NOT_HANDSHAKING);
                return new SSLEngineResult(Status.OK, status.get(), 1, 0);
              }
              int size = Math.min(16, response.remaining());
              for (int i = 0; i < size; i++) output.put(response.get());
              return new SSLEngineResult(Status.OK, status.get(), 1, size);
            });
    when(engine.wrap(any(ByteBuffer.class), any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              assertEquals(HandshakeStatus.NOT_HANDSHAKING, status.get());
              ByteBuffer input = call.getArgument(0), output = call.getArgument(1);
              int size = input.remaining();
              output.put(input);
              if (duringBody && size == 4) status.set(HandshakeStatus.NEED_UNWRAP);
              return new SSLEngineResult(Status.OK, status.get(), size, size);
            });
    AtomicReference<ConnectedSocket> connected = new AtomicReference<>();
    SocketChannel io = mock(SocketChannel.class);
    when(io.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              if (!records.hasRemaining())
                throw new IOException("No further network input expected");
              int size = records.remaining();
              ((ByteBuffer) call.getArgument(0)).put(records);
              return size;
            });
    when(io.write(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer data = call.getArgument(0);
              int size = data.remaining();
              while (data.hasRemaining()) sent.write(data.get());
              if (backpressure && sent.size() == 5) {
                connected.get().fillOutputUntilBlocked();
              }
              return size;
            });
    try (ServerSocket listener = createListener();
        ConnectedSocket transport = new ConnectedSocket(listener.getLocalPort(), context, io)) {
      connected.set(transport);
      try (Socket peer = listener.accept()) {
        // The peer sends nothing and never reads. In the backpressure cases, the final request
        // write fills the real channel, so neither OP_READ nor OP_WRITE can deliver the response.
        TAsyncClientManager manager = new TAsyncClientManager();
        transport.setMaxMessageSize(response.capacity());
        try {
          TBinaryProtocol.Factory factory = new TBinaryProtocol.Factory();
          TAsyncClient client = new TAsyncClient(factory, manager, transport, 2000) {};
          for (int request = 0; request < (backpressure ? 1 : 2); request++) {
            response.rewind();
            records.rewind();
            sent.reset();
            status.set(duringBody ? HandshakeStatus.NOT_HANDSHAKING : HandshakeStatus.NEED_UNWRAP);
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
            assertArrayEquals(expected, result.get(5, TimeUnit.SECONDS));
            assertArrayEquals(new byte[] {0, 0, 0, 1, 7}, sent.toByteArray());
            assertEquals(0, response.remaining());
          }
        } finally {
          manager.stop();
        }
      }
    }
  }

  private static ServerSocket createListener() throws Exception {
    ServerSocket listener = new ServerSocket(0);
    listener.setReceiveBufferSize(1024);
    listener.setSoTimeout(5000);
    return listener;
  }

  private static class ConnectedSocket extends TNonblockingSSLSocket {
    private final SocketChannel io;

    ConnectedSocket(int port, SSLContext context, SocketChannel io) throws Exception {
      super("localhost", port, 0, context);
      this.io = io;
      SocketChannel tcp = super.getSocketChannel();
      tcp.socket().setSendBufferSize(1024);
      tcp.configureBlocking(true);
      tcp.socket().connect(new InetSocketAddress("localhost", port), 2000);
      tcp.configureBlocking(false);
    }

    void fillOutputUntilBlocked() throws Exception {
      SocketChannel tcp = super.getSocketChannel();
      ByteBuffer filler = ByteBuffer.allocate(64 * 1024);
      long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
      try (Selector probe = Selector.open()) {
        tcp.register(probe, SelectionKey.OP_WRITE);
        while (true) {
          assertTrue(System.nanoTime() < deadline, "Channel did not reach backpressure");
          filler.clear();
          if (tcp.write(filler) > 0) continue;
          probe.selectedKeys().clear();
          if (probe.select(100) == 0) return;
        }
      }
    }

    @Override
    public boolean isOpen() {
      return super.getSocketChannel().isConnected();
    }

    @Override
    public SocketChannel getSocketChannel() {
      return io;
    }
  }
}
