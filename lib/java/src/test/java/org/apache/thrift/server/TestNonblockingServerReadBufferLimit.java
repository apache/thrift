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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.DataInputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;
import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/** Frames that do not fit into the read buffer memory limit of the nonblocking servers. */
public class TestNonblockingServerReadBufferLimit {

  private static final String HOST = "localhost";
  private static final int TIMEOUT_MS = 5000;
  private static final int MAX_READ_BUFFER_BYTES = 1024;
  // One frame of this size fits into the read buffer memory limit, two do not.
  private static final int FRAME_SIZE = 600;
  private static final int ANSWER = 4242;
  // A frame starting with this byte makes the processor fail.
  private static final byte FAIL = 1;

  enum ServerType {
    NONBLOCKING,
    HSHA,
    THREADED_SELECTOR
  }

  /** Counts the reads of a server side connection. */
  private static class CountingSocket extends TNonblockingSocket {
    final AtomicInteger reads = new AtomicInteger();

    CountingSocket(SocketChannel channel) throws IOException, TTransportException {
      super(channel);
    }

    @Override
    public int read(ByteBuffer buffer) throws TTransportException {
      reads.incrementAndGet();
      return super.read(buffer);
    }
  }

  /** Hands out counting connections, found by the port of the client. */
  private static class CountingServerSocket extends TNonblockingServerSocket {
    final Map<Integer, CountingSocket> accepted = new ConcurrentHashMap<>();

    CountingServerSocket() throws TTransportException {
      super(new NonblockingAbstractServerSocketArgs().port(0));
    }

    @Override
    public TNonblockingSocket accept() throws TTransportException {
      TNonblockingSocket socket = super.accept();
      if (socket == null) {
        return null;
      }
      try {
        CountingSocket counting = new CountingSocket(socket.getSocketChannel());
        counting.setMaxFrameSize(socket.getMaxFrameSize());
        accepted.put(socket.getSocketChannel().socket().getPort(), counting);
        return counting;
      } catch (IOException e) {
        throw new TTransportException(e);
      }
    }

    CountingSocket serverSideOf(Socket client) throws InterruptedException {
      waitFor(() -> accepted.containsKey(client.getLocalPort()), "connection accepted");
      return accepted.get(client.getLocalPort());
    }
  }

  @ParameterizedTest
  @EnumSource(ServerType.class)
  public void testDeferredFrameIsReadOnceMemoryIsReleased(ServerType type) throws Exception {
    CountingServerSocket serverSocket = new CountingServerSocket();
    AbstractNonblockingServer server = newServer(type, serverSocket);
    Thread serving = serve(server);
    try (Socket first = connect(serverSocket);
        Socket second = connect(serverSocket)) {
      // The first frame fits and holds its read buffer memory while it arrives.
      send(first, header(FRAME_SIZE), new byte[100]);
      waitFor(() -> server.readBufferBytesAllocated.get() == FRAME_SIZE + 4, "first frame read");

      // The second frame has to wait until the first one releases its memory.
      send(second, header(FRAME_SIZE), new byte[1]);
      CountingSocket secondServerSide = serverSocket.serverSideOf(second);
      waitFor(() -> secondServerSide.reads.get() > 0, "second frame size read");
      Thread.sleep(100);
      int readsBefore = secondServerSide.reads.get();
      Thread.sleep(1000);
      int readsWhileWaiting = secondServerSide.reads.get() - readsBefore;
      assertTrue(
          readsWhileWaiting <= 5,
          "reads of the waiting connection within 1 s: " + readsWhileWaiting);
      assertEquals(FRAME_SIZE + 4, server.readBufferBytesAllocated.get());

      send(first, new byte[FRAME_SIZE - 100]);
      assertEquals(ANSWER, readAnswer(first));
      send(second, new byte[FRAME_SIZE - 1]);
      assertEquals(ANSWER, readAnswer(second));
      waitFor(() -> server.readBufferBytesAllocated.get() == 0, "read buffer memory released");
    } finally {
      stop(server, serving);
    }
  }

  @ParameterizedTest
  @EnumSource(ServerType.class)
  public void testFrameLargerThanReadBufferLimitClosesConnection(ServerType type) throws Exception {
    CountingServerSocket serverSocket = new CountingServerSocket();
    AbstractNonblockingServer server = newServer(type, serverSocket);
    Thread serving = serve(server);
    try {
      try (Socket tooLarge = connect(serverSocket)) {
        send(tooLarge, header(MAX_READ_BUFFER_BYTES + 1), new byte[10]);
        assertClosedByServer(tooLarge);
      }
      assertEquals(0, server.readBufferBytesAllocated.get());
      try (Socket other = connect(serverSocket)) {
        send(other, header(FRAME_SIZE), new byte[FRAME_SIZE]);
        assertEquals(ANSWER, readAnswer(other));
      }
    } finally {
      stop(server, serving);
    }
  }

  @ParameterizedTest
  @EnumSource(ServerType.class)
  public void testDeferredFrameIsReadAfterFailedInvocation(ServerType type) throws Exception {
    CountingServerSocket serverSocket = new CountingServerSocket();
    AbstractNonblockingServer server = newServer(type, serverSocket);
    Thread serving = serve(server);
    try (Socket failing = connect(serverSocket);
        Socket second = connect(serverSocket)) {
      byte[] failingFrame = new byte[FRAME_SIZE];
      failingFrame[0] = FAIL;
      send(failing, header(FRAME_SIZE));
      sendPart(failing, failingFrame, 0, 100);
      waitFor(() -> server.readBufferBytesAllocated.get() == FRAME_SIZE + 4, "first frame read");
      send(second, header(FRAME_SIZE), new byte[1]);
      CountingSocket secondServerSide = serverSocket.serverSideOf(second);
      waitFor(() -> secondServerSide.reads.get() > 0, "second frame size read");

      // Closing the failed connection releases its memory for the second frame.
      sendPart(failing, failingFrame, 100, FRAME_SIZE - 100);
      assertClosedByServer(failing);
      send(second, new byte[FRAME_SIZE - 1]);
      assertEquals(ANSWER, readAnswer(second));
      waitFor(() -> server.readBufferBytesAllocated.get() == 0, "read buffer memory released");
    } finally {
      stop(server, serving);
    }
  }

  @ParameterizedTest
  @EnumSource(ServerType.class)
  public void testDeferredConnectionClosedByClientIsCleanedUp(ServerType type) throws Exception {
    CountingServerSocket serverSocket = new CountingServerSocket();
    AbstractNonblockingServer server = newServer(type, serverSocket);
    Thread serving = serve(server);
    try (Socket first = connect(serverSocket)) {
      send(first, header(FRAME_SIZE), new byte[100]);
      waitFor(() -> server.readBufferBytesAllocated.get() == FRAME_SIZE + 4, "first frame read");

      CountingSocket secondServerSide;
      try (Socket second = connect(serverSocket)) {
        send(second, header(FRAME_SIZE), new byte[1]);
        secondServerSide = serverSocket.serverSideOf(second);
        waitFor(() -> secondServerSide.reads.get() > 0, "second frame size read");
      }

      send(first, new byte[FRAME_SIZE - 100]);
      assertEquals(ANSWER, readAnswer(first));
      waitFor(() -> !secondServerSide.isOpen(), "closed connection cleaned up");
      waitFor(() -> server.readBufferBytesAllocated.get() == 0, "read buffer memory released");
    } finally {
      stop(server, serving);
    }
  }

  private static AbstractNonblockingServer newServer(
      ServerType type, TNonblockingServerSocket serverSocket) {
    TProcessor processor =
        (in, out) -> {
          if (in.readByte() == FAIL) {
            throw new TException("test failure");
          }
          out.writeI32(ANSWER);
          out.getTransport().flush();
        };
    switch (type) {
      case NONBLOCKING:
        TNonblockingServer.Args nonblocking =
            new TNonblockingServer.Args(serverSocket).processor(processor);
        nonblocking.maxReadBufferBytes = MAX_READ_BUFFER_BYTES;
        return new TNonblockingServer(nonblocking);
      case HSHA:
        THsHaServer.Args hsha = new THsHaServer.Args(serverSocket).processor(processor);
        hsha.maxReadBufferBytes = MAX_READ_BUFFER_BYTES;
        return new THsHaServer(hsha);
      default:
        TThreadedSelectorServer.Args threaded =
            new TThreadedSelectorServer.Args(serverSocket).processor(processor);
        threaded.maxReadBufferBytes = MAX_READ_BUFFER_BYTES;
        return new TThreadedSelectorServer(threaded);
    }
  }

  private static Thread serve(AbstractNonblockingServer server) throws InterruptedException {
    Thread serving = new Thread(server::serve);
    serving.start();
    waitFor(server::isServing, "server serving");
    return serving;
  }

  private static void stop(AbstractNonblockingServer server, Thread serving)
      throws InterruptedException {
    server.stop();
    serving.join(TIMEOUT_MS);
    assertFalse(serving.isAlive(), "server stopped");
  }

  private static Socket connect(TNonblockingServerSocket serverSocket) throws IOException {
    Socket socket = new Socket();
    socket.connect(new InetSocketAddress(HOST, serverSocket.getPort()), TIMEOUT_MS);
    socket.setSoTimeout(TIMEOUT_MS);
    return socket;
  }

  private static byte[] header(int frameSize) {
    return ByteBuffer.allocate(4).putInt(frameSize).array();
  }

  private static void send(Socket socket, byte[]... parts) throws IOException {
    for (byte[] part : parts) {
      socket.getOutputStream().write(part);
    }
    socket.getOutputStream().flush();
  }

  private static void sendPart(Socket socket, byte[] data, int offset, int length)
      throws IOException {
    socket.getOutputStream().write(data, offset, length);
    socket.getOutputStream().flush();
  }

  private static int readAnswer(Socket socket) throws IOException {
    DataInputStream in = new DataInputStream(socket.getInputStream());
    assertEquals(4, in.readInt());
    return in.readInt();
  }

  private static void assertClosedByServer(Socket socket) {
    try {
      assertEquals(-1, socket.getInputStream().read());
    } catch (SocketTimeoutException e) {
      throw new AssertionError("connection not closed within " + TIMEOUT_MS + " ms", e);
    } catch (IOException e) {
      // reset by the server, as unread data was pending: closed as well
    }
  }

  private static void waitFor(BooleanSupplier condition, String what) throws InterruptedException {
    long deadline = System.currentTimeMillis() + TIMEOUT_MS;
    while (!condition.getAsBoolean()) {
      if (System.currentTimeMillis() > deadline) {
        throw new AssertionError("timed out waiting for: " + what);
      }
      Thread.sleep(10);
    }
  }
}
