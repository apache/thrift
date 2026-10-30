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

package org.apache.thrift.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.transport.TServerSocket.ServerSocketTransportArgs;
import org.apache.thrift.transport.layered.TFramedTransport;
import org.junit.jupiter.api.Test;

/** An accepted socket has the limits of the TServerSocket from the first message on. */
public class TestTServerSocket {

  private static final int MAX_FRAME_SIZE = 1000;
  private static final int MAX_MESSAGE_SIZE = 4096;

  private static ServerSocketTransportArgs limits() {
    return new ServerSocketTransportArgs()
        .maxFrameSize(MAX_FRAME_SIZE)
        .maxMessageSize(MAX_MESSAGE_SIZE);
  }

  /** A client connected to a server socket, and the socket the server accepted for it. */
  private static final class Connection implements AutoCloseable {
    private final TServerSocket server;
    private final Socket client;
    private final TSocket accepted;

    Connection(ServerSocketTransportArgs args) throws IOException, TTransportException {
      InetAddress loopback = InetAddress.getLoopbackAddress();
      server = new TServerSocket(args.serverSocket(new ServerSocket(0, 1, loopback)));
      server.listen();
      client = new Socket(loopback, server.getServerSocket().getLocalPort());
      accepted = server.accept();
    }

    /** A four-byte size and that many bytes: a string to a protocol, a frame to a transport. */
    void send(int size) throws IOException {
      DataOutputStream out = new DataOutputStream(client.getOutputStream());
      out.writeInt(size);
      out.write(new byte[size]);
      out.flush();
    }

    @Override
    public void close() throws IOException {
      accepted.close();
      client.close();
      server.close();
    }
  }

  private static int readFrame(TSocket accepted, int size) throws TTransportException {
    return new TFramedTransport.Factory().getTransport(accepted).readAll(new byte[size], 0, size);
  }

  @Test
  public void testAcceptedSocketHasTheLimits() throws Exception {
    try (Connection connection = new Connection(limits())) {
      TConfiguration config = connection.accepted.getConfiguration();
      assertEquals(MAX_FRAME_SIZE, config.getMaxFrameSize());
      assertEquals(MAX_MESSAGE_SIZE, config.getMaxMessageSize());
    }
  }

  @Test
  public void testAcceptedSocketHasTheDefaultsWithoutLimits() throws Exception {
    try (Connection connection = new Connection(new ServerSocketTransportArgs())) {
      TConfiguration config = connection.accepted.getConfiguration();
      assertEquals(TConfiguration.DEFAULT_MAX_FRAME_SIZE, config.getMaxFrameSize());
      assertEquals(TConfiguration.DEFAULT_MAX_MESSAGE_SIZE, config.getMaxMessageSize());
    }
  }

  @Test
  public void testFirstMessageMayHaveTheMessageSize() throws Exception {
    try (Connection connection = new Connection(limits())) {
      connection.send(MAX_MESSAGE_SIZE);
      int read = new TBinaryProtocol(connection.accepted).readBinary().remaining();
      assertEquals(MAX_MESSAGE_SIZE, read);
    }
  }

  @Test
  public void testFirstMessageIsHeldToTheMessageSize() throws Exception {
    try (Connection connection = new Connection(limits())) {
      connection.send(MAX_MESSAGE_SIZE + 1);
      TTransportException e =
          assertThrows(
              TTransportException.class,
              () -> new TBinaryProtocol(connection.accepted).readBinary());
      assertEquals(TTransportException.MESSAGE_SIZE_LIMIT, e.getType());
    }
  }

  @Test
  public void testFrameMayHaveTheFrameSize() throws Exception {
    try (Connection connection = new Connection(limits())) {
      connection.send(MAX_FRAME_SIZE);
      assertEquals(MAX_FRAME_SIZE, readFrame(connection.accepted, MAX_FRAME_SIZE));
    }
  }

  @Test
  public void testFrameIsHeldToTheFrameSize() throws Exception {
    try (Connection connection = new Connection(limits())) {
      connection.send(MAX_FRAME_SIZE + 1);
      TTransportException e =
          assertThrows(
              TTransportException.class, () -> readFrame(connection.accepted, MAX_FRAME_SIZE + 1));
      assertEquals(TTransportException.CORRUPTED_DATA, e.getType());
    }
  }
}
