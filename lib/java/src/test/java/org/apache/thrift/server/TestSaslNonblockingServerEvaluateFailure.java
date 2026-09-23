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
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.nio.charset.StandardCharsets;
import javax.security.sasl.SaslException;
import javax.security.sasl.SaslServer;
import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.sasl.NegotiationStatus;
import org.apache.thrift.transport.sasl.ServerSaslPeer;
import org.apache.thrift.transport.sasl.TSaslServerFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

/**
 * Verifies that {@link TSaslNonblockingServer} closes a connection whose SASL response evaluation
 * fails with an unchecked exception, and that the server keeps serving afterwards.
 */
public class TestSaslNonblockingServerEvaluateFailure {

  private static final String MECHANISM = "TEST";

  private static final byte STATUS_START = NegotiationStatus.START.getValue();
  private static final byte STATUS_OK = NegotiationStatus.OK.getValue();
  private static final byte STATUS_ERROR = NegotiationStatus.ERROR.getValue();
  // A status byte that is not part of the negotiation protocol.
  private static final byte STATUS_INVALID = (byte) 0xff;

  private static final int CONNECT_TIMEOUT_MS = 2000;
  private static final int READ_TIMEOUT_MS = 4000;

  private TSaslNonblockingServer server;
  private int port;

  @AfterEach
  public void tearDown() throws Exception {
    if (server != null) {
      server.shutdown();
      server = null;
    }
  }

  /** A SaslServer whose response evaluation always throws an unchecked exception. */
  private static class ThrowingSaslServer implements SaslServer {
    @Override
    public String getMechanismName() {
      return MECHANISM;
    }

    @Override
    public byte[] evaluateResponse(byte[] response) throws SaslException {
      throw new IllegalStateException("Response evaluation is not supported by this server.");
    }

    @Override
    public boolean isComplete() {
      return false;
    }

    @Override
    public byte[] unwrap(byte[] incoming, int offset, int len) {
      throw new UnsupportedOperationException();
    }

    @Override
    public byte[] wrap(byte[] outgoing, int offset, int len) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Object getNegotiatedProperty(String propName) {
      return null;
    }

    @Override
    public String getAuthorizationID() {
      return null;
    }

    @Override
    public void dispose() {}
  }

  private static class NoopProcessor implements TProcessor {
    @Override
    public void process(TProtocol in, TProtocol out) throws TException {}
  }

  private void startServer() throws Exception {
    TNonblockingServerSocket serverSocket =
        new TNonblockingServerSocket(
            new TNonblockingServerSocket.NonblockingAbstractServerSocketArgs().port(0));
    port = serverSocket.getPort();
    TSaslServerFactory saslServerFactory =
        new TSaslServerFactory() {
          @Override
          public ServerSaslPeer getSaslPeer(String mechanism) {
            return new ServerSaslPeer(new ThrowingSaslServer());
          }
        };
    TSaslNonblockingServer.Args args =
        new TSaslNonblockingServer.Args(serverSocket)
            .networkThreads(1)
            .saslThreads(1)
            .processor(new NoopProcessor())
            .saslServerFactory(saslServerFactory);
    server = new TSaslNonblockingServer(args);
    server.serve();
  }

  @Test
  public void evaluationFailureClosesConnection() throws Exception {
    startServer();

    // Connection A drives the negotiation up to response evaluation, which throws in the stub.
    Socket crafted = new Socket();
    crafted.connect(new InetSocketAddress("localhost", port), CONNECT_TIMEOUT_MS);
    crafted.setSoTimeout(READ_TIMEOUT_MS);
    OutputStream craftedOut = crafted.getOutputStream();
    writeFrame(craftedOut, STATUS_START, MECHANISM.getBytes(StandardCharsets.UTF_8));
    writeFrame(craftedOut, STATUS_OK, new byte[] {1, 2, 3});

    // Connection B: a fresh connection is still answered with a failure frame and closed, which
    // shows the single network and authentication threads keep working.
    byte[] failureFrame = negotiateInvalidStatus();
    assertEquals(38, failureFrame.length, "Unexpected failure frame size");
    assertEquals(STATUS_ERROR, failureFrame[0], "Failure frame should carry the ERROR status");
    int payloadSize =
        ((failureFrame[1] & 0xff) << 24)
            | ((failureFrame[2] & 0xff) << 16)
            | ((failureFrame[3] & 0xff) << 8)
            | (failureFrame[4] & 0xff);
    assertEquals(33, payloadSize, "Unexpected failure payload size");
    String failureMessage = new String(failureFrame, 5, payloadSize, StandardCharsets.UTF_8);
    assertEquals("PROTOCOL_ERROR: Invalid status -1", failureMessage);

    assertTrue(server.isServing(), "Server should keep serving after the evaluation failure");

    // Connection A must now be closed by the server. Before the fix the connection stays open with
    // no bytes sent, so this read blocks until the socket timeout.
    int firstByte;
    try {
      firstByte = crafted.getInputStream().read();
    } catch (SocketException reset) {
      // A reset surfaces the server-side close just like an orderly EOF does.
      firstByte = -1;
    } catch (SocketTimeoutException stillOpen) {
      crafted.close();
      fail(
          "Connection A stayed open (0 bytes received): the server did not close it after the "
              + "response evaluation failure.");
      return;
    }
    assertEquals(
        -1,
        firstByte,
        "Connection A should be closed (EOF) after the response evaluation failure, but a byte was"
            + " received.");
    crafted.close();
  }

  /**
   * Opens a fresh connection, sends a header with an invalid status byte, and returns everything
   * the server writes back before it closes the connection.
   */
  private byte[] negotiateInvalidStatus() throws IOException {
    try (Socket fresh = new Socket()) {
      fresh.connect(new InetSocketAddress("localhost", port), CONNECT_TIMEOUT_MS);
      fresh.setSoTimeout(READ_TIMEOUT_MS);
      OutputStream out = fresh.getOutputStream();
      // Status byte followed by a zero payload length.
      out.write(new byte[] {STATUS_INVALID, 0, 0, 0, 0});
      out.flush();
      return readUntilEof(fresh.getInputStream());
    }
  }

  private static void writeFrame(OutputStream out, byte status, byte[] payload) throws IOException {
    byte[] frame = new byte[5 + payload.length];
    frame[0] = status;
    frame[1] = (byte) (payload.length >>> 24);
    frame[2] = (byte) (payload.length >>> 16);
    frame[3] = (byte) (payload.length >>> 8);
    frame[4] = (byte) payload.length;
    System.arraycopy(payload, 0, frame, 5, payload.length);
    out.write(frame);
    out.flush();
  }

  private static byte[] readUntilEof(InputStream in) throws IOException {
    ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    byte[] chunk = new byte[256];
    while (true) {
      int read;
      try {
        read = in.read(chunk);
      } catch (SocketException reset) {
        break;
      }
      if (read < 0) {
        break;
      }
      buffer.write(chunk, 0, read);
    }
    return buffer.toByteArray();
  }
}
