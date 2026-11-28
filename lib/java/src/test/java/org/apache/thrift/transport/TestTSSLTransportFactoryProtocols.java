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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;
import java.util.HashSet;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;
import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;
import org.junit.jupiter.params.provider.ValueSource;

public class TestTSSLTransportFactoryProtocols {
  private static final InetAddress LOOPBACK = InetAddress.getLoopbackAddress();

  @Test
  public void testUsesConfiguredServerProtocols() throws Exception {
    TSSLTransportParameters params = serverParameters("TLS");
    String[] protocols = {"TLSv1.2"};
    params.setEnabledProtocols(protocols);
    protocols[0] = "TLSv1.3";
    assertArrayEquals(new String[] {"TLSv1.2"}, enabledServerProtocols(params));

    params.setEnabledProtocols("TLSv1.3", "TLSv1.2");
    assertEquals(
        new HashSet<>(Arrays.asList("TLSv1.3", "TLSv1.2")),
        new HashSet<>(Arrays.asList(enabledServerProtocols(params))));
  }

  @Test
  public void testUsesContextDefaultsWhenServerProtocolsAreUnset() throws Exception {
    SSLContext context = SSLContext.getInstance("TLSv1.2");
    context.init(null, null, null);
    try (SSLServerSocket defaultSocket =
        (SSLServerSocket) context.getServerSocketFactory().createServerSocket()) {
      String[] defaultProtocols = defaultSocket.getEnabledProtocols();
      TSSLTransportParameters params = serverParameters("TLSv1.2");
      assertArrayEquals(defaultProtocols, enabledServerProtocols(params));

      params.setEnabledProtocols("TLSv1.2");
      params.setEnabledProtocols((String[]) null);
      assertArrayEquals(defaultProtocols, enabledServerProtocols(params));
    }
  }

  @Test
  public void testRejectsAnEmptyServerProtocolList() {
    TSSLTransportParameters params = new TSSLTransportParameters();
    assertThrows(IllegalArgumentException.class, params::setEnabledProtocols);
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = "unsupported-protocol")
  public void testClosesSocketWhenServerProtocolIsUnsupported(String rejectedProtocol)
      throws Exception {
    int port;
    try (ServerSocket socket = new ServerSocket(0, 1, LOOPBACK)) {
      port = socket.getLocalPort();
    }

    TSSLTransportParameters params = serverParameters("TLS");
    params.setEnabledProtocols("TLSv1.2", rejectedProtocol);
    TTransportException failure =
        assertThrows(
            TTransportException.class,
            () -> {
              try (TServerSocket socket =
                  TSSLTransportFactory.getServerSocket(port, 0, LOOPBACK, params)) {}
            });
    assertInstanceOf(IllegalArgumentException.class, failure.getCause());

    try (ServerSocket socket = new ServerSocket(port, 1, LOOPBACK)) {
      assertEquals(port, socket.getLocalPort());
    }
  }

  @Test
  public void testUsesConfiguredBlockingClientProtocols() throws Exception {
    TSSLTransportParameters params = clientParameters();
    params.setEnabledProtocols("TLSv1.2");
    try (ServerSocket listener = new ServerSocket(0, 1, LOOPBACK);
        TSocket client =
            TSSLTransportFactory.getClientSocket(
                LOOPBACK.getHostAddress(), listener.getLocalPort(), 0, params)) {
      SSLSocket socket = (SSLSocket) client.getSocket();
      assertArrayEquals(new String[] {"TLSv1.2"}, socket.getEnabledProtocols());
      assertEquals("HTTPS", socket.getSSLParameters().getEndpointIdentificationAlgorithm());
    }
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = "unsupported-protocol")
  public void testClosesBlockingClientSocketWhenProtocolIsUnsupported(String rejectedProtocol)
      throws Exception {
    TSSLTransportParameters params = clientParameters();
    params.setEnabledProtocols("TLSv1.2", rejectedProtocol);
    try (ServerSocket listener = new ServerSocket(0, 1, LOOPBACK)) {
      listener.setSoTimeout(5000);
      TTransportException failure =
          assertThrows(
              TTransportException.class,
              () ->
                  TSSLTransportFactory.getClientSocket(
                          LOOPBACK.getHostAddress(), listener.getLocalPort(), 0, params)
                      .close());
      assertInstanceOf(IllegalArgumentException.class, failure.getCause());

      try (Socket peer = listener.accept()) {
        peer.setSoTimeout(5000);
        assertEquals(-1, peer.getInputStream().read());
      }
    }
  }

  @ParameterizedTest
  @NullSource
  @ValueSource(strings = "unsupported-protocol")
  public void testRejectsUnsupportedNonblockingClientProtocol(String rejectedProtocol) {
    TSSLTransportParameters params = clientParameters();
    params.setEnabledProtocols("TLSv1.2", rejectedProtocol);
    TTransportException failure =
        assertThrows(
            TTransportException.class,
            () ->
                TSSLTransportFactory.getNonblockingClientSocket(
                        LOOPBACK.getHostAddress(), 0, 0, params)
                    .close());
    assertInstanceOf(IllegalArgumentException.class, failure.getCause());
  }

  private static TSSLTransportParameters clientParameters() {
    TSSLTransportParameters params = new TSSLTransportParameters();
    params.setTrustStore(
        System.getProperty("javax.net.ssl.trustStore"),
        System.getProperty("javax.net.ssl.trustStorePassword"));
    return params;
  }

  private static TSSLTransportParameters serverParameters(String algorithm) {
    TSSLTransportParameters params = new TSSLTransportParameters(algorithm, null);
    params.setKeyStore(
        System.getProperty("javax.net.ssl.keyStore"),
        System.getProperty("javax.net.ssl.keyStorePassword"));
    return params;
  }

  private static String[] enabledServerProtocols(TSSLTransportParameters params)
      throws TTransportException {
    try (TServerSocket transport = TSSLTransportFactory.getServerSocket(0, 0, LOOPBACK, params)) {
      SSLServerSocket socket = (SSLServerSocket) transport.getServerSocket();
      return socket.getEnabledProtocols();
    }
  }
}
