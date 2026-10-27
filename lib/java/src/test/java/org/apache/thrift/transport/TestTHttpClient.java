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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;

import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.hc.client5.http.HttpRoute;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.cookie.StandardCookieSpec;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManagerBuilder;
import org.apache.hc.client5.http.io.ConnectionEndpoint;
import org.apache.hc.client5.http.protocol.HttpClientContext;
import org.apache.hc.core5.http.ConnectionRequestTimeoutException;
import org.apache.hc.core5.http.HttpHost;
import org.apache.hc.core5.util.TimeValue;
import org.apache.hc.core5.util.Timeout;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class TestTHttpClient {

  private static final byte[] REQUEST = "request".getBytes(StandardCharsets.US_ASCII);
  private static final byte[] REPLY = "reply".getBytes(StandardCharsets.US_ASCII);

  // client defaults that differ from RequestConfig.DEFAULT in most fields
  private static final RequestConfig CLIENT_DEFAULTS =
      RequestConfig.custom()
          .setCookieSpec(StandardCookieSpec.IGNORE)
          .setAuthenticationEnabled(false)
          .setTargetPreferredAuthSchemes(Collections.singletonList("Basic"))
          .setProxyPreferredAuthSchemes(Collections.singletonList("Basic"))
          .setConnectionRequestTimeout(Timeout.ofSeconds(7))
          .setResponseTimeout(Timeout.ofSeconds(9))
          .setConnectionKeepAlive(TimeValue.ofSeconds(11))
          .setContentCompressionEnabled(false)
          .setHardCancellationEnabled(false)
          .build();

  private final List<String> acceptEncodings = new CopyOnWriteArrayList<>();
  private final AtomicReference<RequestConfig> effective = new AtomicReference<>();
  private HttpServer server;

  @BeforeEach
  public void startServer() throws IOException {
    server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
    server.createContext(
        "/",
        exchange -> {
          try (InputStream body = exchange.getRequestBody()) {
            byte[] buffer = new byte[1024];
            while (body.read(buffer) != -1) {
              // drain the request
            }
          }
          String acceptEncoding = exchange.getRequestHeaders().getFirst("Accept-Encoding");
          acceptEncodings.add(acceptEncoding == null ? "" : acceptEncoding);
          exchange.sendResponseHeaders(200, REPLY.length);
          try (OutputStream body = exchange.getResponseBody()) {
            body.write(REPLY);
          }
        });
    server.start();
  }

  @AfterEach
  public void stopServer() {
    server.stop(0);
  }

  private THttpClient transport(CloseableHttpClient client) throws TTransportException {
    return new THttpClient("http://localhost:" + server.getAddress().getPort() + "/", client);
  }

  private static byte[] roundTrip(THttpClient transport) throws TTransportException {
    transport.write(REQUEST);
    transport.flush();
    byte[] reply = new byte[REPLY.length];
    transport.readAll(reply, 0, reply.length);
    return reply;
  }

  private static CloseableHttpClient clientWithoutContentCompression() {
    return HttpClients.custom()
        .setDefaultRequestConfig(RequestConfig.custom().setContentCompressionEnabled(false).build())
        .build();
  }

  /** A client with CLIENT_DEFAULTS that records the request configuration in effect. */
  private CloseableHttpClient recordingClient() {
    return HttpClients.custom()
        .setDefaultRequestConfig(CLIENT_DEFAULTS)
        .addRequestInterceptorFirst(
            (request, entity, context) ->
                effective.set(HttpClientContext.adapt(context).getRequestConfig()))
        .build();
  }

  @Test
  public void testContentCompressionIsRequestedByDefault() throws Exception {
    try (CloseableHttpClient client = HttpClients.createDefault()) {
      assertArrayEquals(REPLY, roundTrip(transport(client)));
    }
    assertEquals(1, acceptEncodings.size());
    assertFalse(acceptEncodings.get(0).isEmpty());
  }

  @Test
  public void testDefaultRequestConfigOfClientIsUsed() throws Exception {
    try (CloseableHttpClient client = clientWithoutContentCompression()) {
      assertArrayEquals(REPLY, roundTrip(transport(client)));
    }
    assertEquals(Collections.singletonList(""), acceptEncodings);
  }

  @Test
  public void testDefaultRequestConfigOfClientIsUsedWithConnectTimeout() throws Exception {
    try (CloseableHttpClient client = clientWithoutContentCompression()) {
      THttpClient transport = transport(client);
      transport.setConnectTimeout(10000);
      assertArrayEquals(REPLY, roundTrip(transport));
    }
    assertEquals(Collections.singletonList(""), acceptEncodings);
  }

  @Test
  public void testRequestConfigInEffectIsTheClientDefault() throws Exception {
    try (CloseableHttpClient client = recordingClient()) {
      assertArrayEquals(REPLY, roundTrip(transport(client)));
    }
    // RequestConfig has no equals(); its toString() lists every field
    assertEquals(CLIENT_DEFAULTS.toString(), String.valueOf(effective.get()));
  }

  @Test
  public void testConnectTimeoutIsAddedToClientDefault() throws Exception {
    try (CloseableHttpClient client = recordingClient()) {
      THttpClient transport = transport(client);
      transport.setConnectTimeout(10000);
      assertArrayEquals(REPLY, roundTrip(transport));
    }
    RequestConfig expected =
        RequestConfig.copy(CLIENT_DEFAULTS)
            .setConnectionRequestTimeout(Timeout.ofMilliseconds(10000))
            .build();
    assertEquals(expected.toString(), String.valueOf(effective.get()));
  }

  @Test
  public void testConnectTimeoutIsAppliedWithMinimalHttpClient() throws Exception {
    PoolingHttpClientConnectionManager pool =
        PoolingHttpClientConnectionManagerBuilder.create()
            .setMaxConnTotal(1)
            .setMaxConnPerRoute(1)
            .build();
    try (CloseableHttpClient client = HttpClients.createMinimal(pool)) {
      HttpRoute route =
          new HttpRoute(new HttpHost("http", "localhost", server.getAddress().getPort()));
      // hold the only connection of the pool, so that the request has to wait for one
      ConnectionEndpoint held =
          pool.lease("held", route, Timeout.ofSeconds(5), null).get(Timeout.ofSeconds(5));
      try {
        THttpClient transport = transport(client);
        transport.setConnectTimeout(200);
        transport.write(REQUEST);
        assertTimeoutPreemptively(
            Duration.ofSeconds(5),
            () -> {
              TTransportException e = assertThrows(TTransportException.class, transport::flush);
              assertInstanceOf(ConnectionRequestTimeoutException.class, e.getCause());
            });
      } finally {
        pool.release(held, null, TimeValue.ZERO_MILLISECONDS);
      }
    }
  }
}
