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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.apache.hc.client5.http.classic.HttpClient;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.config.ConnectionConfig;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.core5.http.HttpHost;
import org.apache.hc.core5.http.io.entity.ByteArrayEntity;
import org.apache.hc.core5.util.Timeout;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.THttpClientResponseHandler;

/**
 * HTTP implementation of the TTransport interface. Used for working with a Thrift web services
 * implementation (using for example TServlet).
 *
 * <p>This class offers two implementations of the HTTP transport. One uses HttpURLConnection
 * instances, the other HttpClient from Apache Http Components. The chosen implementation depends on
 * the constructor used to create the THttpClient instance. Using the THttpClient(String url)
 * constructor or passing null as the HttpClient to THttpClient(String url, HttpClient client) will
 * create an instance which will use HttpURLConnection.
 *
 * <p>When using HttpClient, the following configuration leads to 5-15% better performance than the
 * HttpURLConnection implementation:
 *
 * <p>http.protocol.version=HttpVersion.HTTP_1_1 http.protocol.content-charset=UTF-8
 * http.protocol.expect-continue=false http.connection.stalecheck=false
 *
 * <p>Also note that under high load, the HttpURLConnection implementation may exhaust the open file
 * descriptor limit.
 *
 * @see <a href="https://issues.apache.org/jira/browse/THRIFT-970">THRIFT-970</a>
 */
public class THttpClient extends TEndpointTransport {

  private final URL url_;

  private final ByteArrayOutputStream requestBuffer_ = new ByteArrayOutputStream();

  private InputStream inputStream_ = null;

  private int connectTimeout_ = 0;

  private int readTimeout_ = 0;

  private Map<String, String> customHeaders_ = null;

  private final HttpHost host;

  private final HttpClient client;

  private static final Map<String, String> DEFAULT_HEADERS =
      Collections.unmodifiableMap(getDefaultHeaders());

  public static class Factory extends TTransportFactory {

    private final String url;
    private final HttpClient client;

    public Factory(String url) {
      this.url = url;
      this.client = null;
    }

    public Factory(String url, HttpClient client) {
      this.url = url;
      this.client = client;
    }

    @Override
    public TTransport getTransport(TTransport trans) {
      try {
        if (null != client) {
          return new THttpClient(trans.getConfiguration(), url, client);
        } else {
          return new THttpClient(trans.getConfiguration(), url);
        }
      } catch (TTransportException tte) {
        return null;
      }
    }
  }

  public THttpClient(TConfiguration config, String url) throws TTransportException {
    super(config);
    try {
      url_ = new URL(url);
      this.client = null;
      this.host = null;
    } catch (IOException iox) {
      throw new TTransportException(iox);
    }
  }

  public THttpClient(String url) throws TTransportException {
    super(new TConfiguration());
    try {
      url_ = new URL(url);
      this.client = null;
      this.host = null;
    } catch (IOException iox) {
      throw new TTransportException(iox);
    }
  }

  public THttpClient(TConfiguration config, String url, HttpClient client)
      throws TTransportException {
    super(config);
    try {
      url_ = new URL(url);
      this.client = client;
      this.host =
          new HttpHost(
              url_.getProtocol(),
              url_.getHost(),
              -1 == url_.getPort() ? url_.getDefaultPort() : url_.getPort());
    } catch (IOException iox) {
      throw new TTransportException(iox);
    }
  }

  public THttpClient(String url, HttpClient client) throws TTransportException {
    super(new TConfiguration());
    try {
      url_ = new URL(url);
      this.client = client;
      this.host =
          new HttpHost(
              url_.getProtocol(),
              url_.getHost(),
              -1 == url_.getPort() ? url_.getDefaultPort() : url_.getPort());
    } catch (IOException iox) {
      throw new TTransportException(iox);
    }
  }

  public void setConnectTimeout(int timeout) {
    connectTimeout_ = timeout;
  }

  /**
   * Use instead {@link
   * org.apache.hc.client5.http.impl.io.BasicHttpClientConnectionManager#setConnectionConfig} or
   * {@link
   * org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager#setDefaultConnectionConfig}
   */
  @Deprecated
  public void setReadTimeout(int timeout) {
    readTimeout_ = timeout;
  }

  public void setCustomHeaders(Map<String, String> headers) {
    customHeaders_ = new HashMap<>(headers);
  }

  public void setCustomHeader(String key, String value) {
    if (customHeaders_ == null) {
      customHeaders_ = new HashMap<>();
    }
    customHeaders_.put(key, value);
  }

  @Override
  public void open() {}

  @Override
  public void close() {
    if (null != inputStream_) {
      try {
        inputStream_.close();
      } catch (IOException ioe) {
      }
      inputStream_ = null;
    }
  }

  @Override
  public boolean isOpen() {
    return true;
  }

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    if (inputStream_ == null) {
      throw new TTransportException("Response buffer is empty, no request.");
    }

    checkReadBytesAvailable(len);

    try {
      int ret = inputStream_.read(buf, off, len);
      if (ret == -1) {
        throw new TTransportException("No more data available.");
      }
      countConsumedMessageBytes(ret);

      return ret;
    } catch (IOException iox) {
      throw new TTransportException(iox);
    }
  }

  @Override
  public void write(byte[] buf, int off, int len) {
    requestBuffer_.write(buf, off, len);
  }

  private RequestConfig getRequestConfig() {
    RequestConfig requestConfig = RequestConfig.DEFAULT;
    if (connectTimeout_ > 0) {
      requestConfig =
          RequestConfig.copy(requestConfig)
              .setConnectionRequestTimeout(Timeout.ofMilliseconds(connectTimeout_))
              .build();
    }
    return requestConfig;
  }

  private ConnectionConfig getConnectionConfig() {
    ConnectionConfig connectionConfig = ConnectionConfig.DEFAULT;
    if (readTimeout_ > 0) {
      connectionConfig =
          ConnectionConfig.copy(connectionConfig)
              .setSocketTimeout(Timeout.ofMilliseconds(readTimeout_))
              .build();
    }
    return connectionConfig;
  }

  private static Map<String, String> getDefaultHeaders() {
    Map<String, String> headers = new HashMap<>();
    headers.put("Content-Type", "application/x-thrift");
    headers.put("Accept", "application/x-thrift");
    headers.put("User-Agent", "Java/THttpClient/HC");
    return headers;
  }

  private void flushUsingHttpClient() throws TTransportException {
    if (null == this.client) {
      throw new TTransportException("Null HttpClient, aborting.");
    }

    // Extract request and reset buffer
    byte[] data = requestBuffer_.toByteArray();
    requestBuffer_.reset();

    HttpPost post = new HttpPost(this.url_.getFile());
    try {
      // Set request to path + query string
      post.setConfig(getRequestConfig());
      DEFAULT_HEADERS.forEach(post::addHeader);
      if (null != customHeaders_) {
        customHeaders_.forEach(post::addHeader);
      }
      post.setEntity(new ByteArrayEntity(data, null));
      inputStream_ = client.execute(this.host, post, new THttpClientResponseHandler());
    } catch (IOException ioe) {
      // Abort method so the connection gets released back to the connection manager
      post.abort();
      throw new TTransportException(ioe);
    } finally {
      resetConsumedMessageSize(-1);
    }
  }

  public void flush() throws TTransportException {

    if (null != this.client) {
      flushUsingHttpClient();
      return;
    }

    // Extract request and reset buffer
    byte[] data = requestBuffer_.toByteArray();
    requestBuffer_.reset();

    try {
      // Create connection object
      HttpURLConnection connection = (HttpURLConnection) url_.openConnection();

      // Timeouts, only if explicitly set
      if (connectTimeout_ > 0) {
        connection.setConnectTimeout(connectTimeout_);
      }
      if (readTimeout_ > 0) {
        connection.setReadTimeout(readTimeout_);
      }

      // Make the request
      connection.setRequestMethod("POST");
      connection.setRequestProperty("Content-Type", "application/x-thrift");
      connection.setRequestProperty("Accept", "application/x-thrift");
      connection.setRequestProperty("User-Agent", "Java/THttpClient");
      if (customHeaders_ != null) {
        for (Map.Entry<String, String> header : customHeaders_.entrySet()) {
          connection.setRequestProperty(header.getKey(), header.getValue());
        }
      }
      connection.setDoOutput(true);
      connection.connect();
      connection.getOutputStream().write(data);

      int responseCode = connection.getResponseCode();
      if (responseCode != HttpURLConnection.HTTP_OK) {
        throw new TTransportException("HTTP Response code: " + responseCode);
      }

      // Read the responses
      inputStream_ = connection.getInputStream();

    } catch (IOException iox) {
      throw new TTransportException(iox);
    } finally {
      resetConsumedMessageSize(-1);
    }
  }
}
