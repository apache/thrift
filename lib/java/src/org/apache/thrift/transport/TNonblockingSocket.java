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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.URL;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.security.KeyStore;
import java.util.Iterator;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;

import org.apache.thrift.transport.TSSLTransportFactory.TSSLTransportParameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Transport for use with async client.
 */
public class TNonblockingSocket extends TNonblockingTransport {

  private static final Logger LOGGER = LoggerFactory.getLogger(TNonblockingSocket.class.getName());

  /**
   * Host and port if passed in, used for lazy non-blocking connect.
   */
  private final SocketAddress socketAddress_;

  private final SocketChannel socketChannel_;

  private SSLEngine sslEngine_;

  private int appBufferSize;

  private int netBufferSize;

  private ByteBuffer clientUnwrap, clientWrap;

  private ByteBuffer serverUnwrap, serverWrap;

  private ByteBuffer decodedBytes;

  private boolean isHandshakeCompleted;


  public static TNonblockingSocket getClientSocket(String host, int port, int timeout, TSSLTransportParameters params)
      throws TTransportException, IOException {
    if (params == null || !(params.isKeyStoreSet || params.isTrustStoreSet)) {
      throw new TTransportException("Either one of the KeyStore or TrustStore must be set for SSLTransportParameters");
    }
    SSLContext sslContext;
    sslContext = TNonblockingSocket.createSSLContext(params);
    return new TNonblockingSocket(host, port, timeout, sslContext);
  }

  public TNonblockingSocket(String host, int port) throws IOException {
    this(host, port, 0);
  }

  /**
   * Create a new nonblocking socket transport that will be connected to host:port.
   * @param host
   * @param port
   * @throws IOException
   */
  public TNonblockingSocket(String host, int port, int timeout) throws IOException {
    this(SocketChannel.open(), timeout, new InetSocketAddress(host, port));
  }

  /**
   * Constructor that takes an already created socket.
   *
   * @param socketChannel Already created SocketChannel object
   * @throws IOException if there is an error setting up the streams
   */
  public TNonblockingSocket(SocketChannel socketChannel) throws IOException {
    this(socketChannel, 0, null);
    if (!socketChannel.isConnected()) throw new IOException("Socket must already be connected");
  }

  private TNonblockingSocket(SocketChannel socketChannel, int timeout, SocketAddress socketAddress)
      throws IOException {
    socketChannel_ = socketChannel;
    socketAddress_ = socketAddress;

    // make it a nonblocking channel
    socketChannel.configureBlocking(false);

    // set options
    Socket socket = socketChannel.socket();
    socket.setSoLinger(false, 0);
    socket.setTcpNoDelay(true);
    socket.setKeepAlive(true);
    setTimeout(timeout);

    isSSLEnabled = false;
    sslEngine_ = null;
  }

  private TNonblockingSocket(String host, int port, int timeout, SSLContext sslContext) throws IOException {
    socketChannel_ = SocketChannel.open();
    socketAddress_ = new InetSocketAddress(host, port);
    sslEngine_ = sslContext.createSSLEngine(host, port);
    sslEngine_.setUseClientMode(true);

    // make it a nonblocking channel
    socketChannel_.configureBlocking(false);

    // set options
    Socket socket = socketChannel_.socket();
    socket.setSoLinger(false, 0);
    socket.setTcpNoDelay(true);
    socket.setKeepAlive(true);
    setTimeout(timeout);

    appBufferSize = sslEngine_.getSession().getApplicationBufferSize();
    netBufferSize = sslEngine_.getSession().getPacketBufferSize();
    clientUnwrap = ByteBuffer.allocate(netBufferSize);
    serverUnwrap = ByteBuffer.allocate(appBufferSize);
    clientWrap = ByteBuffer.allocate(appBufferSize);
    serverWrap = ByteBuffer.allocate(netBufferSize);
    decodedBytes = ByteBuffer.allocate(appBufferSize);
    decodedBytes.flip();
    isHandshakeCompleted = false;

    isSSLEnabled = true;
  }

  /**
   * Register the new SocketChannel with our Selector, indicating
   * we'd like to be notified when it's ready for I/O.
   *
   * @param selector
   * @return the selection key for this socket.
   */
  public SelectionKey registerSelector(Selector selector, int interests) throws IOException {
    return socketChannel_.register(selector, interests);
  }

  /**
   * Sets the socket timeout, although this implementation never uses blocking operations so it is unused.
   *
   * @param timeout Milliseconds timeout
   */
  public void setTimeout(int timeout) {
    try {
      socketChannel_.socket().setSoTimeout(timeout);
    } catch (SocketException sx) {
      LOGGER.warn("Could not set socket timeout.", sx);
    }
  }

  /**
   * Returns a reference to the underlying SocketChannel.
   */
  public SocketChannel getSocketChannel() {
    return socketChannel_;
  }

  /**
   * Checks whether the socket is connected.
   */
  public boolean isOpen() {
    // isConnected() does not return false after close(), but isOpen() does
    if (isSSLEnabled) {
      return socketChannel_.isOpen() && socketChannel_.isConnected() && isHandshakeCompleted;
    } else {
      return socketChannel_.isOpen() && socketChannel_.isConnected();
    }
  }

  /**
   * Do not call, the implementation provides its own lazy non-blocking connect.
   */
  public void open() throws TTransportException {
    throw new RuntimeException("open() is not implemented for TNonblockingSocket");
  }

  public synchronized boolean startConnection() throws IOException {
    boolean result = false;
    if (this.isOpen()) {
      return true;
    }

    Selector sel = SelectorProvider.provider().openSelector();
    try {
    SelectionKey key;
      key = this.registerSelector(sel, SelectionKey.OP_CONNECT | SelectionKey.OP_READ);
      this.startConnect();
      this.beginSSLHandshake();
      key.selector().select();
      Iterator<SelectionKey> keys = key.selector().selectedKeys().iterator();
      while (keys.hasNext()) {
        keys.next();
        keys.remove();
        if (!key.isConnectable() || !this.finishConnect()) {
      throw new IOException("Transport is not connectable. Unable to start connection");
        }
        if (this.doHandShake())
          result = true;
      }
    } catch (IOException e) {
      throw e;
    } finally {
      sel.close();
    }
    return result;
  }

  public void beginSSLHandshake() throws IOException {
    // Begin handshake
    sslEngine_.beginHandshake();
  }

  public synchronized boolean doHandShake() throws IOException {
    LOGGER.debug("Handshake is started");
    ByteBuffer buffer;
    while (true) {
      HandshakeStatus hs = sslEngine_.getHandshakeStatus();
      if (hs == HandshakeStatus.NEED_UNWRAP) {
        buffer = readBuffer();
        if (buffer == null) {
          LOGGER.error("Unexpected. Handshake failed abruptly during read");
          return false;
        }
        clientUnwrap.put(buffer);
        if (dounWrap() == -1) {
          LOGGER.error("Unexpected. Handshake failed abruptly during unwrap");
          return false;
        }
      } else if (hs == HandshakeStatus.NEED_WRAP) {
        if (doWrap() == -1) {
      LOGGER.error("Unexpected. Handshake failed abruptly during wrap");
      return false;
        }
      } else if (hs == HandshakeStatus.NEED_TASK) {
        if (!doTask()) {
          LOGGER.error("Unexpected. Handshake failed abruptly during task");
          return false;
        }
      } else if (hs == HandshakeStatus.FINISHED) {
      LOGGER.error("Unexpected. Handshake finished. Handshake failed");
          return false;
      } else if (hs == HandshakeStatus.NOT_HANDSHAKING) {
          LOGGER.debug("Handshake is finished");
          SSLSession session = sslEngine_.getSession();
          try {
            LOGGER.debug("local principal: " + session.getLocalPrincipal());
            LOGGER.debug("remote principal: " + session.getPeerPrincipal());
            LOGGER.debug("cipher: " + session.getCipherSuite());
          } catch (Exception exc) {
            exc.printStackTrace();
          }
          isHandshakeCompleted = true;
          return true;
      } else {
          LOGGER.error("Unknown handshake status. Handshake failed");
          return false;
      }
    }
  }

  public synchronized boolean doTask() {
    Runnable runnable;
    while ((runnable = sslEngine_.getDelegatedTask()) != null) {
      runnable.run();
    }
    HandshakeStatus hs = sslEngine_.getHandshakeStatus();
    if (hs == HandshakeStatus.NEED_TASK) {
      try {
        throw new TTransportException(TTransportException.UNKNOWN, "handshake shouldn't need additional tasks");
      } catch (TTransportException e) {
        e.printStackTrace();
        return false;
      }
    }
    return true;
  }

  public synchronized int dounWrap() throws IOException {
    int num = 0;
    SSLEngineResult unwrapResult;

    try {
      clientUnwrap.flip();
      unwrapResult = sslEngine_.unwrap(clientUnwrap, serverUnwrap);
      clientUnwrap.compact();
    } catch (SSLException ex) {
      LOGGER.error(ex.getMessage());
      ex.printStackTrace();
      throw ex;
    }

    switch (unwrapResult.getStatus()) {
    case OK:
      if (serverUnwrap.position() > 0) {
        serverUnwrap.flip();
        serverUnwrap.compact();
      }
      break;
    case CLOSED:
      LOGGER.error("SSL session is closed");
      return -1;
    case BUFFER_OVERFLOW:
      throw new IllegalStateException("Failed to unwrap");
    case BUFFER_UNDERFLOW:
      break;
    }
    return num;
  }

  public synchronized int doWrap() throws IOException {
    int num = 0;
    SSLEngineResult wrapResult;
    try {
      clientWrap.flip();
      wrapResult = sslEngine_.wrap(clientWrap, serverWrap);
      clientWrap.compact();
    } catch (SSLException exc) {
      LOGGER.error(exc.getMessage());
      exc.printStackTrace();
      throw exc;
    }

    switch (wrapResult.getStatus()) {
    case OK:
      if (serverWrap.position() > 0) {
        serverWrap.flip();
        num = socketChannel_.write(serverWrap);
        serverWrap.compact();
      }
      break;
    case BUFFER_UNDERFLOW:
      // try again later
      break;
    case BUFFER_OVERFLOW:
      throw new IllegalStateException("Failed to wrap");
    case CLOSED:
      LOGGER.error("SSL session is closed");
      return -1;
    }
    return num;
  }

  public ByteBuffer readBuffer() throws IOException {
    ByteBuffer buffer;
    buffer = ByteBuffer.allocate(netBufferSize);
    int num = socketChannel_.read(buffer);
    if (num < 0) {
      LOGGER.error("Failed during read operation");
      return null;
    }
    buffer.flip();
    return buffer;
  }

  /**
   * Perform a nonblocking read into buffer.
   */
  public int read(ByteBuffer buffer) throws IOException {
    if (!isSSLEnabled) {
      return socketChannel_.read(buffer);
    }
    else {
      boolean isSufficientBytes = false;
      int numBytes = buffer.limit();
      if ((decodedBytes.remaining() >= numBytes)) {
        isSufficientBytes = true;
      }
      while (!isSufficientBytes) {
        HandshakeStatus hs = sslEngine_.getHandshakeStatus();
        if (hs == HandshakeStatus.FINISHED)
          throw new IOException("Read operation is terminated. Handshake is completed");
        ByteBuffer rawdata = readBuffer();
        if(rawdata == null) {
          LOGGER.error("Failed during read operation. Probably server is down");
          return -1;
        }
        if (rawdata != null) {
          clientUnwrap.put(rawdata);
          if (dounWrap() == -1)
            throw new IOException("Unable to read " + numBytes + " bytes");
        }
        if (serverUnwrap.position() > 0) {
          int t;
          serverUnwrap.flip();
          if (decodedBytes.position() > 0)
            decodedBytes.flip();
          t = serverUnwrap.limit() + decodedBytes.limit();
          byte[] tmpBuffer = new byte[t];
          decodedBytes.get(tmpBuffer, 0, decodedBytes.remaining());
          serverUnwrap.get(tmpBuffer, 0, serverUnwrap.remaining());
          if (serverUnwrap.position() > 0) {
            serverUnwrap.clear();
            serverUnwrap.flip();
            serverUnwrap.compact();
          }
          decodedBytes = ByteBuffer.wrap(tmpBuffer);
        }
        if (decodedBytes.remaining() >= numBytes) {
          isSufficientBytes = true;
          break;
        }
      }
      if (isSufficientBytes) {
        byte[] b = new byte[numBytes];
        decodedBytes.get(b, 0, numBytes);
        if (decodedBytes.position() > 0) {
          decodedBytes.compact();
          decodedBytes.flip();
        }
        buffer.put(b);
        return numBytes;
      }
      return -1;
    }
  }


  /**
   * Reads from the underlying input stream if not null.
   */
  public int read(byte[] buf, int off, int len) throws TTransportException {
    if ((socketChannel_.validOps() & SelectionKey.OP_READ) != SelectionKey.OP_READ) {
      throw new TTransportException(TTransportException.NOT_OPEN,
        "Cannot read from write-only socket channel");
    }
    try {
      return this.read(ByteBuffer.wrap(buf, off, len));
    } catch (IOException iox) {
      throw new TTransportException(TTransportException.UNKNOWN, iox);
    }
  }

  /**
   * Perform a nonblocking write of the data in buffer;
   */
  public int write(ByteBuffer buffer) throws IOException {
    if (isSSLEnabled) {
      int numBytes = 0;

      if (buffer.position() > 0)
        buffer.flip();

      int nTransfer = 0;
      int num = 0;
      while (buffer.remaining() != 0) {
        nTransfer = Math.min(clientWrap.remaining(), buffer.remaining());
        if (nTransfer > 0) {
          clientWrap.put(buffer.array(), buffer.arrayOffset()+buffer.position(), nTransfer);
          buffer.position(buffer.position()+nTransfer);
        }

        num = doWrap();
        if ( num < 0) {
          LOGGER.error("Failed while writing. Probably server is down");
          return -1;
        }
        numBytes += num;
      }
      return numBytes;
    } else {
      return socketChannel_.write(buffer);
    }
  }

  /**
   * Writes to the underlying output stream if not null.
   */
  public void write(byte[] buf, int off, int len) throws TTransportException {
    if ((socketChannel_.validOps() & SelectionKey.OP_WRITE) != SelectionKey.OP_WRITE) {
      throw new TTransportException(TTransportException.NOT_OPEN,
        "Cannot write to write-only socket channel");
    }
    try {
      this.write(ByteBuffer.wrap(buf, off, len));
    } catch (IOException iox) {
      throw new TTransportException(TTransportException.UNKNOWN, iox);
    }
  }

  /**
   * Noop.
   */
  public void flush() throws TTransportException {
    // Not supported by SocketChannel.
  }

  /**
   * Closes the socket.
   */
  public void close() {
    try {
      socketChannel_.close();
    } catch (IOException iox) {
      LOGGER.warn("Could not close socket.", iox);
    }
  }

  /** {@inheritDoc} */
  public boolean startConnect() throws IOException {
    return socketChannel_.connect(socketAddress_);
  }

  /** {@inheritDoc} */
  public boolean finishConnect() throws IOException {
    return socketChannel_.finishConnect();
  }

  private static SSLContext createSSLContext(TSSLTransportParameters params) throws TTransportException {
    SSLContext ctx;
    InputStream in = null;
    InputStream is = null;

    try {
      ctx = SSLContext.getInstance(params.protocol);
      TrustManagerFactory tmf = null;
      KeyManagerFactory kmf = null;

      if (params.isTrustStoreSet) {
        tmf = TrustManagerFactory.getInstance(params.trustManagerType);
        KeyStore ts = KeyStore.getInstance(params.trustStoreType);
        in = getStoreAsStream(params.trustStore);
        ts.load(in, (params.trustPass != null ? params.trustPass.toCharArray() : null));
        tmf.init(ts);
      }

      if (params.isKeyStoreSet) {
        kmf = KeyManagerFactory.getInstance(params.keyManagerType);
        KeyStore ks = KeyStore.getInstance(params.keyStoreType);
        is = getStoreAsStream(params.keyStore);
        ks.load(is, params.keyPass.toCharArray());
        kmf.init(ks, params.keyPass.toCharArray());
      }

      if (params.isKeyStoreSet && params.isTrustStoreSet) {
        ctx.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null);
      } else if (params.isKeyStoreSet) {
        ctx.init(kmf.getKeyManagers(), null, null);
      } else {
        ctx.init(null, tmf.getTrustManagers(), null);
      }

    } catch (Exception e) {
      throw new TTransportException("Error creating the transport", e);
    } finally {
      if (in != null) {
        try {
          in.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
      if (is != null) {
        try {
          is.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
    return ctx;
  }

  private static InputStream getStoreAsStream(String store) throws IOException {
    try {
      return new FileInputStream(store);
    } catch (FileNotFoundException e) {
    }

    InputStream storeStream = null;
    try {
      storeStream = new URL(store).openStream();
      if (storeStream != null) {
        return storeStream;
      }
    } catch (MalformedURLException e) {
    }

    storeStream = Thread.currentThread().getContextClassLoader().getResourceAsStream(store);

    if (storeStream != null) {
      return storeStream;
    } else {
      throw new IOException("Could not load file: " + store);
    }
  }
}
