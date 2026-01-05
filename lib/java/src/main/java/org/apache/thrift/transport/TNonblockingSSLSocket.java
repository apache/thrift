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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Transport for use with async ssl client. */
public class TNonblockingSSLSocket extends TNonblockingSocket implements SocketAddressProvider {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(TNonblockingSSLSocket.class.getName());

  private final SSLEngine sslEngine_;

  private final ByteBuffer appUnwrap;
  private final ByteBuffer netUnwrap;

  private final ByteBuffer netWrap;

  private boolean isHandshakeCompleted;

  private SelectionKey selectionKey;

  private final ExecutorService executorService = Executors.newSingleThreadExecutor();

  protected TNonblockingSSLSocket(String host, int port, int timeout, SSLContext sslContext)
      throws IOException, TTransportException {
    super(host, port, timeout);
    sslEngine_ = sslContext.createSSLEngine(host, port);
    sslEngine_.setUseClientMode(true);

    int appBufferSize = sslEngine_.getSession().getApplicationBufferSize();
    int netBufferSize = sslEngine_.getSession().getPacketBufferSize();
    appUnwrap = ByteBuffer.allocate(appBufferSize);
    netUnwrap = ByteBuffer.allocate(netBufferSize);
    netWrap = ByteBuffer.allocate(netBufferSize);
    isHandshakeCompleted = false;
  }

  /** {@inheritDoc} */
  @Override
  public SelectionKey registerSelector(Selector selector, int interests) throws IOException {
    selectionKey = super.registerSelector(selector, interests);
    return selectionKey;
  }

  /** {@inheritDoc} */
  @Override
  public boolean isOpen() {
    // isConnected() does not return false after close(), but isOpen() does
    return super.isOpen() && isHandshakeCompleted;
  }

  /** {@inheritDoc} */
  @Override
  public void open() throws TTransportException {
    throw new RuntimeException("open() is not implemented for TNonblockingSSLSocket");
  }

  /** {@inheritDoc} */
  @Override
  public synchronized int read(ByteBuffer buffer) throws TTransportException {
    int numBytes = buffer.remaining();
    while (appUnwrap.limit() == appUnwrap.capacity()
        || appUnwrap.remaining() < buffer.remaining()) {
      if (appUnwrap.limit() < appUnwrap.capacity() && appUnwrap.hasRemaining()) {
        buffer.put(appUnwrap);
      }
      try {
        if (doUnwrap() == -1) {
          throw new IOException("Unable to read " + numBytes + " bytes");
        }
      } catch (IOException iox) {
        throw new TTransportException(TTransportException.UNKNOWN, iox);
      }
    }
    if (buffer.hasRemaining()) {
      int originLimit = appUnwrap.limit();
      appUnwrap.limit(appUnwrap.position() + buffer.remaining());
      buffer.put(appUnwrap);
      appUnwrap.limit(originLimit);
    }
    // In SSL mode, the Thrift server may merge the frame size and body into a single TLS package.
    // Setting OP_WRITE to trigger subsequent read operations in the Thrift async client.
    selectionKey.interestOps(SelectionKey.OP_WRITE);
    return numBytes;
  }

  /** {@inheritDoc} */
  @Override
  public synchronized int write(ByteBuffer buffer) throws TTransportException {
    int numBytes = buffer.remaining();

    while (buffer.hasRemaining()) {
      try {
        if (doWrap(buffer) == -1) {
          throw new IOException("Unable to write " + numBytes + " bytes");
        }
      } catch (IOException iox) {
        throw new TTransportException(TTransportException.UNKNOWN, iox);
      }
    }
    return numBytes;
  }

  /** {@inheritDoc} */
  @Override
  public void close() {
    executorService.shutdown();
    sslEngine_.closeOutbound();
    super.close();
  }

  /** {@inheritDoc} */
  @Override
  public boolean startConnect() throws IOException {
    if (this.isOpen()) {
      return true;
    }
    sslEngine_.beginHandshake();
    return super.startConnect() && doHandShake();
  }

  /** {@inheritDoc} */
  @Override
  public boolean finishConnect() throws IOException {
    return super.finishConnect() && doHandShake();
  }

  private synchronized boolean doHandShake() throws IOException {
    while (true) {
      HandshakeStatus hs = sslEngine_.getHandshakeStatus();
      switch (hs) {
        case NEED_UNWRAP:
          if (doUnwrap() == -1) {
            LOGGER.error("Unexpected. Handshake failed abruptly during unwrap");
            return false;
          }
          break;
        case NEED_WRAP:
          if (doWrap(ByteBuffer.wrap(new byte[0])) == -1) {
            LOGGER.error("Unexpected. Handshake failed abruptly during wrap");
            return false;
          }
          break;
        case NEED_TASK:
          doTask();
          break;
        case FINISHED:
        case NOT_HANDSHAKING:
          isHandshakeCompleted = true;
          return true;
        default:
          LOGGER.error("Unknown handshake status. Handshake failed");
          return false;
      }
    }
  }

  private void doTask() {
    Runnable runnable;
    while ((runnable = sslEngine_.getDelegatedTask()) != null) {
      executorService.submit(runnable);
    }
  }

  private int doUnwrap() throws IOException {
    int num = getSocketChannel().read(netUnwrap);
    netUnwrap.flip();
    if (num < 0) {
      LOGGER.error("Failed during read operation. Probably server is down");
      return -1;
    }
    SSLEngineResult unwrapResult;

    try {
      appUnwrap.clear();
      unwrapResult = sslEngine_.unwrap(netUnwrap, appUnwrap);
      netUnwrap.compact();
    } catch (SSLException ex) {
      LOGGER.error(ex.getMessage());
      throw ex;
    }

    switch (unwrapResult.getStatus()) {
      case OK:
        if (appUnwrap.position() > 0) {
          appUnwrap.flip();
        }
        break;
      case CLOSED:
        return -1;
      case BUFFER_OVERFLOW:
        throw new IllegalStateException("Failed to unwrap");
      case BUFFER_UNDERFLOW:
        break;
    }
    return num;
  }

  private int doWrap(ByteBuffer appWrap) throws IOException {
    int num = 0;
    SSLEngineResult wrapResult;
    try {
      wrapResult = sslEngine_.wrap(appWrap, netWrap);
    } catch (SSLException exc) {
      LOGGER.error(exc.getMessage());
      throw exc;
    }

    switch (wrapResult.getStatus()) {
      case OK:
        if (netWrap.position() > 0) {
          netWrap.flip();
          num = getSocketChannel().write(netWrap);
          netWrap.clear();
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
}
