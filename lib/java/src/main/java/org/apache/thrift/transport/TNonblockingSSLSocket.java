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
import java.nio.channels.CancelledKeyException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLParameters;
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
  private FutureTask<Void> delegatedTask;

  private final ExecutorService executorService = Executors.newSingleThreadExecutor();

  protected TNonblockingSSLSocket(String host, int port, int timeout, SSLContext sslContext)
      throws IOException, TTransportException {
    super(host, port, timeout);
    sslEngine_ = sslContext.createSSLEngine(host, port);
    sslEngine_.setUseClientMode(true);
    SSLParameters sslParams = sslEngine_.getSSLParameters();
    sslParams.setEndpointIdentificationAlgorithm("HTTPS");
    sslEngine_.setSSLParameters(sslParams);

    int appBufferSize = sslEngine_.getSession().getApplicationBufferSize();
    int netBufferSize = sslEngine_.getSession().getPacketBufferSize();
    appUnwrap = ByteBuffer.allocate(appBufferSize);
    appUnwrap.flip();
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
    if (!buffer.hasRemaining()) {
      return 0;
    }
    int numBytes = buffer.remaining();
    while (appUnwrap.remaining() < buffer.remaining()) {
      if (appUnwrap.hasRemaining()) {
        buffer.put(appUnwrap);
      }
      try {
        int buffered = netUnwrap.position();
        int progress = doUnwrap();
        if (progress == -1) {
          throw new IOException("Unable to read " + numBytes + " bytes");
        }
        if (progress == 0 && netUnwrap.position() == buffered) {
          selectionKey.interestOps(SelectionKey.OP_READ);
          return numBytes - buffer.remaining();
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

    do {
      int position = buffer.position();
      try {
        if (doWrap(buffer) == -1) {
          throw new IOException("Unable to write " + numBytes + " bytes");
        }
      } catch (IOException iox) {
        throw new TTransportException(TTransportException.UNKNOWN, iox);
      }
      if (hasPendingWrites() || buffer.position() == position) {
        break;
      }
    } while (buffer.hasRemaining());
    return numBytes - buffer.remaining();
  }

  @Override
  public synchronized boolean hasPendingWrites() {
    return netWrap.position() > 0;
  }

  /** {@inheritDoc} */
  @Override
  public synchronized void close() {
    super.close();
    if (!executorService.isShutdown()) {
      try {
        if (delegatedTask != null && !delegatedTask.isDone()) {
          // A provider can hold its engine lock throughout a delegated task.
          // Keep timeout handling independent of that task and close the engine afterward.
          executorService.execute(sslEngine_::closeOutbound);
        } else {
          sslEngine_.closeOutbound();
        }
      } finally {
        executorService.shutdown();
      }
    }
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
    if (delegatedTask != null) {
      if (!delegatedTask.isDone()) {
        return false;
      }
      try {
        delegatedTask.get();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new IOException("Interrupted while completing TLS tasks", e);
      } catch (ExecutionException e) {
        throw new IOException("TLS delegated task failed", e.getCause());
      }
      delegatedTask = null;
    }
    while (true) {
      // The engine may advance before its last output has reached the socket.
      HandshakeStatus hs =
          netWrap.position() > 0 ? HandshakeStatus.NEED_WRAP : sslEngine_.getHandshakeStatus();
      switch (hs) {
        case NEED_UNWRAP:
          int buffered = netUnwrap.position();
          int read = doUnwrap();
          if (read == -1) {
            throw new IOException("TLS peer closed during handshake");
          }
          if (read == 0
              && netUnwrap.position() == buffered
              && sslEngine_.getHandshakeStatus() == hs) {
            selectionKey.interestOps(SelectionKey.OP_READ);
            return false;
          }
          break;
        case NEED_WRAP:
          int written = doWrap(ByteBuffer.allocate(0));
          if (written == -1) {
            throw new IOException("TLS engine closed during handshake");
          }
          if (written == 0 && (netWrap.position() > 0 || sslEngine_.getHandshakeStatus() == hs)) {
            selectionKey.interestOps(SelectionKey.OP_WRITE);
            return false;
          }
          break;
        case NEED_TASK:
          doTask();
          return false;
        case FINISHED:
        case NOT_HANDSHAKING:
          isHandshakeCompleted = true;
          return true;
        default:
          throw new IOException("Unexpected TLS handshake status: " + hs);
      }
    }
  }

  private void doTask() throws IOException {
    List<Runnable> tasks = new ArrayList<>();
    Runnable task;
    while ((task = sslEngine_.getDelegatedTask()) != null) {
      tasks.add(task);
    }
    if (tasks.isEmpty()) {
      throw new IOException("TLS engine requested tasks but supplied none");
    }
    selectionKey.interestOps(0);
    delegatedTask =
        new FutureTask<Void>(
            () -> {
              for (Runnable runnable : tasks) {
                runnable.run();
              }
              return null;
            }) {
          @Override
          protected void done() {
            // FutureTask marks itself done before this callback, avoiding a lost wakeup.
            try {
              selectionKey.interestOps(SelectionKey.OP_WRITE);
              selectionKey.selector().wakeup();
            } catch (CancelledKeyException ignored) {
              // The call may have timed out or closed while its TLS task was running.
            }
          }
        };
    executorService.execute(delegatedTask);
  }

  private int doUnwrap() throws IOException {
    int num = 0;
    while (true) {
      netUnwrap.flip();
      appUnwrap.compact();
      SSLEngineResult unwrapResult;
      try {
        unwrapResult = sslEngine_.unwrap(netUnwrap, appUnwrap);
      } catch (SSLException ex) {
        LOGGER.error(ex.getMessage());
        throw ex;
      } finally {
        netUnwrap.compact();
        appUnwrap.flip();
      }

      switch (unwrapResult.getStatus()) {
        case OK:
          return num;
        case CLOSED:
          return appUnwrap.hasRemaining() ? num : -1;
        case BUFFER_OVERFLOW:
          throw new IllegalStateException("Failed to unwrap");
        case BUFFER_UNDERFLOW:
          if (appUnwrap.hasRemaining()) {
            return num;
          }
          break;
      }

      // Drain buffered records before reading from the socket, which may already be at EOF.
      num = getSocketChannel().read(netUnwrap);
      if (num <= 0) {
        return num;
      }
    }
  }

  private int doWrap(ByteBuffer appWrap) throws IOException {
    int num = 0;
    if (hasPendingWrites()) {
      num = writeNetBuffer();
      if (hasPendingWrites() || !appWrap.hasRemaining()) {
        return num;
      }
    }
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
          num += writeNetBuffer();
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

  private int writeNetBuffer() throws IOException {
    netWrap.flip();
    try {
      return getSocketChannel().write(netWrap);
    } finally {
      if (netWrap.position() == 0) {
        // Restore write mode without compacting an unchanged buffer under backpressure.
        netWrap.position(netWrap.limit());
        netWrap.limit(netWrap.capacity());
      } else {
        netWrap.compact();
      }
    }
  }
}
