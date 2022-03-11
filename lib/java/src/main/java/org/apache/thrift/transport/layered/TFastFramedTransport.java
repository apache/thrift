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
package org.apache.thrift.transport.layered;


import org.apache.thrift.TConfiguration;
import org.apache.thrift.transport.*;

import java.util.Objects;

/**
 * This transport is wire compatible with {@link TFramedTransport}, but makes
 * use of reusable, expanding read and write buffers in order to avoid
 * allocating new byte[]s all the time. Since the buffers only expand, you
 * should probably only use this transport if your messages are not too variably
 * large, unless the persistent memory cost is not an issue.
 *
 * This implementation is NOT threadsafe.
 */
public class TFastFramedTransport extends TLayeredTransport {

  public static class Factory extends TTransportFactory {
    private final int initialCapacity;
    private final int maxLength;

    public Factory() {
      this(DEFAULT_BUF_CAPACITY, TConfiguration.DEFAULT_MAX_FRAME_SIZE);
    }

    public Factory(int initialCapacity) {
      this(initialCapacity, TConfiguration.DEFAULT_MAX_FRAME_SIZE);
    }

    public Factory(int initialCapacity, int maxLength) {
      this.initialCapacity = initialCapacity;
      this.maxLength = maxLength;
    }

    @Override
    public TTransport getTransport(TTransport trans) throws TTransportException {
      return new TFastFramedTransport(trans,
          initialCapacity,
          maxLength);
    }
  }

  /**
   * How big should the default read and write buffers be?
   */
  public static final int DEFAULT_BUF_CAPACITY = 1024;

  private final AutoExpandingBufferWriteTransport writeBuffer;
  private AutoExpandingBufferReadTransport readBuffer;
  private final int initialBufferCapacity;
  private final byte[] i32buf = new byte[4];
  private final int maxLength;

  /**
   * Create a new {@link TFastFramedTransport}. Use the defaults
   * for initial buffer size and max frame length.
   * @param underlying Transport that real reads and writes will go through to.
   */
  public TFastFramedTransport(TTransport underlying) throws TTransportException {
    this(underlying, DEFAULT_BUF_CAPACITY, TConfiguration.DEFAULT_MAX_FRAME_SIZE);
  }

  /**
   * Create a new {@link TFastFramedTransport}. Use the specified
   * initial buffer capacity and the default max frame length.
   * @param underlying Transport that real reads and writes will go through to.
   * @param initialBufferCapacity The initial size of the read and write buffers.
   * In practice, it's not critical to set this unless you know in advance that
   * your messages are going to be very large.
   */
  public TFastFramedTransport(TTransport underlying, int initialBufferCapacity) throws TTransportException {
    this(underlying, initialBufferCapacity, TConfiguration.DEFAULT_MAX_FRAME_SIZE);
  }

  /**
   *
   * @param underlying Transport that real reads and writes will go through to.
   * @param initialBufferCapacity The initial size of the read and write buffers.
   * In practice, it's not critical to set this unless you know in advance that
   * your messages are going to be very large. (You can pass
   * TFramedTransportWithReusableBuffer.DEFAULT_BUF_CAPACITY if you're only
   * using this constructor because you want to set the maxLength.)
   * @param maxLength The max frame size you are willing to read. You can use
   * this parameter to limit how much memory can be allocated.
   */
  public TFastFramedTransport(TTransport underlying, int initialBufferCapacity, int maxLength) throws TTransportException {
    super(underlying);
    TConfiguration config = Objects.isNull(underlying.getConfiguration()) ? new TConfiguration() : underlying.getConfiguration();
    this.maxLength = maxLength;
    config.setMaxFrameSize(maxLength);
    this.initialBufferCapacity = initialBufferCapacity;
    readBuffer = new AutoExpandingBufferReadTransport(config, initialBufferCapacity);
    writeBuffer = new AutoExpandingBufferWriteTransport(config, initialBufferCapacity, 4);
  }

  @Override
  public void close() {
    getInnerTransport().close();
  }

  @Override
  public boolean isOpen() {
    return getInnerTransport().isOpen();
  }

  @Override
  public void open() throws TTransportException {
    getInnerTransport().open();
  }

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    int got = readBuffer.read(buf, off, len);
    if (got > 0) {
      return got;
    }

    // Read another frame of data
    readFrame();

    return readBuffer.read(buf, off, len);
  }

  private void readFrame() throws TTransportException {
    getInnerTransport().readAll(i32buf , 0, 4);
    int size = TFramedTransport.decodeFrameSize(i32buf);

    if (size < 0) {
      close();
      throw new TTransportException(TTransportException.CORRUPTED_DATA, "Read a negative frame size (" + size + ")!");
    }

    if (size > getInnerTransport().getConfiguration().getMaxFrameSize()) {
      close();
      throw new TTransportException(TTransportException.CORRUPTED_DATA,
          "Frame size (" + size + ") larger than max length (" + maxLength + ")!");
    }

    readBuffer.fill(getInnerTransport(), size);
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {
    writeBuffer.write(buf, off, len);
  }

  @Override
  public void consumeBuffer(int len) {
    readBuffer.consumeBuffer(len);
  }

  /**
   * Only clears the read buffer!
   */
  public void clear() throws TTransportException {
    readBuffer = new AutoExpandingBufferReadTransport(getConfiguration(), initialBufferCapacity);
  }

  @Override
  public void flush() throws TTransportException {
    int payloadLength = writeBuffer.getLength() - 4;
    byte[] data = writeBuffer.getBuf().array();
    TFramedTransport.encodeFrameSize(payloadLength, data);
    getInnerTransport().write(data, 0, payloadLength + 4);
    writeBuffer.reset();
    getInnerTransport().flush();
  }

  @Override
  public byte[] getBuffer() {
    return readBuffer.getBuffer();
  }

  @Override
  public int getBufferPosition() {
    return readBuffer.getBufferPosition();
  }

  @Override
  public int getBytesRemainingInBuffer() {
    return readBuffer.getBytesRemainingInBuffer();
  }
}
