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

import org.apache.thrift.TByteArrayOutputStream;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.transport.TTransportFactory;

import java.util.Objects;

/**
 * TFramedTransport is a buffered TTransport that ensures a fully read message
 * every time by preceding messages with a 4-byte frame size.
 */
public class TFramedTransport extends TLayeredTransport {

  /**
   * Buffer for output
   */
  private final TByteArrayOutputStream writeBuffer_ =
    new TByteArrayOutputStream(1024);

  /**
   * Buffer for input
   */
  private final TMemoryInputTransport readBuffer_;

  public static class Factory extends TTransportFactory {
    private int maxLength_;

    public Factory() {
      maxLength_ = TConfiguration.DEFAULT_MAX_FRAME_SIZE;
    }

    public Factory(int maxLength) {
      maxLength_ = maxLength;
    }

    @Override
    public TTransport getTransport(TTransport base) throws TTransportException {
      return new TFramedTransport(base, maxLength_);
    }
  }

  /**
   * Something to fill in the first four bytes of the buffer
   * to make room for the frame size.  This allows the
   * implementation to write once instead of twice.
   */
  private static final byte[] sizeFiller_ = new byte[] { 0x00, 0x00, 0x00, 0x00 };

  /**
   * Constructor wraps around another transport
   */
  public TFramedTransport(TTransport transport, int maxLength) throws TTransportException {
    super(transport);
    TConfiguration _configuration = Objects.isNull(transport.getConfiguration()) ? new TConfiguration() : transport.getConfiguration();
    _configuration.setMaxFrameSize(maxLength);
    writeBuffer_.write(sizeFiller_, 0, 4);
    readBuffer_= new TMemoryInputTransport(_configuration, new byte[0]);
  }

  public TFramedTransport(TTransport transport) throws TTransportException {
    this(transport, TConfiguration.DEFAULT_MAX_FRAME_SIZE);
  }

  public void open() throws TTransportException {
    getInnerTransport().open();
  }

  public boolean isOpen() {
    return getInnerTransport().isOpen();
  }

  public void close() {
    getInnerTransport().close();
  }

  public int read(byte[] buf, int off, int len) throws TTransportException {
    int got = readBuffer_.read(buf, off, len);
    if (got > 0) {
      return got;
    }

    // Read another frame of data
    readFrame();

    return readBuffer_.read(buf, off, len);
  }

  @Override
  public byte[] getBuffer() {
    return readBuffer_.getBuffer();
  }

  @Override
  public int getBufferPosition() {
    return readBuffer_.getBufferPosition();
  }

  @Override
  public int getBytesRemainingInBuffer() {
    return readBuffer_.getBytesRemainingInBuffer();
  }

  @Override
  public void consumeBuffer(int len) {
    readBuffer_.consumeBuffer(len);
  }

  public void clear() {
    readBuffer_.clear();
  }

  private final byte[] i32buf = new byte[4];

  private void readFrame() throws TTransportException {
    getInnerTransport().readAll(i32buf, 0, 4);
    int size = decodeFrameSize(i32buf);

    if (size < 0) {
      close();
      throw new TTransportException(TTransportException.CORRUPTED_DATA, "Read a negative frame size (" + size + ")!");
    }

    if (size > getInnerTransport().getConfiguration().getMaxFrameSize()) {
      close();
      throw new TTransportException(TTransportException.CORRUPTED_DATA,
          "Frame size (" + size + ") larger than max length (" + getInnerTransport().getConfiguration().getMaxFrameSize() + ")!");
    }

    byte[] buff = new byte[size];
    getInnerTransport().readAll(buff, 0, size);
    readBuffer_.reset(buff);
  }

  public void write(byte[] buf, int off, int len) throws TTransportException {
    writeBuffer_.write(buf, off, len);
  }

  @Override
  public void flush() throws TTransportException {
    byte[] buf = writeBuffer_.get();
    int len = writeBuffer_.len() - 4;       // account for the prepended frame size
    writeBuffer_.reset();
    writeBuffer_.write(sizeFiller_, 0, 4);  // make room for the next frame's size data

    encodeFrameSize(len, buf);              // this is the frame length without the filler
    getInnerTransport().write(buf, 0, len + 4);      // we have to write the frame size and frame data
    getInnerTransport().flush();
  }

  public static final void encodeFrameSize(final int frameSize, final byte[] buf) {
    buf[0] = (byte)(0xff & (frameSize >> 24));
    buf[1] = (byte)(0xff & (frameSize >> 16));
    buf[2] = (byte)(0xff & (frameSize >> 8));
    buf[3] = (byte)(0xff & (frameSize));
  }

  public static final int decodeFrameSize(final byte[] buf) {
    return
      ((buf[0] & 0xff) << 24) |
      ((buf[1] & 0xff) << 16) |
      ((buf[2] & 0xff) <<  8) |
      ((buf[3] & 0xff));
  }
}
