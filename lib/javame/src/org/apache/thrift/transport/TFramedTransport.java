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

import java.io.ByteArrayInputStream;

import org.apache.thrift.TByteArrayOutputStream;

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 *
 */
public class TFramedTransport extends TTransport {

  /**
   * The largest frame accepted unless the caller says otherwise. The value the
   * desktop Java, netstd and C++ framed transports use.
   */
  public static final int DEFAULT_MAX_FRAME_SIZE = 16384000;

  /**
   * Underlying transport
   */
  private TTransport transport_ = null;

  /**
   * Largest frame this transport will read.
   */
  private int maxFrameSize_ = DEFAULT_MAX_FRAME_SIZE;

  /**
   * Buffer for output
   */
  private final TByteArrayOutputStream writeBuffer_ =
    new TByteArrayOutputStream(1024);

  /**
   * Buffer for input
   */
  private ByteArrayInputStream readBuffer_ = null;

  public static class Factory extends TTransportFactory {
    public Factory() {
    }

    public TTransport getTransport(TTransport base) {
      return new TFramedTransport(base);
    }
  }

  /**
   * Constructor wraps around another tranpsort
   */
  public TFramedTransport(TTransport transport) {
    transport_ = transport;
  }

  /**
   * Constructor wraps around another transport, with an explicit ceiling on
   * the frame size this transport will accept.
   */
  public TFramedTransport(TTransport transport, int maxFrameSize) {
    if (maxFrameSize <= 0) {
      throw new IllegalArgumentException("maxFrameSize must be positive");
    }
    transport_ = transport;
    maxFrameSize_ = maxFrameSize;
  }

  /**
   * @return the largest frame this transport will read.
   */
  public int getMaxFrameSize() {
    return maxFrameSize_;
  }

  public void open() throws TTransportException {
    transport_.open();
  }

  public boolean isOpen() {
    return transport_.isOpen();
  }

  public void close() {
    transport_.close();
  }

  public int read(byte[] buf, int off, int len) throws TTransportException {
    if (readBuffer_ != null) {
      int got = readBuffer_.read(buf, off, len);
      if (got > 0) {
        return got;
      }
    }

    // Read another frame of data
    readFrame();

    return readBuffer_.read(buf, off, len);
  }

  private void readFrame() throws TTransportException {
    byte[] i32rd = new byte[4];
    transport_.readAll(i32rd, 0, 4);
    int size =
      ((i32rd[0] & 0xff) << 24) |
      ((i32rd[1] & 0xff) << 16) |
      ((i32rd[2] & 0xff) <<  8) |
      ((i32rd[3] & 0xff));

    // Checked before the allocation. size is four bytes the peer chose, and
    // new byte[size] commits all of it before readAll blocks for the payload;
    // a negative one would raise NegativeArraySizeException, which is not a
    // TTransportException and escapes every handler expecting one.
    if (size < 0) {
      throw new TTransportException("Read a negative frame size (" + size + ")!");
    }
    if (size > maxFrameSize_) {
      throw new TTransportException("Frame size (" + size
          + ") larger than max length (" + maxFrameSize_ + ")!");
    }

    byte[] buff = new byte[size];
    transport_.readAll(buff, 0, size);
    readBuffer_ = new ByteArrayInputStream(buff);
  }

  public void write(byte[] buf, int off, int len) throws TTransportException {
    writeBuffer_.write(buf, off, len);
  }

  public void flush() throws TTransportException {
    byte[] buf = writeBuffer_.get();
    int len = writeBuffer_.len();
    writeBuffer_.reset();

    byte[] i32out = new byte[4];
    i32out[0] = (byte)(0xff & (len >> 24));
    i32out[1] = (byte)(0xff & (len >> 16));
    i32out[2] = (byte)(0xff & (len >> 8));
    i32out[3] = (byte)(0xff & (len));
    transport_.write(i32out, 0, 4);
    transport_.write(buf, 0, len);
    transport_.flush();
  }
}
