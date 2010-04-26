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

import org.apache.thrift.TByteArrayOutputStream;

/**
 * TFramedTransport is a buffered TTransport that ensures a fully read message
 * every time by preceeding messages with a 4-byte frame size. 
 */
public class TFramedTransport extends TTransport {

  protected static final int DEFAULT_MAX_LENGTH = 0x7FFFFFFF;

  private int maxLength_;

  /**
   * Underlying transport
   */
  private TTransport transport_ = null;

  /**
   * Buffer for output
   */
  private final TByteArrayOutputStream writeBuffer_ =
    new TByteArrayOutputStream(1024);

  /**
   * Buffer for input
   */
  private TMemoryInputTransport readBuffer_ = new TMemoryInputTransport(new byte[0]);

  public static class Factory extends TTransportFactory {
    private int maxLength_;

    public Factory() {
      maxLength_ = TFramedTransport.DEFAULT_MAX_LENGTH;
    }

    public Factory(int maxLength) {
      maxLength_ = maxLength;
    }

    public TTransport getTransport(TTransport base) {
      return new TFramedTransport(base, maxLength_);
    }
  }

  /**
   * Constructor wraps around another tranpsort
   */
  public TFramedTransport(TTransport transport, int maxLength) {
    transport_ = transport;
    maxLength_ = maxLength;
  }

  public TFramedTransport(TTransport transport) {
    transport_ = transport;
    maxLength_ = TFramedTransport.DEFAULT_MAX_LENGTH;
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

  private final byte[] i32rd = new byte[4];
  private void readFrame() throws TTransportException {
    transport_.readAll(i32rd, 0, 4);
    int size =
      ((i32rd[0] & 0xff) << 24) |
      ((i32rd[1] & 0xff) << 16) |
      ((i32rd[2] & 0xff) <<  8) |
      ((i32rd[3] & 0xff));

    if (size < 0) {
      throw new TTransportException("Read a negative frame size (" + size + ")!");
    }

    if (size > maxLength_) {
      throw new TTransportException("Frame size (" + size + ") larger than max length (" + maxLength_ + ")!");
    }

    byte[] buff = new byte[size];
    transport_.readAll(buff, 0, size);
    readBuffer_.reset(buff);
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
