// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.transport;

import java.io.ByteArrayInputStream;

import com.facebook.thrift.TByteArrayOutputStream;

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TFramedTransport extends TTransport {

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
  private ByteArrayInputStream readBuffer_ = null;

  /**
   * Whether to frame input
   */
  private boolean frameRead_ = true;

  /**
   * Whether to frame output
   */
  private boolean frameWrite_ = true;

  /**
   * Constructor wraps around another tranpsort
   */
  public TFramedTransport(TTransport transport) {
    this(transport, true, true);
  }

  /**
   * Constructor wraps around another tranpsort
   */
  public TFramedTransport(TTransport transport, boolean in, boolean out) {
    transport_ = transport;
    frameRead_ = in;
    frameWrite_ = out;
  }

  public void setFrameRead(boolean frameRead) {
    frameRead_ = frameRead;
  }

  public void setFrameWrite(boolean frameWrite) {
    frameWrite_ = frameWrite;
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
    if (!frameRead_) {
      return transport_.read(buf, off, len);
    }

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

    byte[] buff = new byte[size];
    transport_.readAll(buff, 0, size);
    readBuffer_ = new ByteArrayInputStream(buff);
  }

  public void write(byte[] buf, int off, int len) throws TTransportException {
    if (!frameWrite_) {
      transport_.write(buf, off, len);
      return;
    }
    writeBuffer_.write(buf, off, len);
  }

  public void flush() throws TTransportException {
    if (!frameWrite_) {
      transport_.flush();
      return;
    }

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
