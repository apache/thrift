// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.transport;

import java.io.ByteArrayInputStream;

import org.apache.thrift.TByteArrayOutputStream;

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 *
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
