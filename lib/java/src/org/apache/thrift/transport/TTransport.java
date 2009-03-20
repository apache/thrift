// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.transport;

/**
 * Generic class that encapsulates the I/O layer. This is basically a thin
 * wrapper around the combined functionality of Java input/output streams.
 *
 */
public abstract class TTransport {

  /**
   * Queries whether the transport is open.
   *
   * @return True if the transport is open.
   */
  public abstract boolean isOpen();

  /**
   * Is there more data to be read?
   *
   * @return True if the remote side is still alive and feeding us
   */
  public boolean peek() {
    return isOpen();
  }

  /**
   * Opens the transport for reading/writing.
   *
   * @throws TTransportException if the transport could not be opened
   */
  public abstract void open()
    throws TTransportException;

  /**
   * Closes the transport.
   */
  public abstract void close();

  /**
   * Reads up to len bytes into buffer buf, starting att offset off.
   *
   * @param buf Array to read into
   * @param off Index to start reading at
   * @param len Maximum number of bytes to read
   * @return The number of bytes actually read
   * @throws TTransportException if there was an error reading data
   */
  public abstract int read(byte[] buf, int off, int len)
    throws TTransportException;

  /**
   * Guarantees that all of len bytes are actually read off the transport.
   *
   * @param buf Array to read into
   * @param off Index to start reading at
   * @param len Maximum number of bytes to read
   * @return The number of bytes actually read, which must be equal to len
   * @throws TTransportException if there was an error reading data
   */
  public int readAll(byte[] buf, int off, int len)
    throws TTransportException {
    int got = 0;
    int ret = 0;
    while (got < len) {
      ret = read(buf, off+got, len-got);
      if (ret <= 0) {
        throw new TTransportException("Cannot read. Remote side has closed. Tried to read " + len + " bytes, but only got " + got + " bytes.");
      }
      got += ret;
    }
    return got;
  }

  /**
   * Writes the buffer to the output
   *
   * @param buf The output data buffer
   * @throws TTransportException if an error occurs writing data
   */
  public void write(byte[] buf) throws TTransportException {
    write(buf, 0, buf.length);
  }

  /**
   * Writes up to len bytes from the buffer.
   *
   * @param buf The output data buffer
   * @param off The offset to start writing from
   * @param len The number of bytes to write
   * @throws TTransportException if there was an error writing data
   */
  public abstract void write(byte[] buf, int off, int len)
    throws TTransportException;

  /**
   * Flush any pending data out of a transport buffer.
   *
   * @throws TTransportException if there was an error writing out data.
   */
  public void flush()
    throws TTransportException {}
}
