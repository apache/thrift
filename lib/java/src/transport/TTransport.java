package com.facebook.thrift.transport;

/**
 * Generic class that encapsulates the I/O layer. This is basically a thin
 * wrapper around the combined functionality of Java input/output streams.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public abstract class TTransport {

  /**
   * Queries whether the transport is open.
   *
   * @return True if the transport is open.
   */
  public abstract boolean isOpen();

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
   * Guarantees that all of len bytes are 
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
      if (ret == -1) {
        throw new TTransportException("Cannot read. Remote side has closed.");
      }
      got += ret;
    }
    return got;
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
