/**
 * 
 */
package org.apache.thrift.transport;

import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

public class ReadCountingTransport extends TTransport {
  public int readCount = 0;
  private TTransport trans;

  public ReadCountingTransport(TTransport underlying) {
    trans = underlying;
  }

  @Override
  public void close() {}

  @Override
  public boolean isOpen() {return true;}

  @Override
  public void open() throws TTransportException {}

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    readCount++;
    return trans.read(buf, off, len);
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {}
}