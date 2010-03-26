
package org.apache.thrift.transport;

import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

public class WriteCountingTransport extends TTransport {
  public int writeCount = 0;
  private final TTransport trans;

  public WriteCountingTransport(TTransport underlying) {
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
    return 0;
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {
    writeCount ++;
    trans.write(buf, off, len);
  }
}