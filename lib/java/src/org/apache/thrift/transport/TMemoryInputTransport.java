package org.apache.thrift.transport;

public class TMemoryInputTransport extends TTransport {

  private byte[] buf_;
  private int pos_;

  public TMemoryInputTransport(byte[] buf) {
    reset(buf);
  }

  public void reset(byte[] buf) {
    buf_ = buf;
    pos_ = 0;
  }

  @Override
  public void close() {}

  @Override
  public boolean isOpen() {
    return true;
  }

  @Override
  public void open() throws TTransportException {}

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    int bytesRemaining = getBytesRemainingInBuffer();
    int amtToRead = (len > bytesRemaining ? bytesRemaining : len);
    if (amtToRead > 0) {
      System.arraycopy(buf_, pos_, buf, off, amtToRead);
      consumeBuffer(amtToRead);
    }
    return amtToRead;
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {
    throw new UnsupportedOperationException("No writing allowed!");
  }

  @Override
  public byte[] getBuffer() {
    return buf_;
  }

  public int getBufferPosition() {
    return pos_;
  }

  public int getBytesRemainingInBuffer() {
    return buf_.length - pos_;
  }

  public void consumeBuffer(int len) {
    pos_ += len;
  }

}
