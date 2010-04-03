package org.apache.thrift.transport;

public final class TMemoryInputTransport extends TTransport {

  private byte[] buf_;
  private int pos_;
  private int endPos_;

  public TMemoryInputTransport() {
  }

  public TMemoryInputTransport(byte[] buf) {
    reset(buf);
  }

  public TMemoryInputTransport(byte[] buf, int offset, int length) {
    reset(buf, offset, length);
  }

  public void reset(byte[] buf) {
    reset(buf, 0, buf.length);
  }

  public void reset(byte[] buf, int offset, int length) {
    buf_ = buf;
    pos_ = offset;
    endPos_ = offset + length;
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
    return endPos_ - pos_;
  }

  public void consumeBuffer(int len) {
    pos_ += len;
  }

}
