package org.apache.thrift.transport;

import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import org.apache.thrift.TConfiguration;

/** ByteBuffer-backed implementation of TTransport. */
public final class TByteBuffer extends TEndpointTransport {
  private final ByteBuffer byteBuffer;

  /**
   * Creates a new TByteBuffer wrapping a given NIO ByteBuffer and custom TConfiguration.
   *
   * @param configuration the custom TConfiguration.
   * @param byteBuffer the NIO ByteBuffer to wrap.
   * @throws TTransportException on error.
   */
  public TByteBuffer(TConfiguration configuration, ByteBuffer byteBuffer)
      throws TTransportException {
    super(configuration);
    this.byteBuffer = byteBuffer;
    updateKnownMessageSize(byteBuffer.capacity());
  }

  /**
   * Creates a new TByteBuffer wrapping a given NIO ByteBuffer.
   *
   * @param byteBuffer the NIO ByteBuffer to wrap.
   * @throws TTransportException on error.
   */
  public TByteBuffer(ByteBuffer byteBuffer) throws TTransportException {
    this(new TConfiguration(), byteBuffer);
  }

  @Override
  public boolean isOpen() {
    return true;
  }

  @Override
  public void open() {}

  @Override
  public void close() {}

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    //
    checkReadBytesAvailable(len);

    final int n = Math.min(byteBuffer.remaining(), len);
    if (n > 0) {
      try {
        byteBuffer.get(buf, off, n);
      } catch (BufferUnderflowException e) {
        throw new TTransportException("Unexpected end of input buffer", e);
      }
    }
    return n;
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {
    try {
      byteBuffer.put(buf, off, len);
    } catch (BufferOverflowException e) {
      throw new TTransportException("Not enough room in output buffer", e);
    }
  }

  /**
   * Gets the underlying NIO ByteBuffer.
   *
   * @return the underlying NIO ByteBuffer.
   */
  public ByteBuffer getByteBuffer() {
    return byteBuffer;
  }

  /**
   * Convenience method to call clear() on the underlying NIO ByteBuffer.
   *
   * @return this instance.
   */
  public TByteBuffer clear() {
    byteBuffer.clear();
    return this;
  }

  /**
   * Convenience method to call flip() on the underlying NIO ByteBuffer.
   *
   * @return this instance.
   */
  public TByteBuffer flip() {
    byteBuffer.flip();
    return this;
  }

  /**
   * Convenience method to convert the underlying NIO ByteBuffer to a plain old byte array.
   *
   * @return the byte array backing the underlying NIO ByteBuffer.
   */
  public byte[] toByteArray() {
    final byte[] data = new byte[byteBuffer.remaining()];
    byteBuffer.slice().get(data);
    return data;
  }
}
