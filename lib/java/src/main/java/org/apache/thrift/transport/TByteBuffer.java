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
