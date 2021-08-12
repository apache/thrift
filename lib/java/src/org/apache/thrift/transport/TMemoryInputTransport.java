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

import org.apache.thrift.TConfiguration;

public final class TMemoryInputTransport extends TEndpointTransport {

  private byte[] buf_;
  private int pos_;
  private int endPos_;

  public TMemoryInputTransport() throws TTransportException {
    this(new TConfiguration());
  }

  public TMemoryInputTransport(TConfiguration _configuration) throws TTransportException {
    this(_configuration, new byte[0]);
  }

  public TMemoryInputTransport(byte[] buf) throws TTransportException {
    this(new TConfiguration(), buf);
  }

  public TMemoryInputTransport(TConfiguration _configuration, byte[] buf) throws TTransportException {
    this(_configuration, buf, 0, buf.length);
  }

  public TMemoryInputTransport(byte[] buf, int offset, int length) throws TTransportException {
    this(new TConfiguration(), buf, offset, length);
  }

  public TMemoryInputTransport(TConfiguration _configuration, byte[] buf, int offset, int length) throws TTransportException {
    super(_configuration);
    reset(buf, offset, length);
    updateKnownMessageSize(length);
  }

  public void reset(byte[] buf) {
    reset(buf, 0, buf.length);
  }

  public void reset(byte[] buf, int offset, int length) {
    buf_ = buf;
    pos_ = offset;
    endPos_ = offset + length;
    try {
      resetConsumedMessageSize(-1);
    } catch (TTransportException e) {
      // ignore
    }
  }

  public void clear() {
    buf_ = null;
    try {
      resetConsumedMessageSize(-1);
    } catch (TTransportException e) {
      // ignore
    }
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
      countConsumedMessageBytes(amtToRead);
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
