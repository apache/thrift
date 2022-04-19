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

/**
 * TTransport for reading from an AutoExpandingBuffer.
 */
public class AutoExpandingBufferReadTransport extends TEndpointTransport {

  private final AutoExpandingBuffer buf;

  private int pos = 0;
  private int limit = 0;

  public AutoExpandingBufferReadTransport(TConfiguration config, int initialCapacity) throws TTransportException {
    super(config);
    this.buf = new AutoExpandingBuffer(initialCapacity);
  }

  public void fill(TTransport inTrans, int length) throws TTransportException {
    buf.resizeIfNecessary(length);
    inTrans.readAll(buf.array(), 0, length);
    pos = 0;
    limit = length;
  }

  @Override
  public void close() {}

  @Override
  public boolean isOpen() { return true; }

  @Override
  public void open() throws TTransportException {}

  @Override
  public final int read(byte[] target, int off, int len) throws TTransportException {
    int amtToRead = Math.min(len, getBytesRemainingInBuffer());
    if(amtToRead > 0){
      System.arraycopy(buf.array(), pos, target, off, amtToRead);
      consumeBuffer(amtToRead);
    }
    return amtToRead;
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {
    throw new UnsupportedOperationException();
  }

  @Override
  public final void consumeBuffer(int len) {
    pos += len;
  }

  @Override
  public final byte[] getBuffer() {
    return buf.array();
  }

  @Override
  public final int getBufferPosition() {
    return pos;
  }

  @Override
  public final int getBytesRemainingInBuffer() {
    return limit - pos;
  }
}
