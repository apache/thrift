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

package org.apache.thrift.transport.sasl;

import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.utils.StringUtils;

import java.nio.ByteBuffer;

/**
 * Headers' size should be predefined.
 */
public abstract class FixedSizeHeaderReader implements FrameHeaderReader {

  protected final ByteBuffer byteBuffer = ByteBuffer.allocate(headerSize());

  @Override
  public boolean isComplete() {
    return !byteBuffer.hasRemaining();
  }

  @Override
  public void clear() {
    byteBuffer.clear();
  }

  @Override
  public byte[] toBytes() {
    if (!isComplete()) {
      throw new IllegalStateException("Header is not yet complete " + StringUtils.bytesToHexString(byteBuffer.array(), 0, byteBuffer.position()));
    }
    return byteBuffer.array();
  }

  @Override
  public boolean read(TTransport transport) throws TTransportException {
    FrameReader.readAvailable(transport, byteBuffer);
    if (byteBuffer.hasRemaining()) {
      return false;
    }
    onComplete();
    return true;
  }

  /**
   * @return Size of the header.
   */
  protected abstract int headerSize();

  /**
   * Actions (e.g. validation) to carry out when the header is complete.
   *
   * @throws TTransportException
   */
  protected abstract void onComplete() throws TTransportException;
}
