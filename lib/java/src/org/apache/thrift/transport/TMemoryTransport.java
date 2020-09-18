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

import java.nio.ByteBuffer;

import org.apache.thrift.TByteArrayOutputStream;
import org.apache.thrift.TConfiguration;

/**
 * In memory transport with separate buffers for input and output.
 */
public class TMemoryTransport extends TEndpointTransport {

  private final ByteBuffer inputBuffer;
  private final TByteArrayOutputStream outputBuffer;

  public TMemoryTransport(byte[] input) throws TTransportException {
    super(new TConfiguration());
    inputBuffer = ByteBuffer.wrap(input);
    outputBuffer = new TByteArrayOutputStream(1024);
    updateKnownMessageSize(input.length);
  }

  public TMemoryTransport(TConfiguration config, byte[] input) throws TTransportException {
    super(config);
    inputBuffer = ByteBuffer.wrap(input);
    outputBuffer = new TByteArrayOutputStream(1024);
    updateKnownMessageSize(input.length);
  }

  @Override
  public boolean isOpen() {
    return true;
  }

  /**
   * Opening on an in memory transport should have no effect.
   */
  @Override
  public void open() {
    // Do nothing.
  }

  @Override
  public void close() {
    // Do nothing.
  }

  @Override
  public int read(byte[] buf, int off, int len) throws TTransportException {
    checkReadBytesAvailable(len);
    int remaining = inputBuffer.remaining();
    if (remaining < len) {
      throw new TTransportException(TTransportException.END_OF_FILE,
          "There's only " + remaining + "bytes, but it asks for " + len);
    }
    inputBuffer.get(buf, off, len);
    return len;
  }

  @Override
  public void write(byte[] buf, int off, int len) throws TTransportException {
    outputBuffer.write(buf, off, len);
  }

  /**
   * Get all the bytes written by thrift output protocol.
   *
   * @return a byte array.
   */
  public TByteArrayOutputStream getOutput() {
    return outputBuffer;
  }
}
