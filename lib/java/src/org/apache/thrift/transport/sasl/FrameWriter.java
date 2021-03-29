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

import java.io.IOException;
import java.nio.ByteBuffer;

import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * Write frame (header and payload) to transport in a nonblocking way.
 */
public abstract class FrameWriter {

  protected ByteBuffer frameBytes;

  /**
   * Provide (maybe empty) header and payload to the frame. This can be called only when isComplete
   * returns true (last frame has been written out).
   *
   * @param header Some extra header bytes (without the 4 bytes for payload length), which will be
   *               the start of the frame. It can be empty, depending on the message format
   * @param payload Payload as a byte array
   * @throws IllegalStateException if it is called when isComplete returns false
   * @throws IllegalArgumentException if header or payload is invalid
   */
  public void withHeaderAndPayload(byte[] header, byte[] payload) {
    if (payload == null) {
      payload = new byte[0];
    }
    if (header == null) {
      withOnlyPayload(payload);
    } else {
      withHeaderAndPayload(header, 0, header.length, payload, 0, payload.length);
    }
  }

  /**
   * Provide extra header and payload to the frame.
   *
   * @param header byte array containing the extra header
   * @param headerOffset starting offset of the header portition
   * @param headerLength length of the extra header
   * @param payload byte array containing the payload
   * @param payloadOffset starting offset of the payload portion
   * @param payloadLength length of the payload
   * @throws IllegalStateException if preivous frame is not yet complete (isComplete returns fals)
   * @throws IllegalArgumentException if header or payload is invalid
   */
  public void withHeaderAndPayload(byte[] header, int headerOffset, int headerLength,
                                   byte[] payload, int payloadOffset, int payloadLength) {
    if (!isComplete()) {
      throw new IllegalStateException("Previsous write is not yet complete, with " +
          frameBytes.remaining() + " bytes left.");
    }
    frameBytes = buildFrame(header, headerOffset, headerLength, payload, payloadOffset, payloadLength);
  }

  /**
   * Provide only payload to the frame. Throws UnsupportedOperationException if the frame expects
   * a header.
   *
   * @param payload payload as a byte array
   */
  public void withOnlyPayload(byte[] payload) {
    withOnlyPayload(payload, 0, payload.length);
  }

  /**
   * Provide only payload to the frame. Throws UnsupportedOperationException if the frame expects
   * a header.
   *
   * @param payload The underlying byte array as a recipient of the payload
   * @param offset The offset in the byte array starting from where the payload is located
   * @param length The length of the payload
   */
  public abstract void withOnlyPayload(byte[] payload, int offset, int length);

  protected abstract ByteBuffer buildFrame(byte[] header, int headerOffset, int headerLength,
                                           byte[] payload, int payloadOffset, int payloadLength);

  /**
   * Nonblocking write to the underlying transport.
   *
   * @throws TTransportException
   */
  public void write(TNonblockingTransport transport) throws TTransportException {
    transport.write(frameBytes);
  }

  /**
   *
   * @return true when no more data needs to be written out
   */
  public boolean isComplete() {
    return frameBytes == null || !frameBytes.hasRemaining();
  }

  /**
   * Release the byte buffer.
   */
  public void clear() {
    frameBytes = null;
  }
}
