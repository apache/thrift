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

import java.nio.ByteBuffer;

import org.apache.thrift.EncodingUtils;
import org.apache.thrift.utils.StringUtils;

import static org.apache.thrift.transport.sasl.DataFrameHeaderReader.PAYLOAD_LENGTH_BYTES;

/**
 * Write frames of thrift messages. It expects an empty/null header to be provided with a payload
 * to be written out. Non empty headers are considered as error.
 */
public class DataFrameWriter extends FrameWriter {

  @Override
  public void withOnlyPayload(byte[] payload, int offset, int length) {
    if (!isComplete()) {
      throw new IllegalStateException("Previsous write is not yet complete, with " +
          frameBytes.remaining() + " bytes left.");
    }
    frameBytes = buildFrameWithPayload(payload, offset, length);
  }

  @Override
  protected ByteBuffer buildFrame(byte[] header, int headerOffset, int headerLength,
                                  byte[] payload, int payloadOffset, int payloadLength) {
    if (header != null && headerLength > 0) {
      throw new IllegalArgumentException("Extra header [" + StringUtils.bytesToHexString(header) +
          "] offset " + payloadOffset + " length " + payloadLength);
    }
    return buildFrameWithPayload(payload, payloadOffset, payloadLength);
  }

  private ByteBuffer buildFrameWithPayload(byte[] payload, int offset, int length) {
    byte[] bytes = new byte[PAYLOAD_LENGTH_BYTES + length];
    EncodingUtils.encodeBigEndian(length, bytes, 0);
    System.arraycopy(payload, offset, bytes, PAYLOAD_LENGTH_BYTES, length);
    return ByteBuffer.wrap(bytes);
  }
}
