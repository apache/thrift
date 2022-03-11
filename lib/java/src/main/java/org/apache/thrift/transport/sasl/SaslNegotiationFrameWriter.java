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

import static org.apache.thrift.transport.sasl.SaslNegotiationHeaderReader.PAYLOAD_LENGTH_BYTES;
import static org.apache.thrift.transport.sasl.SaslNegotiationHeaderReader.STATUS_BYTES;

/**
 * Writer for sasl negotiation frames. It expect a status byte as header with a payload to be
 * written out (any header whose size is not equal to 1 would be considered as error).
 */
public class SaslNegotiationFrameWriter extends FrameWriter {

  public static final int HEADER_BYTES = STATUS_BYTES + PAYLOAD_LENGTH_BYTES;

  @Override
  public void withOnlyPayload(byte[] payload, int offset, int length) {
    throw new UnsupportedOperationException("Status byte is expected for sasl frame header.");
  }

  @Override
  protected ByteBuffer buildFrame(byte[] header, int headerOffset, int headerLength,
                                  byte[] payload, int payloadOffset, int payloadLength) {
    if (header == null || headerLength != STATUS_BYTES) {
      throw new IllegalArgumentException("Header " + StringUtils.bytesToHexString(header) +
          " does not have expected length " + STATUS_BYTES);
    }
    byte[] bytes = new byte[HEADER_BYTES + payloadLength];
    System.arraycopy(header, headerOffset, bytes, 0, STATUS_BYTES);
    EncodingUtils.encodeBigEndian(payloadLength, bytes, STATUS_BYTES);
    System.arraycopy(payload, payloadOffset, bytes, HEADER_BYTES, payloadLength);
    return ByteBuffer.wrap(bytes);
  }
}
