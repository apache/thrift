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

import static org.apache.thrift.transport.sasl.TSaslNegotiationException.ErrorType.PROTOCOL_ERROR;

/**
 * Header for sasl negotiation frames. It contains status byte of negotiation and a 4-byte integer
 * (payload size).
 */
public class SaslNegotiationHeaderReader extends FixedSizeHeaderReader {
  public static final int STATUS_BYTES = 1;
  public static final int PAYLOAD_LENGTH_BYTES = 4;

  private NegotiationStatus negotiationStatus;
  private int payloadSize;

  @Override
  protected int headerSize() {
    return STATUS_BYTES + PAYLOAD_LENGTH_BYTES;
  }

  @Override
  protected void onComplete() throws TSaslNegotiationException {
    negotiationStatus = NegotiationStatus.byValue(byteBuffer.get(0));
    payloadSize = byteBuffer.getInt(1);
    if (payloadSize < 0) {
      throw new TSaslNegotiationException(PROTOCOL_ERROR, "Payload size is negative: " + payloadSize);
    }
  }

  @Override
  public int payloadSize() {
    return payloadSize;
  }

  public NegotiationStatus getStatus() {
    return negotiationStatus;
  }
}
