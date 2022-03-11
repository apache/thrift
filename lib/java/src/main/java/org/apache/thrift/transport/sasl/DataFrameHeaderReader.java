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

/**
 * The header for data frame, it only contains a 4-byte payload size.
 */
public class DataFrameHeaderReader extends FixedSizeHeaderReader {
  public static final int PAYLOAD_LENGTH_BYTES = 4;

  private int payloadSize;

  @Override
  protected int headerSize() {
    return PAYLOAD_LENGTH_BYTES;
  }

  @Override
  protected void onComplete() throws TInvalidSaslFrameException {
    payloadSize = byteBuffer.getInt(0);
    if (payloadSize < 0) {
      throw new TInvalidSaslFrameException("Payload size is negative: " + payloadSize);
    }
  }

  @Override
  public int payloadSize() {
    return payloadSize;
  }
}
