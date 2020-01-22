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

import java.util.HashMap;
import java.util.Map;

import static org.apache.thrift.transport.sasl.TSaslNegotiationException.ErrorType.PROTOCOL_ERROR;

/**
 * Status bytes used during the initial Thrift SASL handshake.
 */
public enum NegotiationStatus {
  START((byte)0x01),
  OK((byte)0x02),
  BAD((byte)0x03),
  ERROR((byte)0x04),
  COMPLETE((byte)0x05);

  private static final Map<Byte, NegotiationStatus> reverseMap = new HashMap<>();

  static {
    for (NegotiationStatus s : NegotiationStatus.values()) {
      reverseMap.put(s.getValue(), s);
    }
  }

  private final byte value;

  NegotiationStatus(byte val) {
    this.value = val;
  }

  public byte getValue() {
    return value;
  }

  public static NegotiationStatus byValue(byte val) throws TSaslNegotiationException {
    if (!reverseMap.containsKey(val)) {
      throw new TSaslNegotiationException(PROTOCOL_ERROR, "Invalid status " + val);
    }
    return reverseMap.get(val);
  }
}
