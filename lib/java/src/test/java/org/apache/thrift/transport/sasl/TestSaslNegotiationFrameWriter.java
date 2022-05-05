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

import static org.apache.thrift.transport.sasl.SaslNegotiationFrameWriter.HEADER_BYTES;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.ByteBuffer;
import org.apache.thrift.EncodingUtils;
import org.junit.jupiter.api.Test;

public class TestSaslNegotiationFrameWriter {

  private static final byte[] PAYLOAD = {0x11, 0x08, 0x3F, 0x58, 0x73, 0x22, 0x00, (byte) 0xFF};

  @Test
  public void testWithHeaderAndPayload() {
    SaslNegotiationFrameWriter frameWriter = new SaslNegotiationFrameWriter();
    frameWriter.withHeaderAndPayload(new byte[] {NegotiationStatus.OK.getValue()}, PAYLOAD);
    byte[] expectedBytes = new byte[HEADER_BYTES + PAYLOAD.length];
    expectedBytes[0] = NegotiationStatus.OK.getValue();
    EncodingUtils.encodeBigEndian(PAYLOAD.length, expectedBytes, 1);
    System.arraycopy(PAYLOAD, 0, expectedBytes, HEADER_BYTES, PAYLOAD.length);
    assertEquals(ByteBuffer.wrap(expectedBytes), frameWriter.frameBytes);
  }

  @Test
  public void testWithInvalidHeaderLength() {
    SaslNegotiationFrameWriter frameWriter = new SaslNegotiationFrameWriter();
    assertThrows(
        IllegalArgumentException.class,
        () -> frameWriter.withHeaderAndPayload(new byte[5], 0, 2, PAYLOAD, 0, 1));
  }

  @Test
  public void testWithOnlyPayload() {
    SaslNegotiationFrameWriter frameWriter = new SaslNegotiationFrameWriter();
    assertThrows(
        UnsupportedOperationException.class, () -> frameWriter.withOnlyPayload(new byte[0]));
  }
}
