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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.ByteBuffer;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.api.Test;

public class TestSaslNegotiationFrameReader {

  @Test
  public void testRead() throws TTransportException {
    TMemoryInputTransport transport = new TMemoryInputTransport();
    SaslNegotiationFrameReader negotiationReader = new SaslNegotiationFrameReader();
    // No bytes received
    negotiationReader.read(transport);
    assertFalse(negotiationReader.isComplete(), "No bytes received");
    assertFalse(negotiationReader.getHeader().isComplete(), "No bytes received");
    // Read header
    ByteBuffer buffer = ByteBuffer.allocate(5);
    buffer.put(0, NegotiationStatus.OK.getValue());
    buffer.putInt(1, 10);
    transport.reset(buffer.array());
    negotiationReader.read(transport);
    assertFalse(negotiationReader.isComplete(), "Only header is complete");
    assertTrue(negotiationReader.getHeader().isComplete(), "Header should be complete");
    assertEquals(10, negotiationReader.getHeader().payloadSize(), "Payload size should be 10");
    // Read payload
    transport.reset(new byte[20]);
    negotiationReader.read(transport);
    assertTrue(negotiationReader.isComplete(), "Reader should be complete");
    assertEquals(10, negotiationReader.getPayload().length, "Payload length should be 10");
  }

  @Test
  public void testReadInvalidNegotiationStatus() throws TTransportException {
    byte[] bytes = new byte[5];
    // Invalid status byte.
    bytes[0] = -1;
    TMemoryInputTransport transport = new TMemoryInputTransport(bytes);
    SaslNegotiationFrameReader negotiationReader = new SaslNegotiationFrameReader();
    assertThrows(
        TSaslNegotiationException.class,
        () -> {
          negotiationReader.read(transport);
        });
  }
}
