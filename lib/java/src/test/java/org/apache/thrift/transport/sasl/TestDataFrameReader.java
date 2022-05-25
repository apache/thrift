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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.ByteBuffer;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.api.Test;

public class TestDataFrameReader {

  @Test
  public void testRead() throws TTransportException {
    // Prepare data
    int payloadSize = 23;
    ByteBuffer buffer =
        ByteBuffer.allocate(DataFrameHeaderReader.PAYLOAD_LENGTH_BYTES + payloadSize);
    buffer.putInt(payloadSize);
    for (int i = 0; i < payloadSize; i++) {
      buffer.put((byte) i);
    }
    buffer.rewind();

    TMemoryInputTransport transport = new TMemoryInputTransport();
    DataFrameReader dataFrameReader = new DataFrameReader();
    // No bytes received.
    dataFrameReader.read(transport);
    assertFalse(dataFrameReader.isComplete(), "No bytes received");
    assertFalse(dataFrameReader.getHeader().isComplete(), "No bytes received");
    // Payload size (header) and part of the payload are received.
    transport.reset(buffer.array(), 0, 6);
    dataFrameReader.read(transport);
    assertFalse(dataFrameReader.isComplete(), "Only header is complete");
    assertTrue(dataFrameReader.getHeader().isComplete(), "Header should be complete");
    assertEquals(
        payloadSize,
        dataFrameReader.getHeader().payloadSize(),
        "Payload size should be " + payloadSize);
    // Read the rest of payload.
    transport.reset(buffer.array(), 6, 21);
    dataFrameReader.read(transport);
    assertTrue(dataFrameReader.isComplete(), "Reader should be complete");
    buffer.position(DataFrameHeaderReader.PAYLOAD_LENGTH_BYTES);
    assertEquals(
        buffer,
        ByteBuffer.wrap(dataFrameReader.getPayload()),
        "Payload should be the same as from the transport");
  }
}
