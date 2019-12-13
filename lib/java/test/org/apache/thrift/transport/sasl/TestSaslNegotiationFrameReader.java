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

import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransportException;
import org.junit.Assert;
import org.junit.Test;

import java.nio.ByteBuffer;

public class TestSaslNegotiationFrameReader {

  @Test
  public void testRead() throws TTransportException {
    TMemoryInputTransport transport = new TMemoryInputTransport();
    SaslNegotiationFrameReader negotiationReader = new SaslNegotiationFrameReader();
    // No bytes received
    negotiationReader.read(transport);
    Assert.assertFalse("No bytes received", negotiationReader.isComplete());
    Assert.assertFalse("No bytes received", negotiationReader.getHeader().isComplete());
    // Read header
    ByteBuffer buffer = ByteBuffer.allocate(5);
    buffer.put(0, NegotiationStatus.OK.getValue());
    buffer.putInt(1, 10);
    transport.reset(buffer.array());
    negotiationReader.read(transport);
    Assert.assertFalse("Only header is complete", negotiationReader.isComplete());
    Assert.assertTrue("Header should be complete", negotiationReader.getHeader().isComplete());
    Assert.assertEquals("Payload size should be 10", 10, negotiationReader.getHeader().payloadSize());
    // Read payload
    transport.reset(new byte[20]);
    negotiationReader.read(transport);
    Assert.assertTrue("Reader should be complete", negotiationReader.isComplete());
    Assert.assertEquals("Payload length should be 10", 10, negotiationReader.getPayload().length);
  }

  @Test (expected = TSaslNegotiationException.class)
  public void testReadInvalidNegotiationStatus() throws TTransportException {
    byte[] bytes = new byte[5];
    // Invalid status byte.
    bytes[0] = -1;
    TMemoryInputTransport transport = new TMemoryInputTransport(bytes);
    SaslNegotiationFrameReader negotiationReader = new SaslNegotiationFrameReader();
    negotiationReader.read(transport);
  }
}
