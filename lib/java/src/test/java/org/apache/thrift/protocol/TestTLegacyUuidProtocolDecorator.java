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

package org.apache.thrift.protocol;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.util.Arrays;
import java.util.UUID;
import org.apache.thrift.TException;
import org.apache.thrift.transport.TMemoryBuffer;
import org.junit.jupiter.api.Test;

public class TestTLegacyUuidProtocolDecorator {

  private static final UUID TEST_UUID = UUID.fromString("00112233-4455-6677-8899-aabbccddeeff");

  /**
   * Check the correct way to wrap a TBinaryProtocol with legacy UUID byte-swapping behaviour for
   * systems that depend on the original wire format.
   *
   * <p>Use TLegacyUuidProtocolDecorator when: - communicating with a peer that was built against
   * the old TBinaryProtocol - reading data that was serialized with the old implementation
   *
   * <p>Use TBinaryProtocol directly when: - both peers are on the fixed implementation - starting a
   * new integration from scratch
   */
  @Test
  public void checkLegacyUuidRoundTrip() throws TException {
    TMemoryBuffer transport = new TMemoryBuffer(16);
    TProtocol protocol = new TLegacyUuidProtocolDecorator(new TBinaryProtocol(transport));

    protocol.writeUuid(TEST_UUID);
    UUID result = protocol.readUuid();

    assertEquals(TEST_UUID, result);
  }

  /**
   * Demonstrates that the legacy and fixed protocols produce DIFFERENT bytes on the wire, and are
   * therefore incompatible with each other.
   *
   * <p>This test exists to make that incompatibility explicit and visible.
   */
  @Test
  public void checkWireIncompatibility() throws TException {
    TMemoryBuffer legacyTransport = new TMemoryBuffer(16);
    TProtocol legacyProtocol =
        new TLegacyUuidProtocolDecorator(new TBinaryProtocol(legacyTransport));
    legacyProtocol.writeUuid(TEST_UUID);

    TMemoryBuffer fixedTransport = new TMemoryBuffer(16);
    TProtocol fixedProtocol = new TBinaryProtocol(fixedTransport);
    fixedProtocol.writeUuid(TEST_UUID);

    assertNotEquals(
        Arrays.toString(legacyTransport.getArray()),
        Arrays.toString(fixedTransport.getArray()),
        "Legacy and fixed protocols must produce different bytes");
  }

  /**
   * Check that a UUID written by the legacy protocol CANNOT be correctly read back by the fixed
   * protocol, and vice versa.
   */
  @Test
  public void checkCrossProtocolReadFailure() throws TException {
    TMemoryBuffer transport = new TMemoryBuffer(16);

    // write with legacy, read with fixed
    TProtocol legacyWriter = new TLegacyUuidProtocolDecorator(new TBinaryProtocol(transport));
    legacyWriter.writeUuid(TEST_UUID);

    TProtocol fixedReader = new TBinaryProtocol(transport);
    UUID result = fixedReader.readUuid();

    assertNotEquals(
        TEST_UUID,
        result,
        "Reading legacy bytes with the fixed protocol must produce a different UUID");
  }

  /** Check that a UUID written by the legacy protocol is byte swapped as expected. */
  @Test
  public void checkLegacySwapping() throws TException {
    TMemoryBuffer transport = new TMemoryBuffer(16);

    // write with legacy, read with fixed
    TProtocol protocol = new TLegacyUuidProtocolDecorator(new TBinaryProtocol(transport));
    protocol.writeUuid(TEST_UUID);

    // The legacy implementation writes LSB first, then MSB - which is the wrong
    // order
    byte[] expected = {
      (byte) 0x88, (byte) 0x99, (byte) 0xaa, (byte) 0xbb, // LSB high bytes
      (byte) 0xcc, (byte) 0xdd, (byte) 0xee, (byte) 0xff, // LSB low bytes
      (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33, // MSB high bytes
      (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77, // MSB low bytes
    };

    byte[] actual = transport.getArray();
    assertArrayEquals(expected, Arrays.copyOf(actual, 16));
  }

  /**
   * Check that a UUID written by the correct protocol is as expected
   *
   * <p>This test is probably out of place
   */
  @Test
  public void checkCorrectSwapping() throws TException {
    TMemoryBuffer transport = new TMemoryBuffer(16);
    TProtocol protocol = new TBinaryProtocol(transport);
    protocol.writeUuid(TEST_UUID);

    byte[] expected = {
      (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
      (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77,
      (byte) 0x88, (byte) 0x99, (byte) 0xaa, (byte) 0xbb,
      (byte) 0xcc, (byte) 0xdd, (byte) 0xee, (byte) 0xff,
    };

    byte[] actual = transport.getArray();
    assertArrayEquals(expected, Arrays.copyOf(actual, 16));
  }
}
