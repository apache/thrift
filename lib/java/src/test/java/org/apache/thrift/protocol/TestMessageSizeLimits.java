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

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.transport.AutoExpandingBufferReadTransport;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.api.Test;

/**
 * Tests that message size limits are enforced in the fast-path buffer reads of
 * TBinaryProtocol/TCompactProtocol and that consumeBuffer() correctly tracks consumed bytes.
 */
public class TestMessageSizeLimits {

  /** Build TBinaryProtocol-encoded bytes for a string: 4-byte big-endian length + raw bytes. */
  private byte[] encodeBinaryString(String s) {
    byte[] data = s.getBytes(java.nio.charset.StandardCharsets.UTF_8);
    byte[] out = new byte[4 + data.length];
    int len = data.length;
    out[0] = (byte) (len >> 24);
    out[1] = (byte) (len >> 16);
    out[2] = (byte) (len >> 8);
    out[3] = (byte) len;
    System.arraycopy(data, 0, out, 4, data.length);
    return out;
  }

  /**
   * Build TCompactProtocol-encoded bytes for a string: varint length + raw bytes. Assumes length <
   * 128 so it fits in one varint byte.
   */
  private byte[] encodeCompactString(String s) {
    byte[] data = s.getBytes(java.nio.charset.StandardCharsets.UTF_8);
    assert data.length < 128 : "test helper only handles single-byte varints";
    byte[] out = new byte[1 + data.length];
    out[0] = (byte) data.length;
    System.arraycopy(data, 0, out, 1, data.length);
    return out;
  }

  private static String repeat(char c, int n) {
    char[] chars = new char[n];
    Arrays.fill(chars, c);
    return new String(chars);
  }

  @Test
  public void testBinaryProtocol_stringLengthLimitEnforcedInFastPath() throws Exception {
    // 100-char string encoded for TBinaryProtocol - all bytes in transport so fast path fires
    byte[] buf = encodeBinaryString(repeat('A', 100));
    TMemoryInputTransport transport = new TMemoryInputTransport(buf);

    // Protocol limited to 10 bytes per string
    TBinaryProtocol proto = new TBinaryProtocol(transport, 10L, -1L, false, true);

    assertThrows(
        TProtocolException.class,
        proto::readString,
        "stringLengthLimit must be enforced even when the fast path is taken");
  }

  @Test
  public void testBinaryProtocol_stringLengthLimitAllowsValidString() throws Exception {
    byte[] buf = encodeBinaryString("Hello");
    TMemoryInputTransport transport = new TMemoryInputTransport(buf);

    TBinaryProtocol proto = new TBinaryProtocol(transport, 10L, -1L, false, true);

    assertDoesNotThrow(
        proto::readString, "string within stringLengthLimit must be readable via fast path");
  }

  @Test
  public void testCompactProtocol_stringLengthLimitEnforcedInFastPath() throws Exception {
    byte[] buf = encodeCompactString(repeat('A', 100));
    TMemoryInputTransport transport = new TMemoryInputTransport(buf);

    TCompactProtocol proto = new TCompactProtocol(transport, 10L, -1L);

    assertThrows(
        TProtocolException.class,
        proto::readString,
        "TCompactProtocol stringLengthLimit must reject oversized strings in fast path");
  }

  @Test
  public void testConsumeBuffer_decrementsRemainingMessageSize() throws Exception {
    // 20-byte transport; updateKnownMessageSize sets remainingMessageSize = 20
    byte[] buf = new byte[20];
    TMemoryInputTransport transport = new TMemoryInputTransport(buf);

    transport.consumeBuffer(15);

    // 5 bytes remain; requesting 5 must succeed
    assertDoesNotThrow(() -> transport.checkReadBytesAvailable(5));

    // requesting 6 must fail
    assertThrows(
        TTransportException.class,
        () -> transport.checkReadBytesAvailable(6),
        "checkReadBytesAvailable must reflect bytes consumed via consumeBuffer");
  }

  @Test
  public void testBinaryProtocol_fastPathReadsDrainRemainingMessageSize() throws Exception {
    // 10 i32 values = 40 bytes; maxMessageSize exactly 40
    byte[] buf = new byte[40];
    TConfiguration config = TConfiguration.custom().setMaxMessageSize(40).build();
    TMemoryInputTransport transport = new TMemoryInputTransport(config, buf);
    TBinaryProtocol proto = new TBinaryProtocol(transport);

    // Reading 8 i32 values via fast path consumes 32 bytes
    for (int i = 0; i < 8; i++) {
      proto.readI32();
    }

    // 8 bytes remain — requesting exactly 8 must succeed
    assertDoesNotThrow(
        () -> transport.checkReadBytesAvailable(8),
        "8 bytes should still be available after 32 consumed via fast path");

    // requesting 9 must fail — size limit is properly tracked
    assertThrows(
        TTransportException.class,
        () -> transport.checkReadBytesAvailable(9),
        "fast-path reads must decrement remaining message size so limits can be enforced");
  }

  @Test
  public void testRead_doesNotDoubleCountConsumedBytes() throws Exception {
    // If read() double-counted, consuming 10 bytes via read() on a 20-byte transport
    // would wrongly decrement remainingMessageSize by 20 instead of 10.
    byte[] buf = new byte[20];
    TMemoryInputTransport transport = new TMemoryInputTransport(buf);
    byte[] dest = new byte[10];
    transport.read(dest, 0, 10);

    // 10 bytes remain; requesting 10 must succeed
    assertDoesNotThrow(() -> transport.checkReadBytesAvailable(10));

    // requesting 11 must fail
    assertThrows(
        TTransportException.class,
        () -> transport.checkReadBytesAvailable(11),
        "read() must not double-count consumed bytes after consumeBuffer fix");
  }

  @Test
  public void testAutoExpandingBufferReadTransport_fillResetsMessageSizePerFrame()
      throws Exception {
    // fill() must reset remainingMessageSize so that consumption across frames on a long-lived
    // TFastFramedTransport connection does not accumulate toward the per-message limit.
    TConfiguration config = TConfiguration.custom().setMaxMessageSize(80).build();
    AutoExpandingBufferReadTransport readBuf = new AutoExpandingBufferReadTransport(config, 100);
    TMemoryInputTransport source = new TMemoryInputTransport(new byte[200]);

    // Frame 1: fill and consume 70 bytes
    readBuf.fill(source, 70);
    readBuf.consumeBuffer(70);

    // Frame 2: fill resets remainingMessageSize to maxMessageSize (80) before new frame
    readBuf.fill(source, 70);
    readBuf.consumeBuffer(70);

    // After two frames of 70 bytes each, 10 bytes of budget must still be available
    assertDoesNotThrow(
        () -> readBuf.checkReadBytesAvailable(10),
        "fill() must reset remainingMessageSize so multi-frame connections stay functional");
    assertThrows(
        TTransportException.class,
        () -> readBuf.checkReadBytesAvailable(11),
        "remaining budget after fill-reset and 70-byte consume should be exactly 10");
  }
}
