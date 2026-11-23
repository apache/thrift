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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.apache.thrift.TException;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

public class TestTJSONProtocol extends ProtocolTestBase {
  @Override
  protected TProtocolFactory getFactory() {
    return new TJSONProtocol.Factory();
  }

  @Override
  protected boolean canBeUsedNaked() {
    return false;
  }

  @Test
  public void testEscapedUnicode() throws TException {
    String jsonString = "\"hello unicode \\u0e01\\ud834\\udd1e world\"";
    String expectedString = "hello unicode \u0e01\ud834\udd1e world";

    TMemoryBuffer buffer = new TMemoryBuffer(1000);
    TJSONProtocol protocol = new TJSONProtocol(buffer);
    buffer.write(jsonString.getBytes(StandardCharsets.UTF_8));

    assertEquals(expectedString, protocol.readString());
  }

  @Test
  public void testEscapedUnicodeAboveU7FFF() throws TException {
    // Regression test for https://issues.apache.org/jira/browse/THRIFT-4408.
    // Escapes from \u8000 up to \uffff are code units with the top bit set.
    String jsonString = "\"\\uff1c\\u8000\\uffff\"";
    String expectedString = "\uff1c\u8000\uffff";

    TMemoryBuffer buffer = new TMemoryBuffer(1000);
    TJSONProtocol protocol = new TJSONProtocol(buffer);
    buffer.write(jsonString.getBytes(StandardCharsets.UTF_8));

    assertEquals(expectedString, protocol.readString());
  }

  @Test
  public void testUnpairedHighSurrogate() throws TException {
    // A high surrogate has to be followed by a low surrogate escape, whether the
    // string ends, continues with a plain character, or continues with another
    // non-surrogate escape.
    String[] jsonStrings = {"\"\\ud834\"", "\"\\ud834a\"", "\"\\ud834\\u0041\\udd1e\""};
    for (String jsonString : jsonStrings) {
      TMemoryBuffer buffer = new TMemoryBuffer(1000);
      TJSONProtocol protocol = new TJSONProtocol(buffer);
      buffer.write(jsonString.getBytes(StandardCharsets.UTF_8));

      TProtocolException e = assertThrows(TProtocolException.class, protocol::readString);
      assertEquals(TProtocolException.INVALID_DATA, e.getType());
    }
  }

  @Test
  public void testExactlySizedBuffer() throws TException {
    // Regression test for https://issues.apache.org/jira/browse/THRIFT-5383.
    // Ensures that a JSON string can be read after writing to a buffer just
    // large enough to contain it.
    String inputString = "abcdefg";
    TMemoryBuffer buffer = new TMemoryBuffer(inputString.length() + 2);

    TJSONProtocol protocol = new TJSONProtocol(buffer);
    protocol.writeString(inputString);
    String outputString = protocol.readString();

    assertEquals(inputString, outputString);
  }

  private interface Writes {
    void to(TProtocol protocol) throws TException;
  }

  // What an unlimited JSON protocol writes.
  private static byte[] written(Writes writes) throws TException {
    TMemoryBuffer buffer = new TMemoryBuffer(64);
    writes.to(new TJSONProtocol(buffer));
    return Arrays.copyOf(buffer.getArray(), buffer.length());
  }

  private static TJSONProtocol limited(
      byte[] json, long stringLengthLimit, long containerLengthLimit) throws TException {
    return new TJSONProtocol(
        new TMemoryInputTransport(json), stringLengthLimit, containerLengthLimit);
  }

  private static void assertRefused(int type, Executable read) {
    TProtocolException e = assertThrows(TProtocolException.class, read);
    assertEquals(type, e.getType());
  }

  private static String characters(int count) {
    StringBuilder builder = new StringBuilder(count);
    for (int i = 0; i < count; i++) {
      builder.append((char) ('a' + i % 26));
    }
    return builder.toString();
  }

  @Test
  public void testStringLengthLimit() throws TException {
    byte[] ten = written(p -> p.writeString(characters(10)));
    byte[] eleven = written(p -> p.writeString(characters(11)));

    assertEquals(characters(10), limited(ten, 10, -1).readString());
    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(eleven, 10, -1).readString());
    assertEquals(characters(11), limited(eleven, 11, -1).readString());
  }

  @Test
  public void testStringLengthLimitCountsUnescapedBytes() throws TException {
    // Six escaped e-acute are 36 characters on the wire and twelve bytes once unescaped.
    byte[] escaped =
        "\"\\u00e9\\u00e9\\u00e9\\u00e9\\u00e9\\u00e9\"".getBytes(StandardCharsets.UTF_8);

    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(escaped, 11, -1).readString());
    assertEquals("éééééé", limited(escaped, 12, -1).readString());
  }

  @Test
  public void testStringLengthLimitAppliesBeforeTheStringEnds() throws TException {
    // A string that never ends is refused once it passes the limit, not read to the end of the
    // input.
    byte[] unterminated = ("\"" + characters(1000)).getBytes(StandardCharsets.UTF_8);

    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(unterminated, 10, -1).readString());
    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(unterminated, 10, -1).readBinary());
  }

  @Test
  public void testBinaryLengthLimit() throws TException {
    // Ten bytes take sixteen characters of base64; the limit is on the bytes.
    byte[] ten = written(p -> p.writeBinary(ByteBuffer.wrap(new byte[10])));
    byte[] eleven = written(p -> p.writeBinary(ByteBuffer.wrap(new byte[11])));

    assertEquals(10, limited(ten, 10, -1).readBinary().remaining());
    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(eleven, 10, -1).readBinary());
    assertEquals(11, limited(eleven, 11, -1).readBinary().remaining());
  }

  @Test
  public void testMessageNameLengthLimit() throws TException {
    byte[] message =
        written(
            p -> {
              p.writeMessageBegin(new TMessage(characters(11), TMessageType.CALL, 1));
              p.writeMessageEnd();
            });

    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(message, 10, -1).readMessageBegin());
    assertEquals(characters(11), limited(message, 11, -1).readMessageBegin().name);
  }

  @Test
  public void testContainerLengthLimit() throws TException {
    byte[] list =
        written(
            p -> {
              p.writeListBegin(new TList(TType.I32, 11));
              for (int i = 0; i < 11; i++) {
                p.writeI32(i);
              }
              p.writeListEnd();
            });
    byte[] set =
        written(
            p -> {
              p.writeSetBegin(new TSet(TType.I32, 11));
              for (int i = 0; i < 11; i++) {
                p.writeI32(i);
              }
              p.writeSetEnd();
            });
    byte[] map =
        written(
            p -> {
              p.writeMapBegin(new TMap(TType.I32, TType.I32, 11));
              for (int i = 0; i < 11; i++) {
                p.writeI32(i);
                p.writeI32(i);
              }
              p.writeMapEnd();
            });

    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(list, -1, 10).readListBegin());
    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(set, -1, 10).readSetBegin());
    assertRefused(TProtocolException.SIZE_LIMIT, () -> limited(map, -1, 10).readMapBegin());
    assertEquals(11, limited(list, -1, 11).readListBegin().size);
    assertEquals(11, limited(set, -1, 11).readSetBegin().size);
    assertEquals(11, limited(map, -1, 11).readMapBegin().size);
  }

  @Test
  public void testNegativeContainerLength() {
    byte[] list = "[\"i32\",-1]".getBytes(StandardCharsets.UTF_8);

    assertRefused(
        TProtocolException.NEGATIVE_SIZE,
        () -> new TJSONProtocol(new TMemoryInputTransport(list)).readListBegin());
  }

  @Test
  public void testLengthLimitsLeaveTypeNamesAlone() throws TException {
    // "i32" in the list header is part of the protocol's syntax, not a string value.
    byte[] list =
        written(
            p -> {
              p.writeListBegin(new TList(TType.I32, 1));
              p.writeI32(7);
              p.writeListEnd();
            });
    TJSONProtocol protocol = limited(list, 1, 1);

    assertEquals(1, protocol.readListBegin().size);
    assertEquals(7, protocol.readI32());
  }

  @Test
  public void testFactoryCarriesLengthLimits() throws TException {
    byte[] eleven = written(p -> p.writeString(characters(11)));

    assertRefused(
        TProtocolException.SIZE_LIMIT,
        () ->
            new TJSONProtocol.Factory(10, -1)
                .getProtocol(new TMemoryInputTransport(eleven))
                .readString());
    assertRefused(
        TProtocolException.SIZE_LIMIT,
        () ->
            new TJSONProtocol.Factory(true, 10, -1)
                .getProtocol(new TMemoryInputTransport(eleven))
                .readString());
    assertEquals(
        characters(11),
        new TJSONProtocol.Factory().getProtocol(new TMemoryInputTransport(eleven)).readString());
  }

  @Test
  public void testNoLengthLimitByDefault() throws TException {
    String string = characters(100_000);
    byte[] json =
        written(
            p -> {
              p.writeString(string);
              p.writeListBegin(new TList(TType.BYTE, 100_000));
              for (int i = 0; i < 100_000; i++) {
                p.writeByte((byte) i);
              }
              p.writeListEnd();
            });
    TJSONProtocol protocol = new TJSONProtocol(new TMemoryInputTransport(json));

    assertEquals(string, protocol.readString());
    assertEquals(100_000, protocol.readListBegin().size);
  }
}
