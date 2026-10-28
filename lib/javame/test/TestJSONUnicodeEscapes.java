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

import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.transport.TMemoryBuffer;

/**
 * A JSON string can carry any UTF-16 code unit as an escape: a backslash, 'u'
 * and four hex digits. The reader has to turn each escape back into the UTF-8
 * encoding of that character, in the upper half of the Basic Multilingual
 * Plane (U+8000 to U+FFFF) as well as below it, and a surrogate pair into the
 * supplementary character it stands for.
 */
public class TestJSONUnicodeEscapes {

  private static int checks = 0;
  private static int failures = 0;

  private static void check(boolean ok, String what) {
    checks++;
    if (ok) {
      System.out.println("ok   - " + what);
    } else {
      failures++;
      System.out.println("FAIL - " + what);
    }
  }

  private static final char BACKSLASH = (char) 92;

  private static String hex4(int codeUnit) {
    String hex = Integer.toHexString(codeUnit);
    while (hex.length() < 4) {
      hex = "0" + hex;
    }
    return hex;
  }

  /** The JSON escape of one UTF-16 code unit, with lowercase hex digits. */
  private static String escape(int codeUnit) {
    return BACKSLASH + "u" + hex4(codeUnit);
  }

  private static String describe(String s) {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < s.length(); i++) {
      if (i > 0) {
        b.append(' ');
      }
      b.append("U+").append(hex4(s.charAt(i)));
    }
    return "[" + b.toString() + "]";
  }

  /** Reads one JSON string value, given as the text between its quotes. */
  private static String read(String body) throws Exception {
    byte[] json = ('"' + body + '"').getBytes("UTF-8");
    TMemoryBuffer buffer = new TMemoryBuffer(json.length);
    buffer.write(json, 0, json.length);
    return new TJSONProtocol(buffer).readString();
  }

  private static void checkRead(String body, String expected, String what) {
    String actual = null;
    String problem = null;
    try {
      actual = read(body);
    } catch (Exception e) {
      problem = e.toString();
    }
    boolean ok = expected.equals(actual);
    check(ok, what + (ok ? "" : ", got " + (problem != null ? problem : describe(actual))));
  }

  private static void checkEscape(int codeUnit) {
    checkRead(escape(codeUnit), String.valueOf((char) codeUnit),
        "the escape of U+" + hex4(codeUnit) + " reads as that character");
  }

  public static void main(String[] args) throws Exception {
    // below U+8000
    int[] lower = { 0x0041, 0x00e9, 0x07ff, 0x0800, 0x7fff };
    for (int i = 0; i < lower.length; i++) {
      checkEscape(lower[i]);
    }

    // from U+8000 up, below and above the surrogate range
    int[] upper = { 0x8000, 0xd7ff, 0xe000, 0x9a41, 0xffff };
    for (int i = 0; i < upper.length; i++) {
      checkEscape(upper[i]);
    }

    // a surrogate pair, U+1F600
    String pair = String.valueOf(new char[] { (char) 0xd83d, (char) 0xde00 });
    checkRead(escape(0xd83d) + escape(0xde00), pair,
        "the escapes of a surrogate pair read as the supplementary character");

    // escapes mixed with plain characters
    String expected = "a" + (char) 0x00e9 + (char) 0x8000 + "b" + (char) 0x9a41 + pair + "c";
    checkRead("a" + escape(0x00e9) + escape(0x8000) + "b" + escape(0x9a41)
        + escape(0xd83d) + escape(0xde00) + "c", expected,
        "escapes between plain characters read in place");

    System.out.println("\n" + checks + " checks, " + failures + " failures");
    if (failures > 0) {
      System.exit(1);
    }
  }
}
