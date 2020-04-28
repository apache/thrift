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

package org.apache.thrift.utils;

public final class StringUtils {

  private StringUtils() {
    // Utility class.
  }

  private static final char[] HEX_CHARS = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};

  /**
   * Stringify a byte array to the hex representation for each byte.
   *
   * @param bytes
   * @return hex string.
   */
  public static String bytesToHexString(byte[] bytes) {
    if (bytes == null) {
      return null;
    }
    return bytesToHexString(bytes, 0, bytes.length);
  }

  /**
   * Stringify a portion of the byte array.
   *
   * @param bytes byte array.
   * @param offset portion start.
   * @param length portion length.
   * @return hex string.
   */
  public static String bytesToHexString(byte[] bytes, int offset, int length) {
    if (length < 0) {
      throw new IllegalArgumentException("Negative length " + length);
    }
    if (offset < 0) {
      throw new IndexOutOfBoundsException("Negative start offset " + offset);
    }
    if (length > bytes.length - offset) {
      throw new IndexOutOfBoundsException("Invalid range, bytes.length: " + bytes.length + " offset: " + offset + " length: " + length);
    }
    char[] chars = new char[length * 2];
    for (int i = 0; i < length; i++) {
      int unsignedInt = bytes[i + offset] & 0xFF;
      chars[2 * i] = HEX_CHARS[unsignedInt >>> 4];
      chars[2 * i + 1] = HEX_CHARS[unsignedInt & 0x0F];
    }
    return new String(chars);
  }
}
