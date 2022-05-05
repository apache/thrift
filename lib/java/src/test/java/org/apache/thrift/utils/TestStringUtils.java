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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class TestStringUtils {

  @Test
  public void testToHexString() {
    byte[] bytes = {0x00, 0x1A, (byte) 0xEF, (byte) 0xAB, (byte) 0x92};
    assertEquals("001AEFAB92", StringUtils.bytesToHexString(bytes));
    assertEquals("EFAB92", StringUtils.bytesToHexString(bytes, 2, 3));
    assertNull(StringUtils.bytesToHexString(null));
  }

  private byte[] bytes;

  @BeforeEach
  public void setUp() throws Exception {
    bytes = new byte[] {1, 2, 3, 4, 5};
  }

  @Test
  public void testNegativeLength() {
    assertThrows(IllegalArgumentException.class, () -> StringUtils.bytesToHexString(bytes, 0, -1));
  }

  @Test
  public void testNegativeStartOffset() {
    assertThrows(IndexOutOfBoundsException.class, () -> StringUtils.bytesToHexString(bytes, -1, 1));
  }

  @Test
  public void testInvalidRange() {
    assertThrows(IndexOutOfBoundsException.class, () -> StringUtils.bytesToHexString(bytes, 5, 1));
  }
}
