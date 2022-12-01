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

package org.apache.thrift;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import thrift.test.Opt30;
import thrift.test.Opt4;
import thrift.test.Opt64;
import thrift.test.Opt80;

// Exercises the isSet methods using structs from ManyOptionals.thrift
public class TestOptionals {
  @Test
  public void testEncodingUtils() throws Exception {
    assertEquals((short) 0x8, EncodingUtils.setBit((short) 0, 3, true));
    assertEquals((short) 0, EncodingUtils.setBit((short) 0x8, 3, false));
    assertTrue(EncodingUtils.testBit((short) 0x8, 3));
    assertFalse(EncodingUtils.testBit((short) 0x8, 4));

    assertEquals(Short.MIN_VALUE, EncodingUtils.setBit((short) 0, 15, true));
    assertEquals((short) 0, EncodingUtils.setBit(Short.MIN_VALUE, 15, false));
    assertTrue(EncodingUtils.testBit(Short.MIN_VALUE, 15));
    assertFalse(EncodingUtils.testBit(Short.MIN_VALUE, 14));
  }

  @Test
  public void testOpt4() throws Exception {
    Opt4 x = new Opt4();
    assertFalse(x.isSetDef1());
    x.setDef1(3);
    assertTrue(x.isSetDef1());
    assertFalse(x.isSetDef2());

    Opt4 copy = new Opt4(x);
    assertTrue(copy.isSetDef1());
    copy.unsetDef1();
    assertFalse(copy.isSetDef1());
    assertTrue(x.isSetDef1());
  }

  @Test
  public void testOpt30() throws Exception {
    Opt30 x = new Opt30();
    assertFalse(x.isSetDef1());
    x.setDef1(3);
    assertTrue(x.isSetDef1());
    assertFalse(x.isSetDef2());
  }

  @Test
  public void testOpt64() throws Exception {
    Opt64 x = new Opt64();
    assertFalse(x.isSetDef1());
    x.setDef1(3);
    assertTrue(x.isSetDef1());
    assertFalse(x.isSetDef2());
    x.setDef64(22);
    assertTrue(x.isSetDef64());
    assertFalse(x.isSetDef63());
  }

  @Test
  public void testOpt80() throws Exception {
    Opt80 x = new Opt80();
    assertFalse(x.isSetDef1());
    x.setDef1(3);
    assertTrue(x.isSetDef1());
    assertFalse(x.isSetDef2());

    Opt80 copy = new Opt80(x);
    copy.unsetDef1();
    assertFalse(copy.isSetDef1());
    assertTrue(x.isSetDef1());
  }
}
