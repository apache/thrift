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

package org.apache.thrift.partial;

import org.apache.thrift.partial.ExceptionAsserts;

import org.apache.thrift.TEnum;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test ThriftCodec serializes and deserializes thrift objects correctly.
 */
public class EnumCacheTest {

  enum TestEnum implements TEnum {
    Alice(-1),
    Bob(0),
    Charlie(1);

    private int value;

    TestEnum(int value) {
      this.value = value;
    }

    @Override
    public int getValue() {
      return this.value;
    }
  }

  static class NotEnum implements TEnum {

    public static final NotEnum Alice = new NotEnum(-11);
    public static final NotEnum Bob = new NotEnum(10);
    public static final NotEnum Charlie = new NotEnum(11);

    private static final NotEnum[] allValues = { Alice, Bob, Charlie };

    private int value;

    private NotEnum(int value) {
      this.value = value;
    }

    public static TEnum[] values() {
      return NotEnum.allValues;
    }

    @Override
    public int getValue() {
      return this.value;
    }

    @Override
    public String toString() {
      return String.format("NotEnum : %d", this.value);
    }
  }

  @Test
  public void testArgChecks() {
    EnumCache cache = new EnumCache();

    // Should not throw.
    cache.get(TestEnum.class, 0);

    // Verify it throws.
    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'enumClass' must not be null",
        () -> cache.get(null, 1));
  }

  @Test
  public void testGet() {
    EnumCache cache = new EnumCache();

    assertEquals(TestEnum.Alice, cache.get(TestEnum.class, -1));
    assertEquals(TestEnum.Bob, cache.get(TestEnum.class, 0));
    assertEquals(TestEnum.Charlie, cache.get(TestEnum.class, 1));

    assertEquals(NotEnum.Alice, cache.get(NotEnum.class, -11));
    assertEquals(NotEnum.Bob, cache.get(NotEnum.class, 10));
    assertEquals(NotEnum.Charlie, cache.get(NotEnum.class, 11));
  }

  @Test
  public void testGetInvalid() {
    EnumCache cache = new EnumCache();

    assertNull(cache.get(TestEnum.class, 42));
    assertNull(cache.get(NotEnum.class, 42));
  }
}
