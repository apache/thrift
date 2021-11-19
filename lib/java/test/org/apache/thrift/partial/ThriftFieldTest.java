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

import static org.junit.Assert.*;

import org.apache.thrift.partial.ExceptionAsserts;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

public class ThriftFieldTest {

  @Test
  public void testArgChecks() {
    ThriftField test;
    List<ThriftField> testFields;

    // Should not throw.
    test = new ThriftField("foo");
    test = new ThriftField("foo", Arrays.asList(new ThriftField("bar")));
    testFields = ThriftField.fromNames(Arrays.asList("foo"));

    // Verify it throws.
    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'name' must not be null",
        () -> new ThriftField(null, Collections.emptyList()));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fields' must not be null",
        () -> new ThriftField("foo", null));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fieldNames' must not be null",
        () -> ThriftField.fromNames(null));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fieldNames' must have at least one element",
        () -> ThriftField.fromNames(Collections.emptyList()));
  }

  @Test
  public void testFromNames() {
    List<String> fieldNames = Arrays.asList(
        "f1",
        "f2.f21",
        "f3.f31.f311",
        "f3.f32.f321",
        "f3.f32.f322"
    );

    List<ThriftField> testFields = ThriftField.fromNames(fieldNames);

    assertEquals(3, testFields.size());
    ThriftField f1 = testFields.get(0);
    ThriftField f2 = testFields.get(1);
    ThriftField f3 = testFields.get(2);
    assertEquals("f1", f1.name);
    assertEquals("f2", f2.name);
    assertEquals("f3", f3.name);

    assertEquals(0, f1.fields.size());
    assertEquals(1, f2.fields.size());
    assertEquals(2, f3.fields.size());

    ThriftField f21 = f2.fields.get(0);
    ThriftField f31 = f3.fields.get(0);
    ThriftField f32 = f3.fields.get(1);
    assertEquals("f21", f21.name);
    assertEquals("f31", f31.name);
    assertEquals("f32", f32.name);

    assertEquals(0, f21.fields.size());
    assertEquals(1, f31.fields.size());
    assertEquals(2, f32.fields.size());

    ThriftField f311 = f31.fields.get(0);
    ThriftField f321 = f32.fields.get(0);
    ThriftField f322 = f32.fields.get(1);
    assertEquals("f311", f311.name);
    assertEquals("f321", f321.name);
    assertEquals("f322", f322.name);

    assertEquals(0, f311.fields.size());
    assertEquals(0, f321.fields.size());
    assertEquals(0, f322.fields.size());
  }

  @Test
  public void testEquality() {
    List<String> fieldNames = Arrays.asList(
        "f1",
        "f2.f21",
        "f3.f31.f311",
        "f3.f32.f321",
        "f3.f32.f322"
    );

    List<ThriftField> testFields = ThriftField.fromNames(fieldNames);
    List<ThriftField> testFields2 = testFields;

    assertSame(testFields, testFields2);
    assertEquals(testFields, testFields2);

    List<ThriftField> testFields3 = ThriftField.fromNames(fieldNames);
    assertNotSame(testFields, testFields3);
    assertEquals(testFields, testFields3);
    assertEquals(testFields.hashCode(), testFields3.hashCode());

    List<String> fieldNamesDiff = Arrays.asList(
        "f1",
        "f2.f21",
        "f3.f31.f311",
        "f3.f32.f323",
        "f3.f32.f322"
    );

    List<ThriftField> testFields4 = ThriftField.fromNames(fieldNamesDiff);
    assertNotSame(testFields, testFields4);
    assertNotEquals(testFields, testFields4);
    assertNotEquals(testFields.hashCode(), testFields4.hashCode());
  }
}
