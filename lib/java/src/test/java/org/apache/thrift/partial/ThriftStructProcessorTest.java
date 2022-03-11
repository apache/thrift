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

import org.apache.thrift.partial.TestStruct;
import org.apache.thrift.partial.ThriftField;
import org.apache.thrift.partial.ThriftMetadata;
import org.apache.thrift.partial.TstEnum;

import org.apache.thrift.TBase;
import org.apache.thrift.TException;
import org.apache.thrift.TFieldIdEnum;
import org.junit.Test;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ThriftStructProcessorTest {

  private PartialThriftTestData testData = new PartialThriftTestData();

  @Test
  public void testStruct() throws TException {
    List<ThriftField> fields = ThriftField.fromNames(Arrays.asList("i32Field"));
    ThriftMetadata.ThriftStruct metadata =
        ThriftMetadata.ThriftStruct.fromFields(TestStruct.class, fields);
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.createNewStruct(metadata);
    assertNotNull(instance);
    assertTrue(instance instanceof TBase);
    assertTrue(instance instanceof TestStruct);

    Object instance2 = processor.prepareStruct(instance);
    assertSame(instance, instance2);
  }

  @Test
  public void testList() throws TException {
    final int numItems = 10;
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.createNewList(numItems);
    assertNotNull(instance);
    assertTrue(instance instanceof Object[]);

    Object[] items = (Object[]) instance;
    for (int i = 0; i < numItems; i++) {
      assertNull(items[i]);
      processor.setListElement(instance, i, Integer.valueOf(i));
      assertEquals(i, items[i]);
    }

    assertTrue(processor.prepareList(instance) instanceof List<?>);
  }

  @Test
  public void testMap() throws TException {
    final int numItems = 10;
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.createNewMap(numItems);
    assertNotNull(instance);
    assertTrue(instance instanceof Map<?, ?>);

    Map<Object, Object> items = (Map<Object, Object>) instance;
    int ignoredIndex = -1;
    for (int i = 0; i < numItems; i++) {
      assertNull(items.get(i));
      processor.setMapElement(instance, ignoredIndex, Integer.valueOf(i), Integer.valueOf(i));
      assertEquals(i, items.get(i));
    }

    assertTrue(processor.prepareMap(instance) instanceof Map<?, ?>);
  }

  @Test
  public void testSet() throws TException {
    final int numItems = 10;
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.createNewSet(numItems);
    assertNotNull(instance);
    assertTrue(instance instanceof HashSet<?>);

    Set<?> items = (HashSet<?>) instance;
    int ignoredIndex = -1;

    for (int i = 0; i < numItems; i++) {
      assertFalse(items.contains(i));
      processor.setSetElement(instance, ignoredIndex, Integer.valueOf(i));
      assertTrue(items.contains(i));
    }

    assertTrue(processor.prepareSet(instance) instanceof Set<?>);
  }

  @Test
  public void testPrepareEnum() throws TException {
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.prepareEnum(TstEnum.class, 1);
    assertNotNull(instance);
    assertEquals(TstEnum.E_ONE, instance);

    instance = processor.prepareEnum(TstEnum.class, 2);
    assertNotNull(instance);
    assertEquals(TstEnum.E_TWO, instance);
  }

  @Test
  public void testPrepareString() throws TException {
    ThriftStructProcessor processor = new ThriftStructProcessor();
    ByteBuffer emptyBuffer = ByteBuffer.wrap(new byte[0]);
    Object instance = processor.prepareString(emptyBuffer);
    assertNotNull(instance);
    assertTrue(instance instanceof String);
    assertEquals("", instance);

    String value = "Hello world!";
    ByteBuffer buffer = ByteBuffer.wrap(value.getBytes(StandardCharsets.UTF_8));
    instance = processor.prepareString(buffer);
    assertNotNull(instance);
    assertTrue(instance instanceof String);
    assertEquals(value, instance);
  }

  @Test
  public void testPrepareBinary() throws TException {
    ThriftStructProcessor processor = new ThriftStructProcessor();
    ByteBuffer emptyBuffer = ByteBuffer.wrap(new byte[0]);
    Object instance = processor.prepareBinary(emptyBuffer);
    assertNotNull(instance);
    assertTrue(instance instanceof ByteBuffer);
    assertSame(emptyBuffer, instance);
  }

  @Test
  public void testStructPrimitiveFields() throws TException {
    List<ThriftField> fields = ThriftField.fromNames(
        Arrays.asList(
            "byteField",
            "i16Field",
            "i32Field",
            "i64Field",
            "doubleField",
            "stringField",

            "enumField",
            "binaryField"
        ));

    ThriftMetadata.ThriftStruct metadata =
        ThriftMetadata.ThriftStruct.fromFields(TestStruct.class, fields);
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.createNewStruct(metadata);
    assertNotNull(instance);
    assertTrue(instance instanceof TBase);
    assertTrue(instance instanceof TestStruct);

    TestStruct struct = (TestStruct) instance;

    // byte
    TFieldIdEnum fieldId = findFieldId(metadata, "byteField");
    assertNull(getFieldValue(struct, fieldId));
    processor.setByte(struct, fieldId, (byte) 42);
    assertEquals(42, struct.getByteField());

    // short
    fieldId = findFieldId(metadata, "i16Field");
    assertNull(getFieldValue(struct, fieldId));
    processor.setInt16(struct, fieldId, (short) 42);
    assertEquals(42, struct.getI16Field());

    // int
    fieldId = findFieldId(metadata, "i32Field");
    assertNull(getFieldValue(struct, fieldId));
    processor.setInt32(struct, fieldId, 42);
    assertEquals(42, struct.getI32Field());

    // long
    fieldId = findFieldId(metadata, "i64Field");
    assertNull(getFieldValue(struct, fieldId));
    processor.setInt64(struct, fieldId, 42L);
    assertEquals(42, struct.getI64Field());

    // binary
    fieldId = findFieldId(metadata, "binaryField");
    assertNull(getFieldValue(struct, fieldId));
    byte[] noBytes = new byte[0];
    ByteBuffer emptyBuffer = ByteBuffer.wrap(noBytes);
    processor.setBinary(struct, fieldId, emptyBuffer);
    assertArrayEquals(noBytes, struct.getBinaryField());

    // string
    fieldId = findFieldId(metadata, "stringField");
    assertNull(getFieldValue(struct, fieldId));
    String value = "Hello world!";
    ByteBuffer buffer = ByteBuffer.wrap(value.getBytes(StandardCharsets.UTF_8));
    processor.setString(struct, fieldId, buffer);
    assertEquals(value, struct.getStringField());

    // enum
    fieldId = findFieldId(metadata, "enumField");
    assertNull(getFieldValue(struct, fieldId));
    TstEnum e1 = TstEnum.E_ONE;
    processor.setEnumField(struct, fieldId, e1);
    assertEquals(TstEnum.E_ONE, struct.getEnumField());
  }

  @Test
  public void testStructContainerFields() throws TException {
    List<ThriftField> fields = ThriftField.fromNames(
        Arrays.asList(
            // List field
            "i32List",

            // Set field
            "stringSet",

            // Map field
            "stringMap",

            // Struct field
            "structField"
        ));

    ThriftMetadata.ThriftStruct metadata =
        ThriftMetadata.ThriftStruct.fromFields(TestStruct.class, fields);
    ThriftStructProcessor processor = new ThriftStructProcessor();
    Object instance = processor.createNewStruct(metadata);
    assertNotNull(instance);
    assertTrue(instance instanceof TBase);
    assertTrue(instance instanceof TestStruct);

    TestStruct struct = (TestStruct) instance;

    // list
    TFieldIdEnum fieldId = findFieldId(metadata, "i32List");
    assertNull(getFieldValue(struct, fieldId));
    Integer[] ints = new Integer[] { 1, 2, 3 };
    List<Integer> intList = Arrays.asList(ints);
    processor.setListField(struct, fieldId, intList);
    assertArrayEquals(ints, struct.getI32List().toArray());

    // set
    fieldId = findFieldId(metadata, "stringSet");
    assertNull(getFieldValue(struct, fieldId));
    String[] strings = new String[] { "Hello", "World!" };
    Set<String> stringSet = new HashSet<>(Arrays.asList(strings));
    processor.setSetField(struct, fieldId, stringSet);
    assertEquals(stringSet, struct.getStringSet());

    // map
    fieldId = findFieldId(metadata, "stringMap");
    assertNull(getFieldValue(struct, fieldId));
    Map<String, String> stringMap = new HashMap<>();
    stringMap.put("foo", "bar");
    stringMap.put("Hello", "World!");
    processor.setMapField(struct, fieldId, stringMap);
    assertEquals(stringMap, struct.getStringMap());

    // struct
    fieldId = findFieldId(metadata, "structField");
    assertNull(getFieldValue(struct, fieldId));
    SmallStruct smallStruct = new SmallStruct();
    smallStruct.setI32Field(42);
    SmallStruct smallStruct2 = new SmallStruct();
    smallStruct2.setI32Field(42);
    processor.setStructField(struct, fieldId, smallStruct);
    assertEquals(smallStruct2, struct.getStructField());
  }

  private TFieldIdEnum findFieldId(ThriftMetadata.ThriftStruct metadata, String fieldName) {
    Collection<ThriftMetadata.ThriftObject> fields = metadata.fields.values();
    for (ThriftMetadata.ThriftObject field : fields) {
      if (fieldName.equalsIgnoreCase(field.fieldId.getFieldName())) {
        return field.fieldId;
      }
    }

    fail("Field not found: " + fieldName);
    return null;
  }

  private Object getFieldValue(TBase struct, TFieldIdEnum fieldId) {
    TFieldIdEnum fieldRef = struct.fieldForId(fieldId.getThriftFieldId());
    if (struct.isSet(fieldRef)) {
      return struct.getFieldValue(fieldRef);
    } else {
      return null;
    }
  }
}
