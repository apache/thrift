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
import org.apache.thrift.partial.TstEnum;
import org.apache.thrift.partial.ExceptionAsserts;

import org.apache.thrift.TBase;
import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.junit.Test;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class TestPartialThriftDeserializer {

  private ThriftSerDe serde = new ThriftSerDe();
  private TBinaryProtocol.Factory binaryProtocolFactory = new TBinaryProtocol.Factory();
  private TCompactProtocol.Factory compactProtocolFactory = new TCompactProtocol.Factory();

  private PartialThriftTestData testData = new PartialThriftTestData();

  public TestPartialThriftDeserializer() throws TException {
  }

  @Test
  public void testArgChecks() throws TException {
    // Should not throw.
    List<String> fieldNames = Arrays.asList("i32Field");
    new TDeserializer(TestStruct.class, fieldNames, binaryProtocolFactory);

    // Verify it throws correctly.
    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'thriftClass' must not be null",
        () -> new TDeserializer(null, fieldNames, binaryProtocolFactory));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fieldNames' must not be null",
        () -> new TDeserializer(TestStruct.class, null, binaryProtocolFactory));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'processor' must not be null",
        () -> new TDeserializer(TestStruct.class, fieldNames, null, binaryProtocolFactory));
  }

  /**
   * This test does not use partial deserialization. It is used to establish correctness
   * of full serialization used in the other tests.
   */
  @Test
  public void testRoundTripFull() throws TException {
    TestStruct ts1 = testData.createTestStruct(1, 2);

    byte[] bytesBinary = serde.serializeBinary(ts1);
    byte[] bytesCompact = serde.serializeCompact(ts1);

    TestStruct ts2 =  serde.deserializeBinary(bytesBinary, TestStruct.class);
    assertEquals(ts1, ts2);

    ts2 = serde.deserializeCompact(bytesCompact, TestStruct.class);
    assertEquals(ts1, ts2);
  }

  @Test
  public void testPartialSimpleField() throws TException, IOException {
    TestStruct ts1 = testData.createTestStruct(1, 1);
    assertTrue(ts1.isSetI16Field());
    assertTrue(ts1.isSetI32Field());

    byte[] bytesBinary = serde.serializeBinary(ts1);
    byte[] bytesCompact = serde.serializeCompact(ts1);

    List<String> fieldNames = Arrays.asList("i32Field");

    TDeserializer partialBinaryDeserializer =
        new TDeserializer(TestStruct.class, fieldNames, binaryProtocolFactory);
    TDeserializer partialCompactDeserializer =
        new TDeserializer(TestStruct.class, fieldNames, compactProtocolFactory);

    PartialThriftComparer comparer =
        new PartialThriftComparer(partialBinaryDeserializer.getMetadata());

    StringBuilder sb = new StringBuilder();
    TestStruct ts2 = (TestStruct) partialBinaryDeserializer.partialDeserializeObject(bytesBinary);
    validatePartialSimpleField(ts1, ts2);
    if (!comparer.areEqual(ts1, ts2, sb)) {
      fail(sb.toString());
    }

    ts2 = (TestStruct) partialCompactDeserializer.partialDeserializeObject(bytesCompact);
    validatePartialSimpleField(ts1, ts2);
    if (!comparer.areEqual(ts1, ts2, sb)) {
      fail(sb.toString());
    }
  }

  private void validatePartialSimpleField(TestStruct ts1, TestStruct ts2) {
    assertTrue(ts2.toString(), ts2.isSetI32Field());
    assertEquals(ts1.getI32Field(), ts2.getI32Field());
    assertFalse(ts2.isSetI16Field());
  }

  @Test
  public void testPartialComplex() throws TException {
    int id = 1;
    int numItems = 10;
    TestStruct ts1 = testData.createTestStruct(id, numItems);

    byte[] bytesBinary = serde.serializeBinary(ts1);
    byte[] bytesCompact = serde.serializeCompact(ts1);

    List<String> fieldNames = Arrays.asList(
        "byteField",
        "i16Field",
        "i32Field",
        "i64Field",
        "doubleField",
        "stringField",

        "enumField",
        "binaryField",

        // List fields
        "byteList",
        "i16List",
        "i32List",
        "i64List",
        "doubleList",
        "stringList",
        "enumList",
        "listList",
        "setList",
        "mapList",
        "structList",
        "binaryList",

        // Set fields
        "byteSet",
        "i16Set",
        "i32Set",
        "i64Set",
        "doubleSet",
        "stringSet",
        "enumSet",
        "listSet",
        "setSet",
        "mapSet",
        "structSet",
        "binarySet",

        // Map fields
        "byteMap",
        "i16Map",
        "i32Map",
        "i64Map",
        "doubleMap",
        "stringMap",
        "enumMap",
        "listMap",
        "setMap",
        "mapMap",
        "structMap",
        "binaryMap",

        // Struct field
        "structField"
    );
    StringBuilder sb = new StringBuilder();
    TDeserializer partialBinaryDeserializer =
        new TDeserializer(TestStruct.class, fieldNames, binaryProtocolFactory);
    TDeserializer partialCompactDeserializer =
        new TDeserializer(TestStruct.class, fieldNames, compactProtocolFactory);
    PartialThriftComparer comparer =
        new PartialThriftComparer(partialBinaryDeserializer.getMetadata());

    TestStruct ts2 = (TestStruct) partialBinaryDeserializer.partialDeserializeObject(bytesBinary);
    validatePartialComplex(ts1, ts2, id, numItems);
    if (!comparer.areEqual(ts1, ts2, sb)) {
      fail(sb.toString());
    }

    ts2 = (TestStruct) partialCompactDeserializer.partialDeserializeObject(bytesCompact);
    validatePartialComplex(ts1, ts2, id, numItems);
    if (!comparer.areEqual(ts1, ts2, sb)) {
      fail(sb.toString());
    }
  }

  private void validatePartialComplex(TestStruct ts1, TestStruct ts2, int id, int numItems) {

    // Validate primitive fields.
    assertTrue(ts2.toString(), ts2.isSetByteField());
    assertEquals(ts1.getByteField(), ts2.getByteField());

    assertTrue(ts2.isSetI16Field());
    assertEquals(ts1.getI16Field(), ts2.getI16Field());

    assertTrue(ts2.isSetI32Field());
    assertEquals(ts1.getI32Field(), ts2.getI32Field());

    assertTrue(ts2.isSetI64Field());
    assertEquals(ts1.getI64Field(), ts2.getI64Field());

    assertTrue(ts2.isSetDoubleField());
    assertEquals(ts1.getDoubleField(), ts2.getDoubleField(), 0.0001);

    assertTrue(ts2.isSetStringField());
    assertEquals(ts1.getStringField(), ts2.getStringField());

    assertTrue(ts2.isSetEnumField());
    assertEquals(ts1.getEnumField(), ts2.getEnumField());

    assertTrue(ts2.isSetBinaryField());
    assertArrayEquals(ts1.getBinaryField(), ts2.getBinaryField());

    // Validate list fields.
    validateList(ts2.getByteList(), id, numItems);
    validateList(ts2.getI16List(), id, numItems);
    validateList(ts2.getI32List(), id, numItems);
    validateList(ts2.getI64List(), id, numItems);
    validateList(ts2.getDoubleList(), id, numItems);
    validateStringList(ts2.getStringList(), id, numItems);
    validateEnumList(ts2.getEnumList(), id, numItems);

    validateListOfList(ts2.getListList(), id, numItems);
    validateListOfSet(ts2.getSetList(), id, numItems);
    validateListOfMap(ts2.getMapList(), id, numItems);
    validateListOfStruct(ts2.getStructList(), id, numItems);
    validateListOfBinary(ts2.getBinaryList(), id, numItems);

    // Validate set fields.
    validateSet(ts2.getByteSet(), Byte.class, numItems);
    validateSet(ts2.getI16Set(), Short.class, numItems);
    validateSet(ts2.getI32Set(), Integer.class, numItems);
    validateSet(ts2.getI64Set(), Long.class, numItems);
    validateSet(ts2.getDoubleSet(), Double.class, numItems);
    validateStringSet(ts2.getStringSet(), id, numItems);
    validateEnumSet(ts2.getEnumSet(), id, numItems);

    validateSetOfList(ts2.getListSet(), id, numItems);
    validateSetOfSet(ts2.getSetSet(), id, numItems);
    validateSetOfMap(ts2.getMapSet(), id, numItems);
    validateSetOfStruct(ts2.getStructSet(), id, numItems);
    validateSetOfBinary(ts2.getBinarySet(), id, numItems);

    // Validate map fields.
    validateMap(ts2.getByteMap(), Byte.class, numItems);
    validateMap(ts2.getI16Map(), Short.class, numItems);
    validateMap(ts2.getI32Map(), Integer.class, numItems);
    validateMap(ts2.getI64Map(), Long.class, numItems);
    validateMap(ts2.getDoubleMap(), Double.class, numItems);
    validateStringMap(ts2.getStringMap(), id, numItems);
    validateEnumMap(ts2.getEnumMap(), id, numItems);

    validateMapOfList(ts2.getListMap(), id, numItems);
    validateMapOfSet(ts2.getSetMap(), id, numItems);
    validateMapOfMap(ts2.getMapMap(), id, numItems);
    validateMapOfStruct(ts2.getStructMap(), id, numItems);
    validateMapOfBinary(ts2.getBinaryMap(), id, numItems);

    // Validate struct field.
    assertEquals(testData.createSmallStruct(id), ts2.getStructField());
  }

  private void validateNotNullAndNotEmpty(Collection<?> collection, int numItems) {
    assertNotNull(collection);
    assertEquals(numItems, collection.size());
  }

  // ----------------------------------------------------------------------
  // List validation helpers.

  private <V extends Number> void validateList(List<V> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);

    for (int i = 0; i < numItems; i++) {
      assertEquals(i, list.get(i).longValue());
    }
  }

  private void validateStringList(List<String> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);
    for (int i = 0; i < numItems; i++) {
      assertEquals(Integer.valueOf(i), Integer.valueOf(list.get(i)));
    }
  }

  private void validateEnumList(List<TstEnum> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);
    for (int i = 0; i < numItems; i++) {
      assertEquals(TstEnum.E_ONE, list.get(i));
    }
  }

  private <V extends Number> void validateListOfList(List<List<V>> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);

    for (int i = 0; i < numItems; i++) {
      validateList(list.get(i), id, numItems);
    }
  }

  private <V extends Number> void validateListOfSet(List<Set<V>> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);

    for (int i = 0; i < numItems; i++) {
      Set<V> set = list.get(i);
      for (int j = 0; j < numItems; j++) {
        assertTrue(set.contains(j));
      }
    }
  }

  private <V extends Number> void validateListOfMap(
      List<Map<String, V>> list, int id, int numItems) {

    validateNotNullAndNotEmpty(list, numItems);

    for (int i = 0; i < numItems; i++) {
      Map<String, V> map = list.get(i);
      for (int j = 0; j < numItems; j++) {
        String key = Integer.toString(j);
        assertTrue(map.containsKey(key));
        assertEquals(j, map.get(key));
      }
    }
  }

  private void validateListOfStruct(List<SmallStruct> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);

    for (int i = 0; i < numItems; i++) {
      SmallStruct ss = testData.createSmallStruct(i);
      for (int j = 0; j < numItems; j++) {
        assertEquals(ss, list.get(i));
      }
    }
  }

  private void validateListOfBinary(List<ByteBuffer> list, int id, int numItems) {
    validateNotNullAndNotEmpty(list, numItems);

    for (int i = 0; i < numItems; i++) {
      ByteBuffer bb = ByteBuffer.wrap(testData.BYTES);
      assertTrue(bb.compareTo(list.get(i)) == 0);
    }
  }

  // ----------------------------------------------------------------------
  // Set validation helpers.

  private <V extends Number> void validateSet(Set<V> set, Class<V> clasz, int numItems) {
    validateNotNullAndNotEmpty(set, numItems);

    for (int i = 0; i < numItems; i++) {
      if (clasz == Byte.class) {
        assertTrue(set.contains((byte)i));
      } else if (clasz == Short.class) {
        assertTrue(set.contains((short)i));
      } else if (clasz == Integer.class) {
        assertTrue(set.contains(i));
      } else if (clasz == Long.class) {
        assertTrue(set.contains((long)i));
      } else if (clasz == Double.class) {
        assertTrue(set.contains((double)i));
      }
    }
  }

  private void validateStringSet(Set<String> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, numItems);

    for (int i = 0; i < numItems; i++) {
      assertTrue(set.contains(Integer.toString(i)));
    }
  }

  private void validateEnumSet(Set<TstEnum> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, 1);

    assertTrue(set.contains(TstEnum.E_ONE));
  }

  private void validateSetOfList(Set<List<Integer>> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, 1);

    List<Integer> list = new ArrayList<>(numItems);
    for (int i = 0; i < numItems; i++) {
      list.add(i);
    }

    assertTrue(set.contains(list));
  }

  private void validateSetOfSet(Set<Set<Integer>> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, 1);

    Set<Integer> setElt = new HashSet<>();
    for (int i = 0; i < numItems; i++) {
      setElt.add(i);
    }

    assertTrue(set.contains(setElt));
  }

  private void validateSetOfMap(Set<Map<String, Integer>> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, 1);

    Map<String, Integer> map = new HashMap<>();
    for (int i = 0; i < numItems; i++) {
      map.put(Integer.toString(i), i);
    }

    assertTrue(set.contains(map));
  }

  private void validateSetOfStruct(Set<SmallStruct> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, numItems);

    for (int i = 0; i < numItems; i++) {
      SmallStruct ss = testData.createSmallStruct(i);
      assertTrue(set.contains(ss));
    }
  }

  private void validateSetOfBinary(Set<ByteBuffer> set, int id, int numItems) {
    validateNotNullAndNotEmpty(set, 1);

    for (ByteBuffer b : set) {
      ByteBuffer bb = ByteBuffer.wrap(testData.BYTES);
      assertEquals(0, bb.compareTo(b));
    }
  }

  // ----------------------------------------------------------------------
  // Map validation helpers.

  void validateNotNullAndNotEmpty(Map<?, ?> map, int numItems) {
    assertNotNull(map);
    assertEquals(numItems, map.size());
  }

  private <V extends Number> void validateMap(Map<V, V> map, Class<V> clasz, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    for (int i = 0; i < numItems; i++) {
      if (clasz == Byte.class) {
        assertTrue(map.containsKey((byte)i));
        assertEquals((byte) i, map.get((byte) i));
      } else if (clasz == Short.class) {
        assertTrue(map.containsKey((short)i));
        assertEquals((short) i, map.get((short) i));
      } else if (clasz == Integer.class) {
        assertTrue(map.containsKey(i));
        assertEquals(i, map.get(i));
      } else if (clasz == Long.class) {
        assertTrue(map.containsKey((long)i));
        assertEquals((long) i, map.get((long) i));
      } else if (clasz == Double.class) {
        assertTrue(map.containsKey((double)i));
        assertEquals((double) i, map.get((double) i));
      }
    }
  }

  private void validateStringMap(Map<String, String> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    for (int i = 0; i < numItems; i++) {
      String key = Integer.toString(i);
      assertTrue(map.containsKey(key));
      assertEquals(key, map.get(key));
    }
  }

  private void validateEnumMap(Map<TstEnum, TstEnum> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, 1);

    assertTrue(map.containsKey(TstEnum.E_ONE));
    assertEquals(TstEnum.E_ONE, map.get(TstEnum.E_ONE));
  }

  private void validateMapOfList(Map<Integer, List<Integer>> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    List<Integer> list = new ArrayList<>(numItems);
    for (int i = 0; i < numItems; i++) {
      list.add(i);
    }

    for (int i = 0; i < numItems; i++) {
      assertTrue(map.containsKey(i));
      assertEquals(list, map.get(i));
    }
  }

  private void validateMapOfSet(Map<Integer, Set<Integer>> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    Set<Integer> setElt = new HashSet<>();
    for (int i = 0; i < numItems; i++) {
      setElt.add(i);
    }

    for (int i = 0; i < numItems; i++) {
      assertTrue(map.containsKey(i));
      assertEquals(setElt, map.get(i));
    }
  }

  private void validateMapOfMap(Map<Integer, Map<Integer, Integer>> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    Map<Integer, Integer> mapElt = new HashMap<>();
    for (int i = 0; i < numItems; i++) {
      mapElt.put(i, i);
    }

    for (int i = 0; i < numItems; i++) {
      assertTrue(map.containsKey(i));
      assertEquals(mapElt, map.get(i));
    }
  }

  private void validateMapOfStruct(Map<SmallStruct, SmallStruct> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    for (int i = 0; i < numItems; i++) {
      SmallStruct ss = testData.createSmallStruct(i);
      assertTrue(map.containsKey(ss));
      assertEquals(ss, map.get(ss));
    }
  }

  private void validateMapOfBinary(Map<Integer, ByteBuffer> map, int id, int numItems) {
    validateNotNullAndNotEmpty(map, numItems);

    for (int i = 0; i < numItems; i++) {
      ByteBuffer bb = ByteBuffer.wrap(testData.BYTES);
      assertTrue(map.containsKey(i));
      assertEquals(0, bb.compareTo(map.get(i)));
    }
  }
}
