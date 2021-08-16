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

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Helpers for creating test data related to partial deserialization.
 */
public class PartialThriftTestData {

  public final byte[] BYTES = new byte[] { 1, 2, 3 };

  public SmallStruct createSmallStruct(int id) {
    return new SmallStruct()
        .setByteField((byte) id)
        .setI16Field((short) id)
        .setI32Field(id)
        .setI64Field(id)
        .setDoubleField(id)
        .setStringField(Integer.toString(id))
        .setEnumField(TstEnum.E_ONE);
  }

  public TestStruct createTestStruct(int id, int numItems) {

    TestStruct ts = new TestStruct()
        .setByteField((byte) id)
        .setI16Field((short) id)
        .setI32Field(id)
        .setI64Field(id)
        .setDoubleField(id)
        .setStringField(Integer.toString(id))
        .setEnumField(TstEnum.E_ONE)
        .setBinaryField(BYTES)
        .setStructField(createSmallStruct(id));

    initListFields(ts, id, numItems);
    initSetFields(ts, id, numItems);
    initMapFields(ts, id, numItems);

    return ts;
  }

  public void initListFields(TestStruct ts, int id, int numItems) {
    List<Byte> byteList = new ArrayList<>(numItems);
    List<Short> i16List = new ArrayList<>(numItems);
    List<Integer> i32List = new ArrayList<>(numItems);
    List<Long> i64List = new ArrayList<>(numItems);
    List<Double> doubleList = new ArrayList<>(numItems);
    List<String> stringList = new ArrayList<>(numItems);
    List<TstEnum> enumList = new ArrayList<>(numItems);

    List<List<Integer>> listList = new ArrayList<>(numItems);
    List<Set<Integer>> setList = new ArrayList<>(numItems);
    List<Map<String, Integer>> mapList = new ArrayList<>(numItems);
    List<SmallStruct> structList = new ArrayList<>(numItems);
    List<ByteBuffer> binaryList = new ArrayList<>(numItems);

    for (int i = 0; i < numItems; i++) {
      byteList.add((byte) i);
      i16List.add((short) i);
      i32List.add(i);
      i64List.add((long)i);
      doubleList.add((double) i);
      stringList.add(Integer.toString(i));
      enumList.add(TstEnum.E_ONE);
      structList.add(createSmallStruct(i));
      binaryList.add(ByteBuffer.wrap(BYTES));

      List<Integer> listItem = new ArrayList<>(numItems);
      listList.add(listItem);

      Set<Integer> setItem = new HashSet<>();
      setList.add(setItem);

      Map<String, Integer> mapItem = new HashMap<>();
      mapList.add(mapItem);

      for (int j = 0; j < numItems; j++) {
        listItem.add(j);
        setItem.add(j);
        mapItem.put(Integer.toString(j), j);
      }
    }

    ts.setByteList(byteList)
        .setI16List(i16List)
        .setI32List(i32List)
        .setI64List(i64List)
        .setDoubleList(doubleList)
        .setStringList(stringList)
        .setEnumList(enumList)
        .setListList(listList)
        .setSetList(setList)
        .setMapList(mapList)
        .setStructList(structList)
        .setBinaryList(binaryList);
  }

  public void initSetFields(TestStruct ts, int id, int numItems) {
    Set<Byte> byteSet = new HashSet<>();
    Set<Short> i16Set = new HashSet<>();
    Set<Integer> i32Set = new HashSet<>();
    Set<Long> i64Set = new HashSet<>();
    Set<Double> doubleSet = new HashSet<>();
    Set<String> stringSet = new HashSet<>();
    Set<TstEnum> enumSet = new HashSet<>();

    Set<List<Integer>> listSet = new HashSet<>();
    Set<Set<Integer>> setSet = new HashSet<>();
    Set<Map<String, Integer>> mapSet = new HashSet<>();
    Set<SmallStruct> structSet = new HashSet<>();
    Set<ByteBuffer> binarySet = new HashSet<>();

    for (int i = 0; i < numItems; i++) {
      byteSet.add((byte) i);
      i16Set.add((short) i);
      i32Set.add(i);
      i64Set.add((long)i);
      doubleSet.add((double) i);
      stringSet.add(Integer.toString(i));
      enumSet.add(TstEnum.E_ONE);
      structSet.add(createSmallStruct(i));
      binarySet.add(ByteBuffer.wrap(BYTES));

      List<Integer> listItem = new ArrayList<>(numItems);
      Set<Integer> setItem = new HashSet<>();
      Map<String, Integer> mapItem = new HashMap<>();

      for (int j = 0; j < numItems; j++) {
        setItem.add(j);
        listItem.add(j);
        mapItem.put(Integer.toString(j), j);
      }

      listSet.add(listItem);
      setSet.add(setItem);
      mapSet.add(mapItem);
    }

    ts.setByteSet(byteSet)
        .setI16Set(i16Set)
        .setI32Set(i32Set)
        .setI64Set(i64Set)
        .setDoubleSet(doubleSet)
        .setStringSet(stringSet)
        .setEnumSet(enumSet)
        .setListSet(listSet)
        .setSetSet(setSet)
        .setMapSet(mapSet)
        .setStructSet(structSet)
        .setBinarySet(binarySet);
  }

  public void initMapFields(TestStruct ts, int id, int numItems) {
    Map<Byte, Byte> byteMap = new HashMap<>();
    Map<Short, Short> i16Map = new HashMap<>();
    Map<Integer, Integer> i32Map = new HashMap<>();
    Map<Long, Long> i64Map = new HashMap<>();
    Map<Double, Double> doubleMap = new HashMap<>();
    Map<String, String> stringMap = new HashMap<>();
    Map<TstEnum, TstEnum> enumMap = new HashMap<>();

    Map<Integer, List<Integer>> listMap = new HashMap<>();
    Map<Integer, Set<Integer>> setMap = new HashMap<>();
    Map<Integer, Map<Integer, Integer>> mapMap = new HashMap<>();
    Map<SmallStruct, SmallStruct> structMap = new HashMap<>();
    Map<Integer, ByteBuffer> binaryMap = new HashMap<>();

    for (int i = 0; i < numItems; i++) {
      byteMap.put((byte) i, (byte) i);
      i16Map.put((short) i, (short) i);
      i32Map.put(i, i);
      i64Map.put((long) i, (long) i);
      doubleMap.put((double) i, (double) i);
      stringMap.put(Integer.toString(i), Integer.toString(i));
      enumMap.put(TstEnum.E_ONE, TstEnum.E_ONE);
      structMap.put(createSmallStruct(i), createSmallStruct(i));
      binaryMap.put(i, ByteBuffer.wrap(BYTES));

      List<Integer> listItem = new ArrayList<>(numItems);
      listMap.put(i, listItem);

      Set<Integer> setItem = new HashSet<>();
      setMap.put(i, setItem);

      Map<Integer, Integer> mapItem = new HashMap<>();
      mapMap.put(i, mapItem);

      for (int j = 0; j < numItems; j++) {
        listItem.add(j);
        setItem.add(j);
        mapItem.put(j, j);
      }
    }

    ts.setByteMap(byteMap)
        .setI16Map(i16Map)
        .setI32Map(i32Map)
        .setI64Map(i64Map)
        .setDoubleMap(doubleMap)
        .setStringMap(stringMap)
        .setEnumMap(enumMap)
        .setListMap(listMap)
        .setSetMap(setMap)
        .setMapMap(mapMap)
        .setStructMap(structMap)
        .setBinaryMap(binaryMap);
  }

  public List<String> allFieldsOfTestStruct() {
      return new ArrayList<>(
          Arrays.asList(
              "byteField",
              "i16Field",
              "i32Field",
              "i64Field",
              "doubleField",
              "stringField",
              "structField.byteField",
              "structField.i16Field",
              "structField.i32Field",
              "structField.i64Field",
              "structField.doubleField",
              "structField.stringField",
              "structField.enumField",
              "enumField",
              "binaryField",
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
              "structList.byteField",
              "structList.i16Field",
              "structList.i32Field",
              "structList.i64Field",
              "structList.doubleField",
              "structList.stringField",
              "structList.enumField",
              "binaryList",
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
              "structSet.byteField",
              "structSet.i16Field",
              "structSet.i32Field",
              "structSet.i64Field",
              "structSet.doubleField",
              "structSet.stringField",
              "structSet.enumField",
              "binarySet",
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
              "structMap.byteField",
              "structMap.i16Field",
              "structMap.i32Field",
              "structMap.i64Field",
              "structMap.doubleField",
              "structMap.stringField",
              "structMap.enumField",
              "binaryMap"
          )
      );
  }
}
