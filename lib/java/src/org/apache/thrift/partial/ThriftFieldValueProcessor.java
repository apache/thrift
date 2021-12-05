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

import org.apache.thrift.TEnum;
import org.apache.thrift.TFieldIdEnum;

import java.nio.ByteBuffer;

/**
 * Provides an abstraction to process deserialized field values and place them
 * into the collection that holds them. This abstraction allows different types
 * of collections to be output from partial deserialization.
 *
 * In case of the usual Thrift deserialization, the collection that holds field
 * values is simply an instance of TBase.
 */
public interface ThriftFieldValueProcessor<V> {

  // Struct related methods;
  Object createNewStruct(ThriftMetadata.ThriftStruct metadata);

  V prepareStruct(Object instance);

  void setBool(V valueCollection, TFieldIdEnum fieldId,  boolean value);

  void setByte(V valueCollection, TFieldIdEnum fieldId,  byte value);

  void setInt16(V valueCollection, TFieldIdEnum fieldId,  short value);

  void setInt32(V valueCollection, TFieldIdEnum fieldId,  int value);

  void setInt64(V valueCollection, TFieldIdEnum fieldId,  long value);

  void setDouble(V valueCollection, TFieldIdEnum fieldId, double value);

  void setBinary(V valueCollection, TFieldIdEnum fieldId, ByteBuffer value);

  void setString(V valueCollection, TFieldIdEnum fieldId, ByteBuffer buffer);

  void setEnumField(V valueCollection, TFieldIdEnum fieldId, Object value);

  void setListField(V valueCollection, TFieldIdEnum fieldId, Object value);

  void setMapField(V valueCollection, TFieldIdEnum fieldId, Object value);

  void setSetField(V valueCollection, TFieldIdEnum fieldId, Object value);

  void setStructField(V valueCollection, TFieldIdEnum fieldId, Object value);

  Object prepareEnum(Class<? extends TEnum> enumClass, int ordinal);

  Object prepareString(ByteBuffer buffer);

  Object prepareBinary(ByteBuffer buffer);

  // List field related methods.
  Object createNewList(int expectedSize);

  void setListElement(Object instance, int index, Object value);

  Object prepareList(Object instance);

  // Map field related methods.
  Object createNewMap(int expectedSize);

  void setMapElement(Object instance, int index, Object key, Object value);

  Object prepareMap(Object instance);

  // Set field related methods.
  Object createNewSet(int expectedSize);

  void setSetElement(Object instance, int index, Object value);

  Object prepareSet(Object instance);
}
