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

import org.apache.thrift.TBase;
import org.apache.thrift.TEnum;
import org.apache.thrift.TFieldIdEnum;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Provides a way to create and initialize an instance of TBase during partial deserialization.
 *
 * This class is supposed to be used as a helper class for {@code PartialThriftDeserializer}.
 */
public class ThriftStructProcessor implements ThriftFieldValueProcessor<TBase> {

  private static final EnumCache enums = new EnumCache();

  @Override
  public Object createNewStruct(ThriftMetadata.ThriftStruct metadata) {
    return metadata.createNewStruct();
  }

  @Override
  public TBase prepareStruct(Object instance) {
    return (TBase) instance;
  }

  @Override
  public Object createNewList(int expectedSize) {
    return new Object[expectedSize];
  }

  @Override
  public void setListElement(Object instance, int index, Object value) {
    ((Object[]) instance)[index] = value;
  }

  @Override
  public Object prepareList(Object instance) {
    return Arrays.asList((Object[]) instance);
  }

  @Override
  public Object createNewMap(int expectedSize) {
    return new HashMap<Object, Object>(expectedSize);
  }

  @Override
  public void setMapElement(Object instance, int index, Object key, Object value) {
    ((HashMap<Object, Object>) instance).put(key, value);
  }

  @Override
  public Object prepareMap(Object instance) {
    return instance;
  }

  @Override
  public Object createNewSet(int expectedSize) {
    return new HashSet<Object>(expectedSize);
  }

  @Override
  public void setSetElement(Object instance, int index, Object value) {
    ((HashSet<Object>) instance).add(value);
  }

  @Override
  public Object prepareSet(Object instance) {
    return instance;
  }

  @Override
  public Object prepareEnum(Class<? extends TEnum> enumClass, int ordinal) {
    return enums.get(enumClass, ordinal);
  }

  @Override
  public Object prepareString(ByteBuffer buffer) {
    return byteBufferToString(buffer);
  }

  @Override
  public Object prepareBinary(ByteBuffer buffer) {
    return buffer;
  }

  @Override
  public void setBool(TBase valueCollection, TFieldIdEnum fieldId,  boolean value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setByte(TBase valueCollection, TFieldIdEnum fieldId,  byte value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setInt16(TBase valueCollection, TFieldIdEnum fieldId,  short value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setInt32(TBase valueCollection, TFieldIdEnum fieldId,  int value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setInt64(TBase valueCollection, TFieldIdEnum fieldId,  long value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setDouble(TBase valueCollection, TFieldIdEnum fieldId, double value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setBinary(TBase valueCollection, TFieldIdEnum fieldId, ByteBuffer value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setString(TBase valueCollection, TFieldIdEnum fieldId, ByteBuffer buffer) {
    String value = byteBufferToString(buffer);
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setEnumField(TBase valueCollection, TFieldIdEnum fieldId, Object value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setListField(TBase valueCollection, TFieldIdEnum fieldId, Object value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setMapField(TBase valueCollection, TFieldIdEnum fieldId, Object value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setSetField(TBase valueCollection, TFieldIdEnum fieldId, Object value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  @Override
  public void setStructField(TBase valueCollection, TFieldIdEnum fieldId, Object value) {
    valueCollection.setFieldValue(fieldId, value);
  }

  private static String byteBufferToString(ByteBuffer buffer) {
    byte[] bytes = buffer.array();
    int pos = buffer.position();
    return new String(bytes, pos, buffer.limit() - pos, StandardCharsets.UTF_8);
  }
}
