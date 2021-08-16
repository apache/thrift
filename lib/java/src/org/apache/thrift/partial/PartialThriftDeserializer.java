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

import org.apache.thrift.partial.Validate;

import org.apache.thrift.TBase;
import org.apache.thrift.TEnum;
import org.apache.thrift.TException;
import org.apache.thrift.TFieldIdEnum;
import org.apache.thrift.meta_data.EnumMetaData;
import org.apache.thrift.meta_data.StructMetaData;
import org.apache.thrift.protocol.TList;
import org.apache.thrift.protocol.TMap;
import org.apache.thrift.protocol.TSet;
import org.apache.thrift.protocol.TType;

/**
 * Supports partial deserialization of serialized Thrift data.
 *
 * The end result of deserialization can be a Thrift object (? extends TBase)
 * or it could be a different construct. It is achieved by using a
 * ThriftFieldValueProcessor instance which processes each field value
 * as it gets deserialized.
 */
public class PartialThriftDeserializer<T extends TBase> {

  // Metadata that describes fields to deserialize.
  private final ThriftMetadata.ThriftStruct metadata;

  // Processor that handles deserialized field values.
  private final ThriftFieldValueProcessor processor;

  // Partial thrift protocol to use for deserialization.
  private final PartialThriftProtocol protocol;

  /**
   * Constructs an instance of PartialThriftDeserializer.
   *
   * @param metadata the Metadata that describes fields to deserialize.
   * @param processor the Processor that handles deserialized field values.
   * @param protocol the Partial thrift protocol to use for deserialization.
   */
  public PartialThriftDeserializer(
      ThriftMetadata.ThriftStruct metadata,
      ThriftFieldValueProcessor processor,
      PartialThriftProtocol protocol) {

    Validate.checkNotNull(metadata, "metadata");
    Validate.checkNotNull(processor, "processor");
    Validate.checkNotNull(protocol, "protocol");

    this.metadata = metadata;
    this.processor = processor;
    this.protocol = protocol;
  }

  /**
   * Gets the Thrift metadata used by this instance.
   */
  public ThriftMetadata.ThriftStruct getMetadata() {
    return this.metadata;
  }

  /**
   * Deserializes the given serialized blob.
   *
   * @param bytes the serialized blob.
   * @return deserialized instance.
   * @throws TException if an error is encountered during deserialization.
   */
  public Object deserialize(byte[] bytes) throws TException {
    return this.deserialize(bytes, 0, bytes.length);
  }

  /**
   * Deserializes the given serialized blob.
   *
   * @param bytes the serialized blob.
   * @param offset the blob is read starting at this offset.
   * @param length the size of blob read (in number of bytes).
   * @return deserialized instance.
   * @throws TException if an error is encountered during deserialization.
   */
  public Object deserialize(byte[] bytes, int offset, int length) throws TException {
    this.protocol.reset(bytes, offset, length);
    return this.deserializeStruct(this.protocol, this.metadata);
  }

  private Object deserialize(
      PartialThriftProtocol tprot,
      ThriftMetadata.ThriftObject data) throws TException {

    Object value;
    byte fieldType = data.data.valueMetaData.type;
    switch (fieldType) {
      case TType.STRUCT:
        return this.deserializeStruct(tprot, (ThriftMetadata.ThriftStruct) data);

      case TType.LIST:
        return this.deserializeList(tprot, (ThriftMetadata.ThriftList) data);

      case TType.MAP:
        return this.deserializeMap(tprot, (ThriftMetadata.ThriftMap) data);

      case TType.SET:
        return this.deserializeSet(tprot, (ThriftMetadata.ThriftSet) data);

      case TType.ENUM:
        return this.deserializeEnum(tprot, (ThriftMetadata.ThriftEnum) data);

      case TType.BOOL:
        return tprot.readBool();

      case TType.BYTE:
        return tprot.readByte();

      case TType.I16:
        return tprot.readI16();

      case TType.I32:
        return tprot.readI32();

      case TType.I64:
        return tprot.readI64();

      case TType.DOUBLE:
        return tprot.readDouble();

      case TType.STRING:
        if (((ThriftMetadata.ThriftPrimitive) data).isBinary()) {
          return this.processor.prepareBinary(tprot.readBinary());
        } else {
          return this.processor.prepareString(tprot.readBinary());
        }

      default:
        throw unsupportedFieldTypeException(fieldType);
    }
  }

  private Object deserializeStruct(PartialThriftProtocol tprot, ThriftMetadata.ThriftStruct data)
      throws TException {

    if (data.fields.size() == 0) {
      return this.fullDeserialize(tprot, data);
    }

    Object instance = this.processor.createNewStruct(data);
    tprot.readStructBegin();
    while (true) {
      int tfieldData = tprot.readFieldBeginData();
      byte tfieldType = TFieldData.getType(tfieldData);
      if (tfieldType == TType.STOP) {
        break;
      }

      Integer id = (int) TFieldData.getId(tfieldData);
      ThriftMetadata.ThriftObject field = (ThriftMetadata.ThriftObject) data.fields.get(id);

      if (field != null) {
        this.deserializeStructField(tprot, instance, field.fieldId, field);
      } else {
        tprot.skip(tfieldType);
      }
      tprot.readFieldEnd();
    }
    tprot.readStructEnd();

    return this.processor.prepareStruct(instance);
  }

  private void deserializeStructField(
      PartialThriftProtocol tprot,
      Object instance,
      TFieldIdEnum fieldId,
      ThriftMetadata.ThriftObject data) throws TException {

    byte fieldType = data.data.valueMetaData.type;
    Object value;

    switch (fieldType) {
      case TType.BOOL:
        this.processor.setBool(instance, fieldId, tprot.readBool());
        break;

      case TType.BYTE:
        this.processor.setByte(instance, fieldId, tprot.readByte());
        break;

      case TType.I16:
        this.processor.setInt16(instance, fieldId, tprot.readI16());
        break;

      case TType.I32:
        this.processor.setInt32(instance, fieldId, tprot.readI32());
        break;

      case TType.I64:
        this.processor.setInt64(instance, fieldId, tprot.readI64());
        break;

      case TType.DOUBLE:
        this.processor.setDouble(instance, fieldId, tprot.readDouble());
        break;

      case TType.STRING:
        if (((ThriftMetadata.ThriftPrimitive) data).isBinary()) {
          this.processor.setBinary(instance, fieldId, tprot.readBinary());
        } else {
          this.processor.setString(instance, fieldId, tprot.readBinary());
        }
        break;

      case TType.STRUCT:
        value = this.deserializeStruct(tprot, (ThriftMetadata.ThriftStruct) data);
        this.processor.setStructField(instance, fieldId, value);
        break;

      case TType.LIST:
        value = this.deserializeList(tprot, (ThriftMetadata.ThriftList) data);
        this.processor.setListField(instance, fieldId, value);
        break;

      case TType.MAP:
        value = this.deserializeMap(tprot, (ThriftMetadata.ThriftMap) data);
        this.processor.setMapField(instance, fieldId, value);
        break;

      case TType.SET:
        value = this.deserializeSet(tprot, (ThriftMetadata.ThriftSet) data);
        this.processor.setSetField(instance, fieldId, value);
        break;

      case TType.ENUM:
        value = this.deserializeEnum(tprot, (ThriftMetadata.ThriftEnum) data);
        this.processor.setEnumField(instance, fieldId, value);
        break;

      default:
        throw new RuntimeException("Unsupported field type: " + fieldId.toString());
    }
  }

  private Object deserializeList(PartialThriftProtocol tprot, ThriftMetadata.ThriftList data)
        throws TException {

    TList tlist = tprot.readListBegin();
    Object instance = this.processor.createNewList(tlist.size);
    for (int i = 0; i < tlist.size; i++) {
      Object value = this.deserialize(tprot, data.elementData);
      this.processor.setListElement(instance, i, value);
    }
    tprot.readListEnd();
    return this.processor.prepareList(instance);
  }

  private Object deserializeMap(PartialThriftProtocol tprot, ThriftMetadata.ThriftMap data)
      throws TException {
    TMap tmap = tprot.readMapBegin();
    Object instance = this.processor.createNewMap(tmap.size);
    for (int i = 0; i < tmap.size; i++) {
      Object key = this.deserialize(tprot, data.keyData);
      Object val = this.deserialize(tprot, data.valueData);
      this.processor.setMapElement(instance, i, key, val);
    }
    tprot.readMapEnd();
    return this.processor.prepareMap(instance);
  }

  private Object deserializeSet(PartialThriftProtocol tprot, ThriftMetadata.ThriftSet data)
      throws TException {
    TSet tset = tprot.readSetBegin();
    Object instance = this.processor.createNewSet(tset.size);
    for (int i = 0; i < tset.size; i++) {
      Object eltValue = this.deserialize(tprot, data.elementData);
      this.processor.setSetElement(instance, i, eltValue);
    }
    tprot.readSetEnd();
    return this.processor.prepareSet(instance);
  }

  private Object deserializeEnum(PartialThriftProtocol tprot, ThriftMetadata.ThriftEnum data)
      throws TException {
    int ordinal = tprot.readI32();
    Class<? extends TEnum> enumClass = ((EnumMetaData) data.data.valueMetaData).enumClass;
    return this.processor.prepareEnum(enumClass, ordinal);
  }

  private TBase fullDeserialize(PartialThriftProtocol tprot, ThriftMetadata.ThriftStruct data)
      throws TException {
    Validate.checkState(
        data.fields.size() == 0, "Cannot fully deserialize when some fields specified");
    TBase instance = this.createNewStruct(data);
    instance.read(tprot);
    return instance;
  }

  private <T extends TBase> T createNewStruct(ThriftMetadata.ThriftStruct data) {
    T instance = null;

    try {
      instance = (T) this.getStructClass(data).newInstance();
    } catch (InstantiationException e) {
      throw new RuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new RuntimeException(e);
    }

    return instance;
  }

  private Class<T> getStructClass(ThriftMetadata.ThriftStruct data) {
    return (Class<T>) ((StructMetaData) data.data.valueMetaData).structClass;
  }

  static UnsupportedOperationException unsupportedFieldTypeException(byte fieldType) {
    return new UnsupportedOperationException("field type not supported: " + fieldType);
  }
}
