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

import org.apache.thrift.meta_data.EnumMetaData;
import org.apache.thrift.meta_data.StructMetaData;
import org.apache.thrift.partial.TFieldData;
import org.apache.thrift.partial.ThriftFieldValueProcessor;
import org.apache.thrift.partial.ThriftMetadata;
import org.apache.thrift.partial.ThriftStructProcessor;
import org.apache.thrift.partial.Validate;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TList;
import org.apache.thrift.protocol.TMap;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.protocol.TProtocolUtil;
import org.apache.thrift.protocol.TSet;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.transport.TMemoryInputTransport;
import org.apache.thrift.transport.TTransportException;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Collection;

/**
 * Generic utility for easily deserializing objects from a byte array or Java
 * String.
 *
 */
public class TDeserializer {
  private final TProtocol protocol_;
  private final TMemoryInputTransport trans_;

  // Metadata that describes fields to deserialize during partial deserialization.
  private ThriftMetadata.ThriftStruct metadata_ = null;

  // Processor that handles deserialized field values during partial deserialization.
  private ThriftFieldValueProcessor processor_ = null;

  /**
   * Create a new TDeserializer that uses the TBinaryProtocol by default.
   *
   * @throws TTransportException if there an error initializing the underlying transport.
   */
  public TDeserializer() throws TTransportException {
    this(new TBinaryProtocol.Factory());
  }

  /**
   * Create a new TDeserializer. It will use the TProtocol specified by the
   * factory that is passed in.
   *
   * @param protocolFactory Factory to create a protocol
   * @throws TTransportException if there an error initializing the underlying transport.
   */
  public TDeserializer(TProtocolFactory protocolFactory) throws TTransportException {
    trans_ = new TMemoryInputTransport(new TConfiguration());
    protocol_ = protocolFactory.getProtocol(trans_);
  }

  /**
   * Construct a new TDeserializer that supports partial deserialization
   * that outputs instances of type controlled by the given {@code processor}.
   *
   * @param thriftClass a TBase derived class.
   * @param fieldNames list of fields to deserialize.
   * @param processor the Processor that handles deserialized field values.
   * @param protocolFactory the Factory to create a protocol.
   */
  public TDeserializer(
      Class<? extends TBase> thriftClass,
      Collection<String> fieldNames,
      ThriftFieldValueProcessor processor,
      TProtocolFactory protocolFactory) throws TTransportException {
    this(protocolFactory);

    Validate.checkNotNull(thriftClass, "thriftClass");
    Validate.checkNotNull(fieldNames, "fieldNames");
    Validate.checkNotNull(processor, "processor");

    metadata_ = ThriftMetadata.ThriftStruct.fromFieldNames(thriftClass, fieldNames);
    processor_ = processor;
  }

  /**
   * Construct a new TDeserializer that supports partial deserialization
   * that outputs {@code TBase} instances.
   *
   * @param thriftClass a TBase derived class.
   * @param fieldNames list of fields to deserialize.
   * @param protocolFactory the Factory to create a protocol.
   */
  public TDeserializer(
      Class<? extends TBase> thriftClass,
      Collection<String> fieldNames,
      TProtocolFactory protocolFactory) throws TTransportException {
    this(thriftClass, fieldNames, new ThriftStructProcessor(), protocolFactory);
  }

  /**
   * Gets the metadata used for partial deserialization.
   *
   * @return the metadata used for partial deserialization.
   */
  public ThriftMetadata.ThriftStruct getMetadata() {
    return metadata_;
  }

  /**
   * Deserialize the Thrift object from a byte array.
   *
   * @param base The object to read into
   * @param bytes The array to read from
   * @throws TException if an error is encountered during deserialization.
   */
  public void deserialize(TBase base, byte[] bytes) throws TException {
      deserialize(base, bytes, 0, bytes.length);
  }

  /**
   * Deserialize the Thrift object from a byte array.
   *
   * @param base The object to read into
   * @param bytes The array to read from
   * @param offset The offset into {@code bytes}
   * @param length The length to read from {@code bytes}
   * @throws TException if an error is encountered during deserialization.
   */
  public void deserialize(TBase base, byte[] bytes, int offset, int length) throws TException {
    if (this.isPartialDeserializationMode()) {
      this.partialDeserializeThriftObject(base, bytes, offset, length);
    } else {
      try {
        trans_.reset(bytes, offset, length);
        base.read(protocol_);
      } finally {
        trans_.clear();
        protocol_.reset();
      }
    }
  }

  /**
   * Deserialize the Thrift object from a Java string, using a specified
   * character set for decoding.
   *
   * @param base The object to read into
   * @param data The string to read from
   * @param charset Valid JVM charset
   * @throws TException if an error is encountered during deserialization.
   */
  public void deserialize(TBase base, String data, String charset) throws TException {
    try {
      deserialize(base, data.getBytes(charset));
    } catch (UnsupportedEncodingException uex) {
      throw new TException("JVM DOES NOT SUPPORT ENCODING: " + charset);
    } finally {
      protocol_.reset();
    }
  }

  /**
   * Deserialize only a single Thrift object (addressed by recursively using field id)
   * from a byte record.
   * @param tb The object to read into
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path tb
   * @param fieldIdPathRest The rest FieldId's that define a path tb
   * @throws TException if an error is encountered during deserialization.
   */
  public void partialDeserialize(TBase tb, byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    try {
      if (locateField(bytes, fieldIdPathFirst, fieldIdPathRest) != null) {
        // if this line is reached, iprot will be positioned at the start of tb.
        tb.read(protocol_);
      }
    } catch (Exception e) {
      throw new TException(e);
    } finally {
      trans_.clear();
      protocol_.reset();
    }
  }

  /**
   * Deserialize only a boolean field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to a boolean field
   * @param fieldIdPathRest The rest FieldId's that define a path to a boolean field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Boolean partialDeserializeBool(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (Boolean) partialDeserializeField(TType.BOOL, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only a byte field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to a byte field
   * @param fieldIdPathRest The rest FieldId's that define a path to a byte field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Byte partialDeserializeByte(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (Byte) partialDeserializeField(TType.BYTE, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only a double field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to a double field
   * @param fieldIdPathRest The rest FieldId's that define a path to a double field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Double partialDeserializeDouble(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (Double) partialDeserializeField(TType.DOUBLE, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only an i16 field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to an i16 field
   * @param fieldIdPathRest The rest FieldId's that define a path to an i16 field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Short partialDeserializeI16(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (Short) partialDeserializeField(TType.I16, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only an i32 field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to an i32 field
   * @param fieldIdPathRest The rest FieldId's that define a path to an i32 field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Integer partialDeserializeI32(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (Integer) partialDeserializeField(TType.I32, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only an i64 field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to an i64 field
   * @param fieldIdPathRest The rest FieldId's that define a path to an i64 field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Long partialDeserializeI64(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (Long) partialDeserializeField(TType.I64, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only a string field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to a string field
   * @param fieldIdPathRest The rest FieldId's that define a path to a string field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public String partialDeserializeString(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    return (String) partialDeserializeField(TType.STRING, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only a binary field (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to a binary field
   * @param fieldIdPathRest The rest FieldId's that define a path to a binary field
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public ByteBuffer partialDeserializeByteArray(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    // TType does not have binary, so we use the arbitrary num 100
    return (ByteBuffer) partialDeserializeField((byte)100, bytes, fieldIdPathFirst, fieldIdPathRest);
  }

  /**
   * Deserialize only the id of the field set in a TUnion (addressed by recursively using field id)
   * from a byte record.
   * @param bytes The serialized object to read from
   * @param fieldIdPathFirst First of the FieldId's that define a path to a TUnion
   * @param fieldIdPathRest The rest FieldId's that define a path to a TUnion
   * @return the deserialized value.
   * @throws TException if an error is encountered during deserialization.
   */
  public Short partialDeserializeSetFieldIdInUnion(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest)  throws TException {
    try {
      TField field = locateField(bytes, fieldIdPathFirst, fieldIdPathRest);
      if (field != null){
        protocol_.readStructBegin(); // The Union
        return protocol_.readFieldBegin().id; // The field set in the union
      }
      return null;
    } catch (Exception e) {
      throw new TException(e);
    } finally {
      trans_.clear();
      protocol_.reset();
    }
  }

  private Object partialDeserializeField(byte ttype, byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    try {
      TField field = locateField(bytes, fieldIdPathFirst, fieldIdPathRest);
      if (field != null) {
        if (ttype == field.type) {
          // if this point is reached, iprot will be positioned at the start of
          // the field
          switch (ttype) {
          case TType.BOOL:
            return protocol_.readBool();
          case TType.BYTE:
            return protocol_.readByte();
          case TType.DOUBLE:
            return protocol_.readDouble();
          case TType.I16:
            return protocol_.readI16();
          case TType.I32:
            return protocol_.readI32();
          case TType.I64:
            return protocol_.readI64();
          case TType.STRING:
            return protocol_.readString();
          default:
            return null;
          }
        }
        // hack to differentiate between string and binary
        if (ttype == 100 && field.type == TType.STRING) {
          return protocol_.readBinary();
        }
      }
      return null;
    } catch (Exception e) {
      throw new TException(e);
    } finally {
      trans_.clear();
      protocol_.reset();
    }
  }

  private TField locateField(byte[] bytes, TFieldIdEnum fieldIdPathFirst, TFieldIdEnum ... fieldIdPathRest) throws TException {
    trans_.reset(bytes);

    TFieldIdEnum[] fieldIdPath = new TFieldIdEnum[fieldIdPathRest.length + 1];
    fieldIdPath[0] = fieldIdPathFirst;
    System.arraycopy(fieldIdPathRest, 0, fieldIdPath, 1, fieldIdPathRest.length);

    // index into field ID path being currently searched for
    int curPathIndex = 0;

    // this will be the located field, or null if it is not located
    TField field = null;

    protocol_.readStructBegin();

    while (curPathIndex < fieldIdPath.length) {
      field = protocol_.readFieldBegin();
      // we can stop searching if we either see a stop or we go past the field
      // id we're looking for (since fields should now be serialized in asc
      // order).
      if (field.type == TType.STOP || field.id > fieldIdPath[curPathIndex].getThriftFieldId()) {
        return null;
      }

      if (field.id != fieldIdPath[curPathIndex].getThriftFieldId()) {
        // Not the field we're looking for. Skip field.
        TProtocolUtil.skip(protocol_, field.type);
        protocol_.readFieldEnd();
      } else {
        // This field is the next step in the path. Step into field.
        curPathIndex++;
        if (curPathIndex < fieldIdPath.length) {
          protocol_.readStructBegin();
        }
      }
    }
    return field;
  }

  /**
   * Deserialize the Thrift object from a Java string, using the default JVM
   * charset encoding.
   *
   * @param base The object to read into
   * @param data The string to read from
   * @throws TException if an error is encountered during deserialization.
   */
  public void fromString(TBase base, String data) throws TException {
    deserialize(base, data.getBytes());
  }

  // ----------------------------------------------------------------------
  // Methods related to partial deserialization.

  /**
   * Partially deserializes the given serialized blob.
   *
   * @param bytes the serialized blob.
   * @return deserialized instance.
   * @throws TException if an error is encountered during deserialization.
   */
  public Object partialDeserializeObject(byte[] bytes) throws TException {
    return this.partialDeserializeObject(bytes, 0, bytes.length);
  }

  /**
   * Partially deserializes the given serialized blob into the given {@code TBase} instance.
   *
   * @param base the instance into which the given blob is deserialized.
   * @param bytes the serialized blob.
   * @param offset the blob is read starting at this offset.
   * @param length the size of blob read (in number of bytes).
   * @return deserialized instance.
   * @throws TException if an error is encountered during deserialization.
   */
  public Object partialDeserializeThriftObject(TBase base, byte[] bytes, int offset, int length)
      throws TException {
    ensurePartialThriftDeserializationMode();

    return this.partialDeserializeObject(base, bytes, offset, length);
  }

  /**
   * Partially deserializes the given serialized blob.
   *
   * @param bytes the serialized blob.
   * @param offset the blob is read starting at this offset.
   * @param length the size of blob read (in number of bytes).
   * @return deserialized instance.
   * @throws TException if an error is encountered during deserialization.
   */
  public Object partialDeserializeObject(byte[] bytes, int offset, int length) throws TException {
    ensurePartialDeserializationMode();

    return this.partialDeserializeObject(null, bytes, offset, length);
  }

  /**
   * Partially deserializes the given serialized blob.
   *
   * @param instance the instance into which the given blob is deserialized.
   * @param bytes the serialized blob.
   * @param offset the blob is read starting at this offset.
   * @param length the size of blob read (in number of bytes).
   * @return deserialized instance.
   * @throws TException if an error is encountered during deserialization.
   */
  private Object partialDeserializeObject(Object instance, byte[] bytes, int offset, int length)
      throws TException {
    ensurePartialDeserializationMode();

    this.trans_.reset(bytes, offset, length);
    this.protocol_.reset();
    return this.deserializeStruct(instance, this.metadata_);
  }

  private Object deserialize(ThriftMetadata.ThriftObject data) throws TException {

    Object value;
    byte fieldType = data.data.valueMetaData.type;
    switch (fieldType) {
      case TType.STRUCT:
        return this.deserializeStruct(null, (ThriftMetadata.ThriftStruct) data);

      case TType.LIST:
        return this.deserializeList((ThriftMetadata.ThriftList) data);

      case TType.MAP:
        return this.deserializeMap((ThriftMetadata.ThriftMap) data);

      case TType.SET:
        return this.deserializeSet((ThriftMetadata.ThriftSet) data);

      case TType.ENUM:
        return this.deserializeEnum((ThriftMetadata.ThriftEnum) data);

      case TType.BOOL:
        return this.protocol_.readBool();

      case TType.BYTE:
        return this.protocol_.readByte();

      case TType.I16:
        return this.protocol_.readI16();

      case TType.I32:
        return this.protocol_.readI32();

      case TType.I64:
        return this.protocol_.readI64();

      case TType.DOUBLE:
        return this.protocol_.readDouble();

      case TType.STRING:
        if (((ThriftMetadata.ThriftPrimitive) data).isBinary()) {
          return this.processor_.prepareBinary(this.protocol_.readBinary());
        } else {
          return this.processor_.prepareString(this.protocol_.readBinary());
        }

      default:
        throw unsupportedFieldTypeException(fieldType);
    }
  }

  private Object deserializeStruct(Object instance, ThriftMetadata.ThriftStruct data)
      throws TException {

    if (instance == null) {
      instance = this.processor_.createNewStruct(data);
    }

    this.protocol_.readStructBegin();
    while (true) {
      int tfieldData = this.protocol_.readFieldBeginData();
      byte tfieldType = TFieldData.getType(tfieldData);
      if (tfieldType == TType.STOP) {
        break;
      }

      Integer id = (int) TFieldData.getId(tfieldData);
      ThriftMetadata.ThriftObject field = (ThriftMetadata.ThriftObject) data.fields.get(id);

      if (field != null) {
        this.deserializeStructField(instance, field.fieldId, field);
      } else {
        this.protocol_.skip(tfieldType);
      }
      this.protocol_.readFieldEnd();
    }
    this.protocol_.readStructEnd();

    return this.processor_.prepareStruct(instance);
  }

  private void deserializeStructField(
      Object instance,
      TFieldIdEnum fieldId,
      ThriftMetadata.ThriftObject data) throws TException {

    byte fieldType = data.data.valueMetaData.type;
    Object value;

    switch (fieldType) {
      case TType.BOOL:
        this.processor_.setBool(instance, fieldId, this.protocol_.readBool());
        break;

      case TType.BYTE:
        this.processor_.setByte(instance, fieldId, this.protocol_.readByte());
        break;

      case TType.I16:
        this.processor_.setInt16(instance, fieldId, this.protocol_.readI16());
        break;

      case TType.I32:
        this.processor_.setInt32(instance, fieldId, this.protocol_.readI32());
        break;

      case TType.I64:
        this.processor_.setInt64(instance, fieldId, this.protocol_.readI64());
        break;

      case TType.DOUBLE:
        this.processor_.setDouble(instance, fieldId, this.protocol_.readDouble());
        break;

      case TType.STRING:
        if (((ThriftMetadata.ThriftPrimitive) data).isBinary()) {
          this.processor_.setBinary(instance, fieldId, this.protocol_.readBinary());
        } else {
          this.processor_.setString(instance, fieldId, this.protocol_.readBinary());
        }
        break;

      case TType.STRUCT:
        value = this.deserializeStruct(null, (ThriftMetadata.ThriftStruct) data);
        this.processor_.setStructField(instance, fieldId, value);
        break;

      case TType.LIST:
        value = this.deserializeList((ThriftMetadata.ThriftList) data);
        this.processor_.setListField(instance, fieldId, value);
        break;

      case TType.MAP:
        value = this.deserializeMap((ThriftMetadata.ThriftMap) data);
        this.processor_.setMapField(instance, fieldId, value);
        break;

      case TType.SET:
        value = this.deserializeSet((ThriftMetadata.ThriftSet) data);
        this.processor_.setSetField(instance, fieldId, value);
        break;

      case TType.ENUM:
        value = this.deserializeEnum((ThriftMetadata.ThriftEnum) data);
        this.processor_.setEnumField(instance, fieldId, value);
        break;

      default:
        throw new RuntimeException("Unsupported field type: " + fieldId.toString());
    }
  }

  private Object deserializeList(ThriftMetadata.ThriftList data) throws TException {

    TList tlist = this.protocol_.readListBegin();
    Object instance = this.processor_.createNewList(tlist.size);
    for (int i = 0; i < tlist.size; i++) {
      Object value = this.deserialize(data.elementData);
      this.processor_.setListElement(instance, i, value);
    }
    this.protocol_.readListEnd();
    return this.processor_.prepareList(instance);
  }

  private Object deserializeMap(ThriftMetadata.ThriftMap data) throws TException {
    TMap tmap = this.protocol_.readMapBegin();
    Object instance = this.processor_.createNewMap(tmap.size);
    for (int i = 0; i < tmap.size; i++) {
      Object key = this.deserialize(data.keyData);
      Object val = this.deserialize(data.valueData);
      this.processor_.setMapElement(instance, i, key, val);
    }
    this.protocol_.readMapEnd();
    return this.processor_.prepareMap(instance);
  }

  private Object deserializeSet(ThriftMetadata.ThriftSet data) throws TException {
    TSet tset = this.protocol_.readSetBegin();
    Object instance = this.processor_.createNewSet(tset.size);
    for (int i = 0; i < tset.size; i++) {
      Object eltValue = this.deserialize(data.elementData);
      this.processor_.setSetElement(instance, i, eltValue);
    }
    this.protocol_.readSetEnd();
    return this.processor_.prepareSet(instance);
  }

  private Object deserializeEnum(ThriftMetadata.ThriftEnum data) throws TException {
    int ordinal = this.protocol_.readI32();
    Class<? extends TEnum> enumClass = ((EnumMetaData) data.data.valueMetaData).enumClass;
    return this.processor_.prepareEnum(enumClass, ordinal);
  }

  private <T extends TBase> Class<T> getStructClass(ThriftMetadata.ThriftStruct data) {
    return (Class<T>) ((StructMetaData) data.data.valueMetaData).structClass;
  }

  private static UnsupportedOperationException unsupportedFieldTypeException(byte fieldType) {
    return new UnsupportedOperationException("field type not supported: " + fieldType);
  }

  private boolean isPartialDeserializationMode() {
    return (this.metadata_ != null) && (this.processor_ != null);
  }

  private void ensurePartialDeserializationMode() throws IllegalStateException {
    if (!this.isPartialDeserializationMode()) {
      throw new IllegalStateException(
          "Members metadata and processor must be correctly initialized in order to use this method"
      );
    }
  }

  private void ensurePartialThriftDeserializationMode() throws IllegalStateException {
    this.ensurePartialDeserializationMode();

    if (!(this.processor_ instanceof ThriftStructProcessor)) {
      throw new IllegalStateException(
          "processor must be an instance of ThriftStructProcessor to use this method"
      );
    }
  }
}
