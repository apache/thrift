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
import org.apache.thrift.TFieldIdEnum;
import org.apache.thrift.TFieldRequirementType;
import org.apache.thrift.TUnion;
import org.apache.thrift.meta_data.FieldMetaData;
import org.apache.thrift.meta_data.FieldValueMetaData;
import org.apache.thrift.meta_data.ListMetaData;
import org.apache.thrift.meta_data.MapMetaData;
import org.apache.thrift.meta_data.SetMetaData;
import org.apache.thrift.meta_data.StructMetaData;
import org.apache.thrift.protocol.TType;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Container for Thrift metadata classes such as {@link ThriftPrimitive},
 * {@link ThriftList}, etc.
 * <p>
 * This class is mainly used by {@link PartialThriftDeserializer}.
 */
public class ThriftMetadata {

  enum FieldTypeEnum implements TFieldIdEnum {
    ROOT((short) 0, "root"),
    ENUM((short) 1, "enum"),
    LIST_ELEMENT((short) 2, "listElement"),
    MAP_KEY((short) 3, "mapKey"),
    MAP_VALUE((short) 4, "mapValue"),
    SET_ELEMENT((short) 5, "setElement");

    private short id;
    private String name;

    FieldTypeEnum(short id, String name) {
      this.id = id;
      this.name = name;
    }

    @Override
    public short getThriftFieldId() {
      return id;
    }

    @Override
    public String getFieldName() {
      return name;
    }
  }

  private enum ComparisonResult {
    UNKNOWN,
    EQUAL,
    NOT_EQUAL
  }

  /**
   * Base class of field types that can be partially deserialized.
   *
   * Holds metadata necessary for partial deserialization.
   * The metadata is internally computed and used; therefore it is not visible to
   * the users of {@link PartialThriftDeserializer}.
   */
  abstract static class ThriftObject implements Serializable {
    public final ThriftObject parent;
    public final TFieldIdEnum fieldId;
    public final FieldMetaData data;

    // Placeholder to attach additional data. This class or its descendents
    // do not try to access or interpret this field.
    public Object additionalData;

    ThriftObject(ThriftObject parent, TFieldIdEnum fieldId, FieldMetaData data) {
      this.parent = parent;
      this.fieldId = fieldId;
      this.data = data;
    }

    @Override
    public String toString() {
      String parentId = parent == null ? "" : this.parent.toString() + " ==> ";
      return String.format("%s%s", parentId, this.fieldId.getFieldName());
    }

    protected List<String> noFields = Collections.emptyList();

    protected String getSubElementName(TFieldIdEnum fieldId) {
      return getSubElementName(fieldId, "element");
    }

    protected String getSubElementName(TFieldIdEnum fieldId, String suffix) {
      return String.format("%s_%s", fieldId.getFieldName(), suffix);
    }

    private static class Factory {

      static ThriftObject createNew(
          ThriftObject parent,
          TFieldIdEnum fieldId,
          FieldMetaData data,
          List<ThriftField> fields) {

        byte fieldType = data.valueMetaData.type;
        switch (fieldType) {
          case TType.STRUCT:
            return ThriftStructBase.create(parent, fieldId, data, fields);

          case TType.LIST:
            return new ThriftList(parent, fieldId, data, fields);

          case TType.MAP:
            return new ThriftMap(parent, fieldId, data, fields);

          case TType.SET:
            return new ThriftSet(parent, fieldId, data, fields);

          case TType.ENUM:
            return new ThriftEnum(parent, fieldId, data);

          case TType.BOOL:
          case TType.BYTE:
          case TType.I16:
          case TType.I32:
          case TType.I64:
          case TType.DOUBLE:
          case TType.STRING:
            return new ThriftPrimitive(parent, fieldId, data);

          default:
            throw unsupportedFieldTypeException(fieldType);
        }
      }
    }
  }

  /**
   * Metadata about primitive types.
   */
  public static class ThriftPrimitive extends ThriftObject {
    // If true and if type is TType.STRING, implies a binary field.
    // A separate state like this is required because 'binary' is not a
    // separate primitive type in Thrift. That is, Thrift implementation
    // overloads TType.STRING to mean either string or binary depending
    // upon external state.
    private boolean isBinaryField = false;

    ThriftPrimitive(ThriftObject parent, TFieldIdEnum fieldId, FieldMetaData data) {
      super(parent, fieldId, data);

      if (data.valueMetaData.type == TType.STRING) {
        ThriftStruct parentStruct = getParentStruct();
        if (parentStruct != null) {
          this.isBinaryField = parentStruct.isBinaryField(data.valueMetaData);
        }
      }
    }

    public boolean isBinary() {
      return this.isBinaryField;
    }

    private ThriftStruct getParentStruct() {
      ThriftObject tparent = parent;
      while (tparent != null) {
        if (tparent instanceof ThriftStruct) {
          return (ThriftStruct) tparent;
        }
        tparent = tparent.parent;
      }
      return null;
    }
  }

  public static class ThriftEnum extends ThriftObject {
    private static EnumCache enums = new EnumCache();

    ThriftEnum(ThriftObject parent, TFieldIdEnum fieldId, FieldMetaData data) {
      super(parent, fieldId, data);
    }
  }

  /**
   * Metadata of container like objects: list, set, map
   */
  public abstract static class ThriftContainer extends ThriftObject {

    public ThriftContainer(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data) {
      super(parent, fieldId, data);
    }

    public abstract boolean hasUnion();
  }

  public static class ThriftList extends ThriftContainer {
    public final ThriftObject elementData;

    ThriftList(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        List<ThriftField> fields) {
      super(parent, fieldId, data);

      this.elementData = ThriftObject.Factory.createNew(
          this,
          FieldTypeEnum.LIST_ELEMENT,
          new FieldMetaData(
              getSubElementName(fieldId),
              TFieldRequirementType.REQUIRED,
              ((ListMetaData) data.valueMetaData).elemMetaData),
          fields);
    }

    @Override
    public boolean hasUnion() {
      return this.elementData instanceof ThriftUnion;
    }
  }

  public static class ThriftSet extends ThriftContainer {
    public final ThriftObject elementData;

    ThriftSet(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        List<ThriftField> fields) {
      super(parent, fieldId, data);

      this.elementData = ThriftObject.Factory.createNew(
          this,
          FieldTypeEnum.SET_ELEMENT,
          new FieldMetaData(
              getSubElementName(fieldId),
              TFieldRequirementType.REQUIRED,
              ((SetMetaData) data.valueMetaData).elemMetaData),
          fields);
    }

    @Override
    public boolean hasUnion() {
      return this.elementData instanceof ThriftUnion;
    }
  }

  public static class ThriftMap extends ThriftContainer {
    public final ThriftObject keyData;
    public final ThriftObject valueData;

    ThriftMap(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        List<ThriftField> fields) {
      super(parent, fieldId, data);

      this.keyData = ThriftObject.Factory.createNew(
          this,
          FieldTypeEnum.MAP_KEY,
          new FieldMetaData(
              getSubElementName(fieldId, "key"),
              TFieldRequirementType.REQUIRED,
              ((MapMetaData) data.valueMetaData).keyMetaData),
          Collections.emptyList());

      this.valueData = ThriftObject.Factory.createNew(
          this,
          FieldTypeEnum.MAP_VALUE,
          new FieldMetaData(
              getSubElementName(fieldId, "value"),
              TFieldRequirementType.REQUIRED,
              ((MapMetaData) data.valueMetaData).valueMetaData),
          Collections.emptyList());
    }

    @Override
    public boolean hasUnion() {
      return (this.keyData instanceof ThriftUnion) || (this.valueData instanceof ThriftUnion);
    }
  }

  /**
   * Base class for metadata of ThriftStruct and ThriftUnion.
   * Holds functionality that is common to both.
   */
  public abstract static class ThriftStructBase<U extends TBase> extends ThriftObject {
    public ThriftStructBase(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data) {
      super(parent, fieldId, data);
    }

    public Class<U> getStructClass() {
      return getStructClass(this.data);
    }

    public static <U extends TBase> Class<U> getStructClass(FieldMetaData data) {
      return (Class<U>) ((StructMetaData) data.valueMetaData).structClass;
    }

    public boolean isUnion() {
      return isUnion(this.data);
    }

    public static boolean isUnion(FieldMetaData data) {
      return TUnion.class.isAssignableFrom(getStructClass(data));
    }

    public static ThriftStructBase create(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        Iterable<ThriftField> fieldsData) {

      if (isUnion(data)) {
        return new ThriftUnion(parent, fieldId, data, fieldsData);
      } else {
        return new ThriftStruct(parent, fieldId, data, fieldsData);
      }
    }
  }

  /**
   * Metadata of a Thrift union.
   * Currently not adequately supported.
   */
  public static class ThriftUnion<U extends TBase> extends ThriftStructBase {
    public ThriftUnion(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        Iterable<ThriftField> fieldsData) {
      super(parent, fieldId, data);
    }
  }

  /**
   * Metadata of a Thrift struct.
   */
  public static class ThriftStruct<U extends TBase> extends ThriftStructBase {
    public final Map<Integer, ThriftObject> fields;

    // Contains metadata of fields of TType.STRING that should be treated as binary fields.
    private Set<FieldValueMetaData> binaryFieldValueMetaDatas;

    ThriftStruct(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        Iterable<ThriftField> fieldsData) {
      super(parent, fieldId, data);

      Class<U> clasz = getStructClass(data);
      initBinaryFieldValueMetaDatas(clasz);
      this.fields = getFields(this, clasz, fieldsData);
    }

    public <T extends TBase> T createNewStruct() {
      T instance = null;

      try {
        Class<T> structClass = getStructClass(this.data);
        instance = (T) structClass.newInstance();
      } catch (InstantiationException e) {
        throw new RuntimeException(e);
      } catch (IllegalAccessException e) {
        throw new RuntimeException(e);
      }

      return instance;
    }

    public boolean isBinaryField(FieldValueMetaData fieldValueMetadata) {
      return (this.binaryFieldValueMetaDatas != null)
          && this.binaryFieldValueMetaDatas.contains(fieldValueMetadata);
    }

    public static <T extends TBase> ThriftStruct of(Class<T> clasz) {
      return ThriftStruct.fromFields(clasz, Collections.emptyList());
    }

    public static <T extends TBase> ThriftStruct fromFields(
        Class<T> clasz,
        Iterable<ThriftField> fields) {

      Validate.checkNotNull(clasz, "clasz");
      Validate.checkNotNull(fields, "fields");

      return new ThriftStruct(
          null,
          FieldTypeEnum.ROOT,
          new FieldMetaData(
              FieldTypeEnum.ROOT.getFieldName(),
              TFieldRequirementType.REQUIRED,
              new StructMetaData(TType.STRUCT, clasz)),
          fields);
    }

    private static <U extends TBase> Map<Integer, ThriftObject> getFields(
        ThriftStruct parent,
        Class<U> clasz,
        Iterable<ThriftField> fieldsData) {

      Map<? extends TFieldIdEnum, FieldMetaData> fieldsMetaData =
          FieldMetaData.getStructMetaDataMap(clasz);
      Map<Integer, ThriftObject> fields = new HashMap();
      boolean getAllFields = !fieldsData.iterator().hasNext();

      if (getAllFields) {
        for (Map.Entry<? extends TFieldIdEnum, FieldMetaData> entry : fieldsMetaData.entrySet()) {
          TFieldIdEnum fieldId = entry.getKey();
          FieldMetaData fieldMetaData = entry.getValue();
          ThriftObject field =
              ThriftObject.Factory.createNew(parent, fieldId, fieldMetaData, Collections.emptyList());
          fields.put((int) fieldId.getThriftFieldId(), field);
        }
      } else {
        for (ThriftField fieldData : fieldsData) {
          String fieldName = fieldData.name;
          FieldMetaData fieldMetaData = findFieldMetaData(fieldsMetaData, fieldName);
          TFieldIdEnum fieldId = findFieldId(fieldsMetaData, fieldName);
          ThriftObject field =
              ThriftObject.Factory.createNew(parent, fieldId, fieldMetaData, fieldData.fields);
          fields.put((int) fieldId.getThriftFieldId(), field);
        }
      }

      return fields;
    }

    private static FieldMetaData findFieldMetaData(
        Map<? extends TFieldIdEnum, FieldMetaData> fieldsMetaData,
        String fieldName) {

      for (FieldMetaData fieldData : fieldsMetaData.values()) {
        if (fieldData.fieldName.equals(fieldName)) {
          return fieldData;
        }
      }

      throw fieldNotFoundException(fieldName);
    }

    private static TFieldIdEnum findFieldId(
        Map<? extends TFieldIdEnum, FieldMetaData> fieldsMetaData,
        String fieldName) {

      for (TFieldIdEnum fieldId : fieldsMetaData.keySet()) {
        if (fieldId.getFieldName().equals(fieldName)) {
          return fieldId;
        }
      }

      throw fieldNotFoundException(fieldName);
    }

    private void initBinaryFieldValueMetaDatas(Class<U> clasz) {
      try {
        Field field = clasz.getField("binaryFieldValueMetaDatas");
        this.binaryFieldValueMetaDatas = (Set<FieldValueMetaData>) field.get(null);
      } catch (NoSuchFieldException e) {
        // If this field is missing, there are no binary fields in this class.
      } catch (IllegalAccessException e) {
        // If this field is inaccessible, there are no binary fields in this class.
      }
    }
  }

  static IllegalArgumentException fieldNotFoundException(String fieldName) {
    return new IllegalArgumentException("field not found: '" + fieldName + "'");
  }

  static UnsupportedOperationException unsupportedFieldTypeException(byte fieldType) {
    return new UnsupportedOperationException("field type not supported: '" + fieldType + "'");
  }
}
