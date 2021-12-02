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

import org.apache.commons.lang3.StringUtils;
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
import org.apache.thrift.partial.Validate;
import org.apache.thrift.protocol.TType;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Container for Thrift metadata classes such as {@link ThriftPrimitive},
 * {@link ThriftList}, etc.
 * <p>
 * This class is mainly used by {@code TDeserializer}.
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
   * the users of {@code TDeserializer}.
   */
  public abstract static class ThriftObject implements Serializable {
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

    /**
     * Converts this instance to formatted and indented string representation.
     *
     * @param sb the {@code StringBuilder} to add formatted strings to.
     * @param level the current indent level.
     */
    protected abstract void toPrettyString(StringBuilder sb, int level);

    /**
     * Gets a space string whose length is proportional to the given indent level.
     */
    protected String getIndent(int level) {
      return StringUtils.repeat(" ", level * 4);
    }

    /**
     * Helper method to append a formatted string to the given {@code StringBuilder}.
     */
    protected void append(StringBuilder sb, String format, Object... args) {
      sb.append(String.format(format, args));
    }

    /**
     * Gets the name of this field.
     */
    protected String getName() {
      return this.fieldId.getFieldName();
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
    ThriftPrimitive(ThriftObject parent, TFieldIdEnum fieldId, FieldMetaData data) {
      super(parent, fieldId, data);
    }

    public boolean isBinary() {
      return this.data.valueMetaData.isBinary();
    }

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      String fieldType = this.getTypeName();
      this.append(sb, "%s%s %s;\n", this.getIndent(level), fieldType, this.getName());
    }

    private String getTypeName() {
      byte fieldType = this.data.valueMetaData.type;
      switch (fieldType) {
        case TType.BOOL:
          return "bool";

        case TType.BYTE:
          return "byte";

        case TType.I16:
          return "i16";

        case TType.I32:
          return "i32";

        case TType.I64:
          return "i64";

        case TType.DOUBLE:
          return "double";

        case TType.STRING:
          if (this.isBinary()) {
            return "binary";
          } else {
            return "string";
          }

        default:
          throw unsupportedFieldTypeException(fieldType);
      }
    }
  }

  public static class ThriftEnum extends ThriftObject {
    private static EnumCache enums = new EnumCache();

    ThriftEnum(ThriftObject parent, TFieldIdEnum fieldId, FieldMetaData data) {
      super(parent, fieldId, data);
    }

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      this.append(sb, "%senum %s;\n", this.getIndent(level), this.getName());
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

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      this.append(sb, "%slist<\n", this.getIndent(level));
      this.elementData.toPrettyString(sb, level + 1);
      this.append(sb, "%s> %s;\n", this.getIndent(level), this.getName());
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

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      this.append(sb, "%sset<\n", this.getIndent(level));
      this.elementData.toPrettyString(sb, level + 1);
      this.append(sb, "%s> %s;\n", this.getIndent(level), this.getName());
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
          fields);
    }

    @Override
    public boolean hasUnion() {
      return (this.keyData instanceof ThriftUnion) || (this.valueData instanceof ThriftUnion);
    }

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      this.append(sb, "%smap<\n", this.getIndent(level));
      this.append(sb, "%skey = {\n", this.getIndent(level + 1));
      this.keyData.toPrettyString(sb, level + 2);
      this.append(sb, "%s},\n", this.getIndent(level + 1));
      this.append(sb, "%svalue = {\n", this.getIndent(level + 1));
      this.valueData.toPrettyString(sb, level + 2);
      this.append(sb, "%s}\n", this.getIndent(level + 1));
      this.append(sb, "%s> %s;\n", this.getIndent(level), this.getName());
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

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      String indent = this.getIndent(level);
      String indent2 = this.getIndent(level + 1);
      this.append(sb, "%sunion %s {\n", indent, this.getName());
      this.append(sb, "%s// unions not adequately supported at present.\n", indent2);
      this.append(sb, "%s}\n", indent);
    }
  }

  /**
   * Metadata of a Thrift struct.
   */
  public static class ThriftStruct<U extends TBase> extends ThriftStructBase {
    public final Map<Integer, ThriftObject> fields;

    ThriftStruct(
        ThriftObject parent,
        TFieldIdEnum fieldId,
        FieldMetaData data,
        Iterable<ThriftField> fieldsData) {
      super(parent, fieldId, data);

      Class<U> clasz = getStructClass(data);
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

    public static <T extends TBase> ThriftStruct of(Class<T> clasz) {
      return ThriftStruct.fromFields(clasz, Collections.emptyList());
    }

    public static <T extends TBase> ThriftStruct fromFieldNames(
        Class<T> clasz,
        Collection<String> fieldNames) {
      return fromFields(clasz, ThriftField.fromNames(fieldNames));
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

    @Override
    public String toString() {
      StringBuilder sb = new StringBuilder();
      this.toPrettyString(sb, 0);
      return sb.toString();
    }

    @Override
    protected void toPrettyString(StringBuilder sb, int level) {
      String indent = this.getIndent(level);
      String indent2 = this.getIndent(level + 1);
      this.append(sb, "%sstruct %s {\n", indent, this.getName());
      if (this.fields.size() == 0) {
        this.append(sb, "%s*;", indent2);
      } else {
        List<Integer> ids = new ArrayList(this.fields.keySet());
        Collections.sort(ids);
        for (Integer id : ids) {
          this.fields.get(id).toPrettyString(sb, level + 1);
        }
      }
      this.append(sb, "%s}\n", indent);
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
  }

  static IllegalArgumentException fieldNotFoundException(String fieldName) {
    return new IllegalArgumentException("field not found: '" + fieldName + "'");
  }

  static UnsupportedOperationException unsupportedFieldTypeException(byte fieldType) {
    return new UnsupportedOperationException("field type not supported: '" + fieldType + "'");
  }
}
