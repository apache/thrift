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
import org.apache.thrift.partial.ExceptionAsserts;

import org.apache.thrift.TBase;
import org.apache.thrift.meta_data.EnumMetaData;
import org.apache.thrift.meta_data.FieldValueMetaData;
import org.apache.thrift.meta_data.ListMetaData;
import org.apache.thrift.meta_data.MapMetaData;
import org.apache.thrift.meta_data.SetMetaData;
import org.apache.thrift.meta_data.StructMetaData;
import org.apache.thrift.protocol.TType;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

public class ThriftMetadataTest {

  private PartialThriftTestData testData = new PartialThriftTestData();

  @Test
  public void testArgChecks() {
    // Should not throw.
    List<ThriftField> testFields = ThriftField.fromNames(Arrays.asList("byteField"));
    ThriftMetadata.ThriftStruct.fromFields(TestStruct.class, testFields);

    // Verify it throws correctly.
    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'clasz' must not be null",
        () -> ThriftMetadata.ThriftStruct.fromFields(null, testFields));

    ExceptionAsserts.assertThrows(
        IllegalArgumentException.class,
        "'fields' must not be null",
        () -> ThriftMetadata.ThriftStruct.fromFields(TestStruct.class, null));
  }

  @Test
  public void testThriftStructOf() {
    ThriftMetadata.ThriftStruct testStruct = ThriftMetadata.ThriftStruct.of(TestStruct.class);
    assertEquals(45, testStruct.fields.keySet().size());
    validateFieldMetadata(testStruct, 1, "byteField", TType.BYTE);
    validateFieldMetadata(testStruct, 2, "i16Field", TType.I16);
    validateFieldMetadata(testStruct, 3, "i32Field", TType.I32);
    validateFieldMetadata(testStruct, 4, "i64Field", TType.I64);
    validateFieldMetadata(testStruct, 5, "doubleField", TType.DOUBLE);
    validateFieldMetadata(testStruct, 6, "stringField", TType.STRING);
    validateFieldMetadata(testStruct, 7, "enumField", TType.ENUM);
    validateFieldMetadata(testStruct, 8, "binaryField", TType.STRING);

    validateListFieldMetadata(testStruct, 10, "byteList", TType.BYTE);
    validateSetFieldMetadata(testStruct, 35, "stringSet", TType.STRING);
    validateMapFieldMetadata(testStruct, 61, "binaryMap", TType.I32, TType.STRING);
  }

  @Test
  public void testUnion() {
    ThriftMetadata.ThriftStruct structWithUnions =
        ThriftMetadata.ThriftStruct.of(StructWithUnions.class);
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 1, "intValue");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 2, "smallStruct");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 3, "simpleUnion");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 4, "unionList");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 5, "unionSet");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 6, "keyUnionMap");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 7, "valUnionMap");
    validateBasicFieldMetadata(structWithUnions, StructWithUnions.class, 8, "unionMap");

    ThriftMetadata.ThriftStructBase smallStructMetadata =
        (ThriftMetadata.ThriftStructBase) structWithUnions.fields.get(2);
    assertFalse(smallStructMetadata.isUnion());

    ThriftMetadata.ThriftStructBase simpleUnionMetadata =
        (ThriftMetadata.ThriftStructBase) structWithUnions.fields.get(3);
    assertTrue(simpleUnionMetadata.isUnion());

    ThriftMetadata.ThriftList unionListMetadata =
        (ThriftMetadata.ThriftList) structWithUnions.fields.get(4);
    assertTrue(unionListMetadata.hasUnion());

    ThriftMetadata.ThriftSet unionSetMetadata =
        (ThriftMetadata.ThriftSet) structWithUnions.fields.get(5);
    assertTrue(unionSetMetadata.hasUnion());

    ThriftMetadata.ThriftMap keyUnionMapMetadata =
        (ThriftMetadata.ThriftMap) structWithUnions.fields.get(6);
    assertTrue(keyUnionMapMetadata.hasUnion());

    ThriftMetadata.ThriftMap valUnionMapMetadata =
        (ThriftMetadata.ThriftMap) structWithUnions.fields.get(7);
    assertTrue(valUnionMapMetadata.hasUnion());

    ThriftMetadata.ThriftMap unionMapMetadata =
        (ThriftMetadata.ThriftMap) structWithUnions.fields.get(8);
    assertTrue(unionMapMetadata.hasUnion());
  }

  private ThriftMetadata.ThriftObject validateBasicFieldMetadata(
      ThriftMetadata.ThriftStruct testStruct,
      int id,
      String fieldName) {
    return validateBasicFieldMetadata(testStruct, TestStruct.class, id, fieldName);
  }

  private ThriftMetadata.ThriftObject validateBasicFieldMetadata(
      ThriftMetadata.ThriftStruct testStruct,
      Class<? extends TBase> clazz,
      int id,
      String fieldName) {

    assertNotNull(testStruct);
    assertNull(testStruct.parent);
    assertEquals(clazz, ((StructMetaData) testStruct.data.valueMetaData).structClass);
    assertTrue(testStruct.fields.containsKey(id));

    ThriftMetadata.ThriftObject fieldMetadata =
        (ThriftMetadata.ThriftObject) testStruct.fields.get(id);
    assertEquals(testStruct, fieldMetadata.parent);

    assertEquals(id, fieldMetadata.fieldId.getThriftFieldId());
    assertEquals(fieldName, fieldMetadata.fieldId.getFieldName());
    assertEquals(fieldName, fieldMetadata.data.fieldName);

    assertEquals("root ==> " + fieldName, fieldMetadata.toString());

    return fieldMetadata;
  }

  private void validateBasicFieldValueMetadata(
      ThriftMetadata.ThriftObject fieldMetadata,
      String fieldName,
      byte ttype) {

    assertEquals(ttype, fieldMetadata.data.valueMetaData.type);
    assertEquals(getMetaDataClassForTType(ttype), fieldMetadata.data.valueMetaData.getClass());
    Class<? extends ThriftMetadata.ThriftObject> fieldMetadataClass = getClassForTType(ttype);
    assertEquals(fieldMetadataClass, fieldMetadata.getClass());
    if (fieldMetadataClass == ThriftMetadata.ThriftPrimitive.class) {
      ThriftMetadata.ThriftPrimitive primitive
          = (ThriftMetadata.ThriftPrimitive) fieldMetadata;
      if (fieldName.startsWith("binary") && (ttype == TType.STRING)) {
        assertTrue(primitive.isBinary());
      } else {
        assertFalse(primitive.isBinary());
      }
    }
  }

  private void validateFieldMetadata(
      ThriftMetadata.ThriftStruct testStruct,
      int id,
      String fieldName,
      byte ttype) {

    ThriftMetadata.ThriftObject fieldMetadata =
        validateBasicFieldMetadata(testStruct, id, fieldName);
    validateBasicFieldValueMetadata(fieldMetadata, fieldName, ttype);
  }

  private void validateListFieldMetadata(
      ThriftMetadata.ThriftStruct testStruct,
      int id,
      String fieldName,
      byte ttype) {

    ThriftMetadata.ThriftObject fieldMetadata =
        validateBasicFieldMetadata(testStruct, id, fieldName);
    validateBasicFieldValueMetadata(fieldMetadata, fieldName, TType.LIST);

    ThriftMetadata.ThriftList thriftList = (ThriftMetadata.ThriftList) fieldMetadata;
    ThriftMetadata.ThriftObject elementMetadata = thriftList.elementData;
    validateBasicFieldValueMetadata(elementMetadata, fieldName + "_element", ttype);
  }

  private void validateSetFieldMetadata(
      ThriftMetadata.ThriftStruct testStruct,
      int id,
      String fieldName,
      byte ttype) {

    ThriftMetadata.ThriftObject fieldMetadata =
        validateBasicFieldMetadata(testStruct, id, fieldName);
    validateBasicFieldValueMetadata(fieldMetadata, fieldName, TType.SET);

    ThriftMetadata.ThriftSet thriftSet = (ThriftMetadata.ThriftSet) fieldMetadata;
    ThriftMetadata.ThriftObject elementMetadata = thriftSet.elementData;
    validateBasicFieldValueMetadata(elementMetadata, fieldName + "_element", ttype);
  }

  private void validateMapFieldMetadata(
      ThriftMetadata.ThriftStruct testStruct,
      int id,
      String fieldName,
      byte keyType,
      byte valueType) {

    ThriftMetadata.ThriftObject fieldMetadata =
        validateBasicFieldMetadata(testStruct, id, fieldName);
    validateBasicFieldValueMetadata(fieldMetadata, fieldName, TType.MAP);

    ThriftMetadata.ThriftMap thriftMap = (ThriftMetadata.ThriftMap) fieldMetadata;
    ThriftMetadata.ThriftObject keyMetadata = thriftMap.keyData;
    ThriftMetadata.ThriftObject valueMetadata = thriftMap.valueData;
    validateBasicFieldValueMetadata(keyMetadata, fieldName + "_key", keyType);
    validateBasicFieldValueMetadata(valueMetadata, fieldName + "_value", valueType);
  }

  private Class<? extends FieldValueMetaData> getMetaDataClassForTType(byte ttype) {
    switch (ttype) {
      case TType.STRUCT:
        return  StructMetaData.class;

      case TType.LIST:
        return ListMetaData.class;

      case TType.MAP:
        return MapMetaData.class;

      case TType.SET:
        return SetMetaData.class;

      case TType.ENUM:
        return EnumMetaData.class;

      case TType.BOOL:
      case TType.BYTE:
      case TType.I16:
      case TType.I32:
      case TType.I64:
      case TType.DOUBLE:
      case TType.STRING:
        return FieldValueMetaData.class;

      default:
        throw ThriftMetadata.unsupportedFieldTypeException(ttype);
    }
  }

  private Class<? extends ThriftMetadata.ThriftObject> getClassForTType(byte ttype) {
    switch (ttype) {
      case TType.STRUCT:
        return  ThriftMetadata.ThriftStruct.class;

      case TType.LIST:
        return ThriftMetadata.ThriftList.class;

      case TType.MAP:
        return ThriftMetadata.ThriftMap.class;

      case TType.SET:
        return ThriftMetadata.ThriftSet.class;

      case TType.ENUM:
        return ThriftMetadata.ThriftEnum.class;

      case TType.BOOL:
      case TType.BYTE:
      case TType.I16:
      case TType.I32:
      case TType.I64:
      case TType.DOUBLE:
      case TType.STRING:
        return ThriftMetadata.ThriftPrimitive.class;

      default:
        throw ThriftMetadata.unsupportedFieldTypeException(ttype);
    }
  }
}
