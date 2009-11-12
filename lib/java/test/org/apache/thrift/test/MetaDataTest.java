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


package org.apache.thrift.test;

import java.util.Map;
import org.apache.thrift.TFieldRequirementType;
import org.apache.thrift.meta_data.FieldMetaData;
import org.apache.thrift.meta_data.ListMetaData;
import org.apache.thrift.meta_data.MapMetaData;
import org.apache.thrift.meta_data.SetMetaData;
import org.apache.thrift.meta_data.StructMetaData;
import org.apache.thrift.protocol.TType;
import org.apache.thrift.TFieldIdEnum;
import thrift.test.*;

public class MetaDataTest {
  public static void main(String[] args) throws Exception {
    Map<CrazyNesting._Fields, FieldMetaData> mdMap = CrazyNesting.metaDataMap;

    // Check for struct fields existence
    if (mdMap.size() != 3)
      throw new RuntimeException("metadata map contains wrong number of entries!");
    if (!mdMap.containsKey(CrazyNesting._Fields.SET_FIELD) || !mdMap.containsKey(CrazyNesting._Fields.LIST_FIELD) || !mdMap.containsKey(CrazyNesting._Fields.STRING_FIELD))
      throw new RuntimeException("metadata map doesn't contain entry for a struct field!");

    // Check for struct fields contents
    if (!mdMap.get(CrazyNesting._Fields.STRING_FIELD).fieldName.equals("string_field") ||
            !mdMap.get(CrazyNesting._Fields.LIST_FIELD).fieldName.equals("list_field") ||
            !mdMap.get(CrazyNesting._Fields.SET_FIELD).fieldName.equals("set_field"))
      throw new RuntimeException("metadata map contains a wrong fieldname");
    if (mdMap.get(CrazyNesting._Fields.STRING_FIELD).requirementType != TFieldRequirementType.DEFAULT ||
            mdMap.get(CrazyNesting._Fields.LIST_FIELD).requirementType != TFieldRequirementType.REQUIRED ||
            mdMap.get(CrazyNesting._Fields.SET_FIELD).requirementType != TFieldRequirementType.OPTIONAL)
      throw new RuntimeException("metadata map contains the wrong requirement type for a field");
    if (mdMap.get(CrazyNesting._Fields.STRING_FIELD).valueMetaData.type != TType.STRING ||
            mdMap.get(CrazyNesting._Fields.LIST_FIELD).valueMetaData.type != TType.LIST ||
            mdMap.get(CrazyNesting._Fields.SET_FIELD).valueMetaData.type != TType.SET)
      throw new RuntimeException("metadata map contains the wrong requirement type for a field");

    // Check nested structures
    if (!mdMap.get(CrazyNesting._Fields.LIST_FIELD).valueMetaData.isContainer())
      throw new RuntimeException("value metadata for a list is stored as non-container!");
    if (mdMap.get(CrazyNesting._Fields.LIST_FIELD).valueMetaData.isStruct())
      throw new RuntimeException("value metadata for a list is stored as a struct!");
    if (((MapMetaData)((ListMetaData)((SetMetaData)((MapMetaData)((MapMetaData)((ListMetaData)mdMap.get(CrazyNesting._Fields.LIST_FIELD).valueMetaData).elemMetaData).valueMetaData).valueMetaData).elemMetaData).elemMetaData).keyMetaData.type != TType.STRUCT)
      throw new RuntimeException("metadata map contains wrong type for a value in a deeply nested structure");
    if (((StructMetaData)((MapMetaData)((ListMetaData)((SetMetaData)((MapMetaData)((MapMetaData)((ListMetaData)mdMap.get(CrazyNesting._Fields.LIST_FIELD).valueMetaData).elemMetaData).valueMetaData).valueMetaData).elemMetaData).elemMetaData).keyMetaData).structClass != Insanity.class)
      throw new RuntimeException("metadata map contains wrong class for a struct in a deeply nested structure");

    // Check that FieldMetaData contains a map with metadata for all generated struct classes
    if (FieldMetaData.getStructMetaDataMap(CrazyNesting.class) == null ||
            FieldMetaData.getStructMetaDataMap(Insanity.class) == null ||
            FieldMetaData.getStructMetaDataMap(Xtruct.class) == null)
      throw new RuntimeException("global metadata map doesn't contain an entry for a known struct");
    if (FieldMetaData.getStructMetaDataMap(CrazyNesting.class) != CrazyNesting.metaDataMap ||
            FieldMetaData.getStructMetaDataMap(Insanity.class) != Insanity.metaDataMap)
      throw new RuntimeException("global metadata map contains wrong entry for a loaded struct");    

    for (Map.Entry<? extends TFieldIdEnum, FieldMetaData> mdEntry : mdMap.entrySet()) {
      if (!CrazyNesting._Fields.findByName(mdEntry.getValue().fieldName).equals(mdEntry.getKey())) {
        throw new RuntimeException("Field name map contained invalid Name <-> ID mapping");
      }
    }
  }
}
