/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.thrift;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.apache.thrift.meta_data.FieldMetaData;
import org.junit.jupiter.api.Test;
import thrift.test.OneOfEachBeans;
import thrift.test.annotations.OneOfEachBeansWithAnnotations;

public class TestAnnotationMetadata {

  @Test
  public void testWithoutParamShouldGenerateEmpty() {
    Map<? extends TFieldIdEnum, FieldMetaData> structMetaDataMap =
        FieldMetaData.getStructMetaDataMap(OneOfEachBeans.class);
    {
      Map<String, String> metadata =
          structMetaDataMap.get(OneOfEachBeans._Fields.I16_LIST).getFieldAnnotations();
      assertEquals(Collections.emptyMap(), metadata);
    }
    {
      Map<String, String> metadata =
          structMetaDataMap.get(OneOfEachBeans._Fields.A_BITE).getFieldAnnotations();
      assertEquals(Collections.emptyMap(), metadata);
    }
  }

  @Test
  public void testGeneratedAnnotations() {
    Map<? extends TFieldIdEnum, FieldMetaData> structMetaDataMap =
        FieldMetaData.getStructMetaDataMap(OneOfEachBeansWithAnnotations.class);
    {
      Map<String, String> metadata =
          structMetaDataMap
              .get(OneOfEachBeansWithAnnotations._Fields.I16_LIST)
              .getFieldAnnotations();
      assertEquals(Collections.emptyMap(), metadata);
    }
    {
      Map<String, String> metadata =
          structMetaDataMap.get(OneOfEachBeansWithAnnotations._Fields.A_BITE).getFieldAnnotations();
      Map<String, String> expected = new HashMap<>();
      expected.put("compression", "false");
      assertEquals(expected, metadata);
    }
    {
      Map<String, String> metadata =
          structMetaDataMap
              .get(OneOfEachBeansWithAnnotations._Fields.TYPEDEF_META)
              .getFieldAnnotations();
      Map<String, String> expected = new HashMap<>();
      expected.put("a", "b");
      expected.put("c", "d");
      assertEquals(expected, metadata);
    }
  }
}
