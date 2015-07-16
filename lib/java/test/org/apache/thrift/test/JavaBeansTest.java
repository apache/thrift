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

import java.nio.ByteBuffer;
import java.util.LinkedList;

import thrift.test.OneOfEachBeans;

public class JavaBeansTest {
  public static void main(String[] args) throws Exception {
    // Test isSet methods
    OneOfEachBeans ooe = new OneOfEachBeans();

    // Nothing should be set
    if (ooe.isSetA_bite())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetBase64())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetByte_list())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetDouble_precision())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetI16_list())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetI64_list())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetBoolean_field())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetInteger16())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetInteger32())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetInteger64())
      throw new RuntimeException("isSet method error: unset field returned as set!");
    if (ooe.isSetSome_characters())
      throw new RuntimeException("isSet method error: unset field returned as set!");

    for (int i = 1; i < 12; i++){
      if (ooe.isSet(ooe.fieldForId(i)))
        throw new RuntimeException("isSet method error: unset field " + i + " returned as set!");
    }

    // Everything is set
    ooe.setA_bite((byte) 1);
    ooe.setBase64(ByteBuffer.wrap("bytes".getBytes()));
    ooe.setByte_list(new LinkedList<Byte>());
    ooe.setDouble_precision(1);
    ooe.setI16_list(new LinkedList<Short>());
    ooe.setI64_list(new LinkedList<Long>());
    ooe.setBoolean_field(true);
    ooe.setInteger16((short) 1);
    ooe.setInteger32(1);
    ooe.setInteger64(1);
    ooe.setSome_characters("string");

    if (!ooe.isSetA_bite())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetBase64())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetByte_list())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetDouble_precision())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetI16_list())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetI64_list())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetBoolean_field())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetInteger16())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetInteger32())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetInteger64())
      throw new RuntimeException("isSet method error: set field returned as unset!");
    if (!ooe.isSetSome_characters())
      throw new RuntimeException("isSet method error: set field returned as unset!");

    for (int i = 1; i < 12; i++){
      if (!ooe.isSet(ooe.fieldForId(i)))
        throw new RuntimeException("isSet method error: set field " + i + " returned as unset!");
    }

    // Should throw exception when field doesn't exist
    boolean exceptionThrown = false;
    try{
      if (ooe.isSet(ooe.fieldForId(100)));
    } catch (IllegalArgumentException e){
      exceptionThrown = true;
    }
    if (!exceptionThrown)
      throw new RuntimeException("isSet method error: non-existent field provided as agument but no exception thrown!");
  }
}
