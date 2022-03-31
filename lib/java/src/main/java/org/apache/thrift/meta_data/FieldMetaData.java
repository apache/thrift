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

package org.apache.thrift.meta_data;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.thrift.TBase;
import org.apache.thrift.TFieldIdEnum;

/**
 * This class is used to store meta data about thrift fields. Every field in a
 * a struct should have a corresponding instance of this class describing it.
 *
 * The meta data is registered by ALL Thrift struct classes via a static {...}
 * initializer block in the generated Thrift code.
 *
 * Since different threads could be initializing different Thrift classes, calls
 * to the public static methods of this class could be racy.
 *
 * All methods of this class should be made thread safe.
 */
public class FieldMetaData implements java.io.Serializable {
  public final String fieldName;
  public final byte requirementType;
  public final FieldValueMetaData valueMetaData;
  private static final ConcurrentMap<Class<? extends TBase>, Map<? extends TFieldIdEnum, FieldMetaData>> structMap = new ConcurrentHashMap<>();

  public FieldMetaData(String name, byte req, FieldValueMetaData vMetaData){
    this.fieldName = name;
    this.requirementType = req;
    this.valueMetaData = vMetaData;
  }

  public static <T extends TBase<T, F>, F extends TFieldIdEnum> void addStructMetaDataMap(Class<T> sClass, Map<F, FieldMetaData> map) {
    structMap.put(sClass, map);
  }

  /**
   * Returns a map with metadata (i.e. instances of FieldMetaData) that
   * describe the fields of the given class.
   *
   * @param sClass The TBase class for which the metadata map is requested. It is not
   *               guaranteed that sClass will have been statically initialized before
   *               this method is called. A racy call to
   *               {@link FieldMetaData#addStructMetaDataMap(Class, Map)} from a different
   *               thread during static initialization of the Thrift class is possible.
   */
  public static <T extends TBase<T, F>, F extends TFieldIdEnum> Map<F, FieldMetaData> getStructMetaDataMap(
      Class<T> sClass) {
    // Note: Do not use synchronized on this method declaration - it leads to a deadlock.
    // Similarly, do not trigger sClass.newInstance() while holding a lock on structMap,
    // it will lead to the same deadlock.
    // See: https://issues.apache.org/jira/browse/THRIFT-5430 for details.
    if (!structMap.containsKey(sClass)) { // Load class if it hasn't been loaded
      try {
        sClass.getDeclaredConstructor().newInstance();
      } catch (InstantiationException e) {
        throw new RuntimeException("InstantiationException for TBase class: " + sClass.getName() + ", message: " + e.getMessage());
      } catch (IllegalAccessException e) {
        throw new RuntimeException("IllegalAccessException for TBase class: " + sClass.getName() + ", message: " + e.getMessage());
      } catch (ReflectiveOperationException e) {
        throw new RuntimeException("ReflectiveOperationException for TBase class: " + sClass.getName() + ", message: " + e.getMessage());
      }
    }
    //noinspection unchecked
    return (Map<F, FieldMetaData>) structMap.get(sClass);
  }
}
