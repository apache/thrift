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

import org.apache.thrift.TEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * Provides a memoized way to lookup an enum by its value.
 *
 * This class is used internally by {@code TDeserializer}.
 * It is not intended to be used separately on its own.
 */
public class EnumCache {
  private static Logger LOG = LoggerFactory.getLogger(EnumCache.class);

  private Map<Class<? extends TEnum>, Map<Integer, TEnum>> classMap;

  public EnumCache() {
    this.classMap = new HashMap<>();
  }

  /**
   * Gets an instance of the enum type {@code enumClass}
   * corresponding to the given {@code value}.
   *
   * @param enumClass class of the enum to be returned.
   * @param value value returned by {@code getValue()}.
   */
  public TEnum get(Class<? extends TEnum> enumClass, int value) {
    Validate.checkNotNull(enumClass, "enumClass");

    Map<Integer, TEnum> valueMap = classMap.get(enumClass);
    if (valueMap == null) {
      valueMap = addClass(enumClass);
      if (valueMap == null) {
        return null;
      }
    }

    return valueMap.get(value);
  }

  private Map<Integer, TEnum> addClass(Class<? extends TEnum> enumClass) {
    try {
      Method valuesMethod = enumClass.getMethod("values");
      TEnum[] enumValues = (TEnum[]) valuesMethod.invoke(null);
      Map<Integer, TEnum> valueMap = new HashMap<>();

      for (TEnum enumValue : enumValues) {
        valueMap.put(enumValue.getValue(), enumValue);
      }

      classMap.put(enumClass, valueMap);
      return valueMap;
    } catch (NoSuchMethodException e) {
      LOG.error("enum class does not have values() method", e);
      return null;
    } catch (IllegalAccessException e) {
      LOG.error("Enum.values() method should be public!", e);
      return null;
    } catch (InvocationTargetException e) {
      LOG.error("Enum.values() threw exception", e);
      return null;
    }
  }
}
