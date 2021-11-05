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

import java.io.Serializable;

import org.apache.thrift.protocol.TProtocol;

/**
 * Generic base interface for generated Thrift objects.
 *
 */
public interface TBase<T extends TBase<T,F>, F extends TFieldIdEnum> extends Comparable<T>,  TSerializable, Serializable {

  /**
   * Get the F instance that corresponds to fieldId.
   *
   * @param fieldId the ID of the requested field.
   * @return F instance that corresponds to fieldId.
   */
  public F fieldForId(int fieldId);

  /**
   * Check if a field is currently set or unset.
   *
   * @param field the field to check.
   * @return true if the field is set, false otherwise.
   */
  public boolean isSet(F field);

  /**
   * Get a field's value by field variable. Primitive types will be wrapped in
   * the appropriate "boxed" types.
   *
   * @param field the field whose value is requested.
   * @return the value of the requested field.
   */
  public Object getFieldValue(F field);

  /**
   * Set a field's value by field variable. Primitive types must be "boxed" in
   * the appropriate object wrapper type.
   *
   * @param field the field whose value is to be set.
   * @param value the value to be assigned to field.
   */
  public void setFieldValue(F field, Object value);

  /**
   * Performs a deep copy of this instance and returns the copy.
   *
   * @return a deep copy of this instance.
   */
  public T deepCopy();

  /**
   * Return to the state of having just been initialized, as though you had just
   * called the default constructor.
   */
  public void clear();
}
