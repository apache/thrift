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
public interface TBase<F extends TFieldIdEnum> extends Serializable {

  /**
   * Reads the TObject from the given input protocol.
   *
   * @param iprot Input protocol
   */
  public void read(TProtocol iprot) throws TException;

  /**
   * Writes the objects out to the protocol
   *
   * @param oprot Output protocol
   */
  public void write(TProtocol oprot) throws TException;

  /**
   * Check if a field is currently set or unset.
   *
   * @param fieldId The field's id tag as found in the IDL.
   */
  @Deprecated
  public boolean isSet(int fieldId);

  /**
   * Check if a field is currently set or unset.
   *
   * @param field
   */
  public boolean isSet(F field);

  /**
   * Get a field's value by id. Primitive types will be wrapped in the 
   * appropriate "boxed" types.
   *
   * @param fieldId The field's id tag as found in the IDL.
   */
  @Deprecated
  public Object getFieldValue(int fieldId);

  /**
   * Get a field's value by field variable. Primitive types will be wrapped in 
   * the appropriate "boxed" types.
   *
   * @param field
   */
  public Object getFieldValue(F field);

  /**
   * Set a field's value by id. Primitive types must be "boxed" in the 
   * appropriate object wrapper type.
   *
   * @param fieldId The field's id tag as found in the IDL.
   */
  @Deprecated
  public void setFieldValue(int fieldId, Object value);

  /**
   * Set a field's value by field variable. Primitive types must be "boxed" in
   * the appropriate object wrapper type.
   *
   * @param field
   */
  public void setFieldValue(F field, Object value);

  public TBase<F> deepCopy();
}
