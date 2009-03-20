// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import org.apache.thrift.protocol.TProtocol;

/**
 * Generic base interface for generated Thrift objects.
 *
 */
public interface TBase extends Cloneable {

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
  public boolean isSet(int fieldId);

  /**
   * Get a field's value by id. Primitive types will be wrapped in the 
   * appropriate "boxed" types.
   *
   * @param fieldId The field's id tag as found in the IDL.
   */
  public Object getFieldValue(int fieldId);

  /**
   * Set a field's value by id. Primitive types must be "boxed" in the 
   * appropriate object wrapper type.
   *
   * @param fieldId The field's id tag as found in the IDL.
   */
  public void setFieldValue(int fieldId, Object value);
}
