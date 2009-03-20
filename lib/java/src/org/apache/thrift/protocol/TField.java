// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Helper class that encapsulates field metadata.
 *
 */
public class TField {
  public TField() {
    this("", TType.STOP, (short)0);
  }

  public TField(String n, byte t, short i) {
    name = n;
    type = t;
    id = i;
  }

  public final String name;
  public final byte   type;
  public final short  id;

  public String toString() {
    return "<TField name:'" + name + "' type:" + type + " field-id:" + id + ">";
  }

  public boolean equals(TField otherField) {
    return type == otherField.type && id == otherField.id;
  }
}
