// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

/**
 * Helper class that encapsulates field metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TField {
  public TField() {}

  public TField(String n, byte t, short i) {
    name = n;
    type = t;
    id = i;
  }

  public String name = "";
  public byte   type = TType.STOP;
  public short  id   = 0;
}
