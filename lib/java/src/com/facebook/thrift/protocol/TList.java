// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

/**
 * Helper class that encapsulates list metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TList {
  public TList() {}

  public TList(byte t, int s) {
    elemType = t;
    size = s;
  }

  public byte elemType = TType.STOP;
  public int  size     = 0;
}
