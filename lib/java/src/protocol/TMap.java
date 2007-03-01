// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

/**
 * Helper class that encapsulates map metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TMap {
  public TMap() {}
  
  public TMap(byte k, byte v, int s) {
    keyType = k;
    valueType = v;
    size = s;
  }

  public byte  keyType   = TType.STOP;
  public byte  valueType = TType.STOP;
  public int   size      = 0;
}
