// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Helper class that encapsulates map metadata.
 *
 */
public final class TMap {
  public TMap() {
    this(TType.STOP, TType.STOP, 0);
  }

  public TMap(byte k, byte v, int s) {
    keyType = k;
    valueType = v;
    size = s;
  }

  public final byte  keyType;
  public final byte  valueType;
  public final int   size;
}
