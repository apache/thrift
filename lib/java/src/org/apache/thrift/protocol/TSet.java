// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Helper class that encapsulates set metadata.
 *
 */
public final class TSet {
  public TSet() {
    this(TType.STOP, 0);
  }

  public TSet(byte t, int s) {
    elemType = t;
    size = s;
  }

  public TSet(TList list) {
    this(list.elemType, list.size);
  }

  public final byte elemType;
  public final int  size;
}
