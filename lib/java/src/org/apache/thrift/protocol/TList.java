// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Helper class that encapsulates list metadata.
 *
 */
public final class TList {
  public TList() {
    this(TType.STOP, 0);
  }

  public TList(byte t, int s) {
    elemType = t;
    size = s;
  }

  public final byte elemType;
  public final int  size;
}
