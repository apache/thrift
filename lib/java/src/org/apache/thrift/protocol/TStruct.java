// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Helper class that encapsulates struct metadata.
 *
 */
public final class TStruct {
  public TStruct() {
    this("");
  }

  public TStruct(String n) {
    name = n;
  }

  public final String name;
}
