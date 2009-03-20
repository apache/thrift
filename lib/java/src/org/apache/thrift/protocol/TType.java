// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Type constants in the Thrift protocol.
 *
 */
public final class TType {
  public static final byte STOP   = 0;
  public static final byte VOID   = 1;
  public static final byte BOOL   = 2;
  public static final byte BYTE   = 3;
  public static final byte DOUBLE = 4;
  public static final byte I16    = 6;
  public static final byte I32    = 8;
  public static final byte I64    = 10;
  public static final byte STRING = 11;
  public static final byte STRUCT = 12;
  public static final byte MAP    = 13;
  public static final byte SET    = 14;
  public static final byte LIST   = 15;
}
