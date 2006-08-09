package com.facebook.thrift.protocol;

/**
 * Type constants in the Thrift protocol.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public final class TType {
  public static final byte STOP   = 1;
  public static final byte BYTE   = 2;
  public static final byte U16    = 3;
  public static final byte I16    = 4;
  public static final byte U32    = 5;
  public static final byte I32    = 6;
  public static final byte U64    = 7;
  public static final byte I64    = 8;
  public static final byte STRING = 9;
  public static final byte STRUCT = 10;
  public static final byte MAP    = 11;
  public static final byte SET    = 12;
  public static final byte LIST   = 13;
}
