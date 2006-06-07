package com.facebook.thrift.protocol;

import com.facebook.thrift.types.UInt8;

/**
 * Type constants in the Thrift protocol.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public enum TType {
  STOP   (1),
  BYTE   (2),
  U16    (3),
  I16    (4),
  U32    (5),
  I32    (6),
  U64    (7),
  I64    (8),
  STRING (9),
  STRUCT (10),
  MAP    (11),
  SET    (12),
  LIST   (13);

  /** U8 identifier */
  private UInt8 code_;

  /**
   * Constructor to create a TType object from its code.
   *
   * @param code The identifier code for this type
   */
  private TType(int code) {
    code_ = new UInt8((byte)code);
  }

  /**
   * Accessor for the code.
   */
  public UInt8 getCode() {
    return code_;
  }

  /**
   * Static method to get a type object from a byte.
   *
   * @param code The type code
   */
  public static TType getType(UInt8 code) {
    switch (code.get()) {
    case 1:
      return STOP;
    case 2:
      return BYTE;
    case 3:
      return U16;
    case 4:
      return I16;
    case 5:
      return U32;
    case 6:
      return I32;
    case 7:
      return U64;
    case 8:
      return I64;
    case 9:
      return STRING;
    case 10:
      return STRUCT;
    case 11:
      return MAP;
    case 12:
      return SET;
    case 13:
      return LIST;
    default:
      System.err.println("WARNING: Unidentified type code: " + code.get());
      return STOP;
    }
  }
}
