// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolUtil;
import org.apache.thrift.protocol.TStruct;
import org.apache.thrift.protocol.TType;

/**
 * Application level exception
 *
 */
public class TApplicationException extends TException {

  private static final TStruct TAPPLICATION_EXCEPTION_STRUCT = new TStruct("TApplicationException");
  private static final TField MESSAGE_FIELD = new TField("message", TType.STRING, (short)1);
  private static final TField TYPE_FIELD = new TField("type", TType.I32, (short)2);

  private static final long serialVersionUID = 1L;

  public static final int UNKNOWN = 0;
  public static final int UNKNOWN_METHOD = 1;
  public static final int INVALID_MESSAGE_TYPE = 2;
  public static final int WRONG_METHOD_NAME = 3;
  public static final int BAD_SEQUENCE_ID = 4;
  public static final int MISSING_RESULT = 5;

  protected int type_ = UNKNOWN;

  public TApplicationException() {
    super();
  }

  public TApplicationException(int type) {
    super();
    type_ = type;
  }

  public TApplicationException(int type, String message) {
    super(message);
    type_ = type;
  }

  public TApplicationException(String message) {
    super(message);
  }

  public int getType() {
    return type_;
  }

  public static TApplicationException read(TProtocol iprot) throws TException {
    TField field;
    iprot.readStructBegin();

    String message = null;
    int type = UNKNOWN;

    while (true) {
      field = iprot.readFieldBegin();
      if (field.type == TType.STOP) {
        break;
      }
      switch (field.id) {
      case 1:
        if (field.type == TType.STRING) {
          message = iprot.readString();
        } else {
          TProtocolUtil.skip(iprot, field.type);
        }
        break;
      case 2:
        if (field.type == TType.I32) {
          type = iprot.readI32();
        } else {
          TProtocolUtil.skip(iprot, field.type);
        }
        break;
      default:
        TProtocolUtil.skip(iprot, field.type);
        break;
      }
      iprot.readFieldEnd();
    }
    iprot.readStructEnd();

    return new TApplicationException(type, message);
  }

  public void write(TProtocol oprot) throws TException {
    oprot.writeStructBegin(TAPPLICATION_EXCEPTION_STRUCT);
    if (getMessage() != null) {
      oprot.writeFieldBegin(MESSAGE_FIELD);
      oprot.writeString(getMessage());
      oprot.writeFieldEnd();
    }
    oprot.writeFieldBegin(TYPE_FIELD);
    oprot.writeI32(type_);
    oprot.writeFieldEnd();
    oprot.writeFieldStop();
    oprot.writeStructEnd();
  }
}
