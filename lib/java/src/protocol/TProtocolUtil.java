// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

import com.facebook.thrift.TException;
import com.facebook.thrift.transport.TTransport;

/**
 * Utility class with static methods for interacting with protocol data
 * streams.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TProtocolUtil {
  public static void skip(TProtocol prot, byte type)
    throws TException {

    switch (type) {
    case TType.BOOL:
      {
        prot.readBool();
        break;
      }
    case TType.BYTE:
      {
        prot.readByte();
        break;
      }
    case TType.I16:
      {
        prot.readI16();
        break;
      }
    case TType.I32:
      {
        prot.readI32();
        break;
      }
    case TType.I64:
      {
        prot.readI64();
        break;
      }
    case TType.DOUBLE:
      {
        prot.readDouble();
        break;
      }
    case TType.STRING:
      {
        prot.readString();
        break;
      }
    case TType.STRUCT:
      {
        prot.readStructBegin();
        while (true) {
          TField field = prot.readFieldBegin();
          if (field.type == TType.STOP) {
            break;
          }
          skip(prot, field.type);
          prot.readFieldEnd();
        }
        prot.readStructEnd();
        break;
      }
    case TType.MAP:
      {
        TMap map = prot.readMapBegin();
        for (int i = 0; i < map.size; i++) {
          skip(prot, map.keyType);
          skip(prot, map.valueType);
        }
        prot.readMapEnd();
        break;
      }
    case TType.SET:
      {        
        TSet set = prot.readSetBegin();
        for (int i = 0; i < set.size; i++) {
          skip(prot, set.elemType);
        }
        prot.readSetEnd();
        break;
      }
    case TType.LIST:
      {
        TList list = prot.readListBegin();
        for (int i = 0; i < list.size; i++) {
          skip(prot, list.elemType);
        }
        prot.readListEnd();
        break;
      }
    default:
      break;
    }
  }
}
