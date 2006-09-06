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
  public static void skip(TProtocol prot, TTransport in, byte type)
    throws TException {

    switch (type) {
    case TType.BOOL:
      {
        prot.readBool(in);
      }
    case TType.BYTE:
      {
        prot.readByte(in);
      }
    case TType.I16:
      {
        prot.readI16(in);
      }
    case TType.I32:
      {
        prot.readI32(in);
      }
    case TType.I64:
      {
        prot.readI64(in);
      }
    case TType.DOUBLE:
      {
        prot.readDouble(in);
      }
    case TType.STRING:
      {
        prot.readString(in);
      }
    case TType.STRUCT:
      {
        prot.readStructBegin(in);
        while (true) {
          TField field = prot.readFieldBegin(in);
          if (field.type == TType.STOP) {
            break;
          }
          skip(prot, in, field.type);
          prot.readFieldEnd(in);
        }
        prot.readStructEnd(in);
      }
    case TType.MAP:
      {
        TMap map = prot.readMapBegin(in);
        for (int i = 0; i < map.size; i++) {
          skip(prot, in, map.keyType);
          skip(prot, in, map.valueType);
        }
        prot.readMapEnd(in);
      }
    case TType.SET:
      {        
        TSet set = prot.readSetBegin(in);
        for (int i = 0; i < set.size; i++) {
          skip(prot, in, set.elemType);
        }
        prot.readSetEnd(in);
      }
    case TType.LIST:
      {
        TList list = prot.readListBegin(in);
        for (int i = 0; i < list.size; i++) {
          skip(prot, in, list.elemType);
        }
        prot.readListEnd(in);
      }
    default:
      return;
    }
  }
}
