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
      }
    case TType.BYTE:
      {
        prot.readByte();
      }
    case TType.I16:
      {
        prot.readI16();
      }
    case TType.I32:
      {
        prot.readI32();
      }
    case TType.I64:
      {
        prot.readI64();
      }
    case TType.DOUBLE:
      {
        prot.readDouble();
      }
    case TType.STRING:
      {
        prot.readString();
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
      }
    case TType.MAP:
      {
        TMap map = prot.readMapBegin();
        for (int i = 0; i < map.size; i++) {
          skip(prot, map.keyType);
          skip(prot, map.valueType);
        }
        prot.readMapEnd();
      }
    case TType.SET:
      {        
        TSet set = prot.readSetBegin();
        for (int i = 0; i < set.size; i++) {
          skip(prot, set.elemType);
        }
        prot.readSetEnd();
      }
    case TType.LIST:
      {
        TList list = prot.readListBegin();
        for (int i = 0; i < list.size; i++) {
          skip(prot, list.elemType);
        }
        prot.readListEnd();
      }
    default:
      return;
    }
  }
}
