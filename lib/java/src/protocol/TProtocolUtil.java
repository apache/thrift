package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;
import com.facebook.thrift.TException;
import com.facebook.thrift.transport.TTransport;

/**
 * Utility class with static methods for interacting with protocol data
 * streams.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TProtocolUtil {
  public static int skip(TProtocol prot, TTransport in, TType type)
    throws TException {

    switch (type) {
    case BYTE:
      {
        UInt8 b = new UInt8();
        return prot.readByte(in, b);
      }
    case U32:
      {
        UInt32 u32 = new UInt32();
        return prot.readU32(in, u32);
      }
    case I32:
      {
        Int32 i32 = new Int32();
        return prot.readI32(in, i32);
      }
    case U64:
      {
        UInt64 u64 = new UInt64();
        return prot.readU64(in, u64);
      }
    case I64:
      {
        Int64 i64 = new Int64();
        return prot.readI64(in, i64);
      }
    case STRING:
      {
        TString s = new TString();
        return prot.readString(in, s);
      }
    case STRUCT:
      {
        int result = 0;
        TString name = new TString();
        TStruct struct = new TStruct();
        TField field = new TField();
        result += prot.readStructBegin(in, struct);
        while (true) {
          result += prot.readFieldBegin(in, field);
          if (field.type.equals(TType.STOP)) {
            break;
          }
          result += skip(prot, in, field.type);
          result += prot.readFieldEnd(in);
        }
        result += prot.readStructEnd(in);
        return result;
      }
    case MAP:
      {
        int result = 0;
        TMap map = new TMap();
        result += prot.readMapBegin(in, map);
        for (int i = 0; i < map.size.get(); i++) {
          result += skip(prot, in, map.keyType);
          result += skip(prot, in, map.valueType);
        }
        result += prot.readMapEnd(in);
        return result;
      }
    case SET:
      {
        int result = 0;
        TSet set = new TSet();
        result += prot.readSetBegin(in, set);
        for (int i = 0; i < set.size.get(); i++) {
          result += skip(prot, in, set.elemType);
        }
        result += prot.readSetEnd(in);
        return result;
      }
    case LIST:
      {
        int result = 0;
        TList list = new TList();
        result += prot.readListBegin(in, list);
        for (int i = 0; i < list.size.get(); i++) {
          result += skip(prot, in, list.elemType);
        }
        result += prot.readListEnd(in);
        return result;
      }
    default:
      return 0;
    }
  }
}
