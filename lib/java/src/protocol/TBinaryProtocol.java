package com.facebook.thrift.protocol;

import com.facebook.thrift.TException;
import com.facebook.thrift.transport.TTransport;
import com.facebook.thrift.types.*;

/**
 * Binary protocol implementation for thrift.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TBinaryProtocol implements TProtocol {
  public int writeStructBegin (TTransport out,
                               TStruct    struct)  throws TException {
    return 0;
  }

  public int writeStructEnd   (TTransport out)     throws TException {
    return 0;
  }

  public int writeFieldBegin  (TTransport out,
                               TField     field)   throws TException {
    return
      writeByte(out, field.type.getCode()) +
      writeU32(out, field.id);
  }

  public int writeFieldEnd    (TTransport out)     throws TException {
    return 0;
  }

  public int writeFieldStop   (TTransport out)     throws TException {
    return
      writeByte(out, TType.STOP.getCode());
  }

  public int writeMapBegin    (TTransport out,
                               TMap       map)     throws TException {
    return
      writeByte(out, map.keyType.getCode()) +
      writeByte(out, map.valueType.getCode()) +
      writeU32(out, map.size);
  }

  public int writeMapEnd      (TTransport out)     throws TException {
    return 0;
  }

  public int writeListBegin   (TTransport out,
                               TList      list)    throws TException {
    return
      writeByte(out, list.elemType.getCode()) +
      writeU32(out, list.size);
  }

  public int writeListEnd     (TTransport out)     throws TException {
    return 0;
  }

  public int writeSetBegin    (TTransport out,
                               TSet       set)     throws TException {
    return
      writeByte(out, set.elemType.getCode()) +
      writeU32(out, set.size);
  }

  public int writeSetEnd      (TTransport out)     throws TException {
    return 0;
  }

  public int writeByte        (TTransport out,
                               UInt8      b)       throws TException {
    out.write(b.data(), 0, 1);
    return 1;
  }

  public int writeU32         (TTransport out,
                               UInt32     u32)     throws TException {
    out.write(u32.data(), 0, 4);
    return 4;
  }

  public int writeI32         (TTransport out,
                               Int32      i32)     throws TException {
    out.write(i32.data(), 0, 4);
    return 4;
  }

  public int writeU64         (TTransport out,
                               UInt64     u64)     throws TException {
    out.write(u64.data(), 0, 8);
    return 8;
  }

  public int writeI64         (TTransport out,
                               Int64      i64)     throws TException {
    out.write(i64.data(), 0, 8);
    return 8;
  }

  public int writeString      (TTransport out,
                               TString    str)     throws TException {
    byte[] dat = str.value.getBytes();
    int sent = writeU32(out, new UInt32(dat.length));
    out.write(dat, 0, dat.length);
    return sent + dat.length;
  }

  /**
   * Reading methods.
   */

  public int readStructBegin  (TTransport in,
                               TStruct    struct)  throws TException {
    struct.name = "";
    return 0;
  }

  public int readStructEnd    (TTransport  in)     throws TException {
    return 0;
  }

  public int readFieldBegin   (TTransport  in,
                               TField      field)  throws TException {
    int recv = 0;
    UInt8 t = new UInt8();

    recv += readByte(in, t);
    field.type = TType.getType(t);
    if (field.type.equals(TType.STOP)) {
      field.id = new UInt32(0);
      return recv;
    }
    recv += readU32(in, field.id);
    return recv;
  }
  
  public int readFieldEnd     (TTransport  in)     throws TException {
    return 0;
  }
 
  public int readMapBegin     (TTransport  in,
                               TMap        map)    throws TException {
    int recv = 0;
    UInt8 t = new UInt8();
    recv += readByte(in, t);
    map.keyType = TType.getType(t);
    recv += readByte(in, t);
    map.valueType = TType.getType(t);
    recv += readU32(in, map.size);
    return recv;
  }

  public int readMapEnd       (TTransport  in)     throws TException {
    return 0;
  }

  public int readListBegin    (TTransport  in,
                               TList       list)   throws TException {
    int recv = 0;
    UInt8 t = new UInt8();
    recv += readByte(in, t);
    list.elemType = TType.getType(t);
    recv += readU32(in, list.size);
    return recv;
  }

  public int readListEnd      (TTransport  in)     throws TException {
    return 0;
  }

  public int readSetBegin     (TTransport  in,
                               TSet        set)    throws TException {
    int recv = 0;
    UInt8 t = new UInt8();
    recv += readByte(in, t);
    set.elemType = TType.getType(t);
    recv += readU32(in, set.size);
    return recv;
  }

  public int readSetEnd       (TTransport  in)     throws TException {
    return 0;
  }

  public int readByte         (TTransport  in,
                               UInt8       b)      throws TException {
    byte[] buf = new byte[1];
    in.readAll(buf, 0, 1);
    b.read(buf, 0);
    return 1;
  }

  public int readU32          (TTransport  in,
                               UInt32      u32)    throws TException {
    byte[] buf = new byte[4];
    in.readAll(buf, 0, 4);
    u32.read(buf, 0);
    return 4;
  }

  public int readI32          (TTransport  in,
                               Int32       i32)    throws TException {
    byte[] buf = new byte[4];
    in.readAll(buf, 0, 4);
    i32.read(buf, 0);
    return 4;
  }

  public int readU64          (TTransport  in,
                               UInt64      u64)    throws TException {
    byte[] buf = new byte[8];
    in.readAll(buf, 0, 8);
    u64.read(buf, 0);
    return 8;
  }
  
  public int readI64          (TTransport  in,
                               Int64       i64)    throws TException {
    byte[] buf = new byte[8];
    in.readAll(buf, 0, 8);
    i64.read(buf, 0);
    return 8;
  }

  public int readString       (TTransport  in,
                               TString     s)      throws TException {
    UInt32 size = new UInt32();
    int recv = readU32(in, size);
    byte[] buf = new byte[size.toInt()];
    in.readAll(buf, 0, size.toInt());
    s.value = new String(buf);
    return recv + size.toInt();
  }
}
