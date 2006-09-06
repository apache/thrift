package com.facebook.thrift.protocol;

import com.facebook.thrift.TException;
import com.facebook.thrift.transport.TTransport;

/**
 * Binary protocol implementation for thrift.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TBinaryProtocol implements TProtocol {

  public void writeMessageBegin(TTransport out, TMessage message) throws TException {
    writeString(out, message.name);
    writeByte(out, message.type);
    writeI32(out, message.seqid);
  }

  public void writeMessageEnd(TTransport out) throws TException {}


  public void writeStructBegin(TTransport out, TStruct struct) throws TException {}

  public void writeStructEnd(TTransport out) throws TException {}

  public void writeFieldBegin(TTransport out, TField field) throws TException {
    writeByte(out, field.type);
    writeI16(out, field.id);
  }

  public void writeFieldEnd(TTransport out) throws TException {}

  public void writeFieldStop(TTransport out) throws TException {
    writeByte(out, TType.STOP);
  }

  public void writeMapBegin(TTransport out, TMap map) throws TException {
    writeByte(out, map.keyType);
    writeByte(out, map.valueType);
    writeI32(out, map.size);
  }

  public void writeMapEnd(TTransport out) throws TException {}

  public void writeListBegin(TTransport out, TList list) throws TException {
    writeByte(out, list.elemType);
    writeI32(out, list.size);
  }

  public void writeListEnd(TTransport out) throws TException {}

  public void writeSetBegin(TTransport out, TSet set) throws TException {
    writeByte(out, set.elemType);
    writeI32(out, set.size);
  }

  public void writeSetEnd(TTransport out) throws TException {}

  public void writeBool(TTransport out, boolean b) throws TException {
    writeByte(out, b ? (byte)1 : (byte)0);
  }

  byte[] bout = new byte[1];
  public void writeByte(TTransport out, byte b) throws TException {
    bout[0] = b;
    out.write(bout, 0, 1);
  }

  byte[] i16out = new byte[2];
  public void writeI16(TTransport out, short i16) throws TException {
    i16out[0] = (byte)(0xff & (i16 >> 8));
    i16out[1] = (byte)(0xff & (i16));
    out.write(i16out, 0, 2);
  }

  byte[] i32out = new byte[4];
  public void writeI32(TTransport out, int i32) throws TException {
    i32out[0] = (byte)(0xff & (i32 >> 24));
    i32out[1] = (byte)(0xff & (i32 >> 16));
    i32out[2] = (byte)(0xff & (i32 >> 8));
    i32out[3] = (byte)(0xff & (i32));
    out.write(i32out, 0, 4);
  }

  byte[] i64out = new byte[8];
  public void writeI64(TTransport out, long i64) throws TException {
    i64out[0] = (byte)(0xff & (i64 >> 56));
    i64out[1] = (byte)(0xff & (i64 >> 48));
    i64out[2] = (byte)(0xff & (i64 >> 40));
    i64out[3] = (byte)(0xff & (i64 >> 32));
    i64out[4] = (byte)(0xff & (i64 >> 24));
    i64out[5] = (byte)(0xff & (i64 >> 16));
    i64out[6] = (byte)(0xff & (i64 >> 8));
    i64out[7] = (byte)(0xff & (i64));
    out.write(i64out, 0, 8);
  }

  public void writeDouble(TTransport out, double dub) throws TException {
    writeI64(out, Double.doubleToLongBits(dub));
  }

  public void writeString(TTransport out, String str) throws TException {
    byte[] dat = str.getBytes();
    writeI32(out, dat.length);
    out.write(dat, 0, dat.length);
  }

  /**
   * Reading methods.
   */

  public TMessage readMessageBegin(TTransport in) throws TException {
    TMessage message = new TMessage();
    message.name = readString(in);
    message.type = readByte(in);
    message.seqid = readI32(in);
    return message;
  }

  public void readMessageEnd(TTransport in) throws TException {}

  public TStruct readStructBegin(TTransport in) throws TException {
    return new TStruct();
  }

  public void readStructEnd(TTransport in) throws TException {}

  public TField readFieldBegin(TTransport in) throws TException {
    TField field = new TField();
    field.type = readByte(in);
    if (field.type != TType.STOP) {
      field.id = readI16(in);
    }
    return field;
  }
  
  public void readFieldEnd(TTransport in) throws TException {}
 
  public TMap readMapBegin(TTransport in) throws TException {
    TMap map = new TMap();
    map.keyType = readByte(in);
    map.valueType = readByte(in);
    map.size = readI32(in);
    return map;
  }

  public void readMapEnd(TTransport in) throws TException {}

  public TList readListBegin(TTransport in) throws TException {
    TList list = new TList();
    list.elemType = readByte(in);
    list.size = readI32(in);
    return list;
  }

  public void readListEnd(TTransport in) throws TException {}

  public TSet readSetBegin(TTransport in) throws TException {
    TSet set = new TSet();
    set.elemType = readByte(in);
    set.size = readI32(in);
    return set;
  }

  public void readSetEnd(TTransport in) throws TException {}

  public boolean readBool(TTransport in) throws TException {
    return (readByte(in) == 1);
  }

  byte[] bin = new byte[1];
  public byte readByte(TTransport in) throws TException {
    in.readAll(bin, 0, 1);
    return bin[0];
  }

  byte[] i16rd = new byte[2];
  public short readI16(TTransport in) throws TException {
    in.readAll(i16rd, 0, 2);
    return
      (short)
      (((i16rd[0] & 0xff) << 8) |
       ((i16rd[1] & 0xff)));
  }

  byte[] i32rd = new byte[4];
  public int readI32(TTransport in) throws TException {
    in.readAll(i32rd, 0, 4);
    return
      ((i32rd[0] & 0xff) << 24) |
      ((i32rd[1] & 0xff) << 16) |
      ((i32rd[2] & 0xff) <<  8) |
      ((i32rd[3] & 0xff));
  }
 
  byte[] i64rd = new byte[8];
  public long readI64(TTransport in) throws TException {
    in.readAll(i64rd, 0, 8);
    return
      ((long)(i64rd[0] & 0xff) << 56) |
      ((long)(i64rd[1] & 0xff) << 48) |
      ((long)(i64rd[2] & 0xff) << 40) |
      ((long)(i64rd[3] & 0xff) << 32) |
      ((long)(i64rd[4] & 0xff) << 24) |
      ((long)(i64rd[5] & 0xff) << 16) |
      ((long)(i64rd[6] & 0xff) <<  8) |
      ((long)(i64rd[7] & 0xff));
  }

  public double readDouble(TTransport in) throws TException {
    return Double.longBitsToDouble(readI64(in));
  }

  public String readString(TTransport in)  throws TException {
    int size = readI32(in);
    byte[] buf = new byte[size];
    in.readAll(buf, 0, size);
    return new String(buf);
  }
}
