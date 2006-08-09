package com.facebook.thrift.protocol;

import com.facebook.thrift.TException;
import com.facebook.thrift.transport.TTransport;

/**
 * Protocol interface definition.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public interface TProtocol {

  /**
   * Writing methods.
   */

  public void writeStructBegin (TTransport out,
                                TStruct    struct)  throws TException;

  public void writeStructEnd   (TTransport out)     throws TException;

  public void writeFieldBegin  (TTransport out,
                                TField     field)   throws TException;

  public void writeFieldEnd    (TTransport out)     throws TException;

  public void writeFieldStop   (TTransport out)     throws TException;

  public void writeMapBegin    (TTransport out,
                                TMap       map)     throws TException;

  public void writeMapEnd      (TTransport out)     throws TException;

  public void writeListBegin   (TTransport out,
                                TList      list)    throws TException;

  public void writeListEnd     (TTransport out)     throws TException;

  public void writeSetBegin    (TTransport out,
                                TSet       set)     throws TException;

  public void writeSetEnd      (TTransport out)     throws TException;

  public void writeByte        (TTransport out,
                                byte       b)       throws TException;

  public void writeU32         (TTransport out,
                                int        u32)     throws TException;

  public void writeI32         (TTransport out,
                                int        i32)     throws TException;

  public void writeU64         (TTransport out,
                                long       u64)     throws TException;

  public void writeI64         (TTransport out,
                                long       i64)     throws TException;

  public void writeString      (TTransport out,
                                String     str)     throws TException;

  /**
   * Reading methods.
   */

  public TStruct readStructBegin  (TTransport in)  throws TException;

  public void    readStructEnd    (TTransport in)  throws TException;

  public TField  readFieldBegin   (TTransport in)  throws TException;
  
  public void    readFieldEnd     (TTransport in)  throws TException;
 
  public TMap    readMapBegin     (TTransport in)  throws TException;

  public void    readMapEnd       (TTransport in)  throws TException;

  public TList   readListBegin    (TTransport in)  throws TException;

  public void    readListEnd      (TTransport in)  throws TException;

  public TSet    readSetBegin     (TTransport in)  throws TException;

  public void    readSetEnd       (TTransport in)  throws TException;

  public byte    readByte         (TTransport in)  throws TException;

  public int     readU32          (TTransport in)  throws TException;

  public int     readI32          (TTransport in)  throws TException;

  public long    readU64          (TTransport in)  throws TException;
  
  public long    readI64          (TTransport in)  throws TException;

  public String  readString       (TTransport in)  throws TException;

}
