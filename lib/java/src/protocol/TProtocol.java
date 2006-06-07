package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;
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

  public int writeStructBegin (TTransport out,
                               TStruct    struct)  throws TException;
  public int writeStructEnd   (TTransport out)     throws TException;

  public int writeFieldBegin  (TTransport out,
                               TField     field)   throws TException;

  public int writeFieldEnd    (TTransport out)     throws TException;

  public int writeFieldStop   (TTransport out)     throws TException;

  public int writeMapBegin    (TTransport out,
                               TMap       map)     throws TException;

  public int writeMapEnd      (TTransport out)     throws TException;

  public int writeListBegin   (TTransport out,
                               TList      list)    throws TException;

  public int writeListEnd     (TTransport out)     throws TException;

  public int writeSetBegin    (TTransport out,
                               TSet       set)     throws TException;

  public int writeSetEnd      (TTransport out)     throws TException;

  public int writeByte        (TTransport out,
                               UInt8      b)       throws TException;

  public int writeU32         (TTransport out,
                               UInt32     u32)     throws TException;

  public int writeI32         (TTransport out,
                               Int32      i32)     throws TException;

  public int writeU64         (TTransport out,
                               UInt64     u64)     throws TException;

  public int writeI64         (TTransport out,
                               Int64      i64)     throws TException;

  public int writeString      (TTransport out,
                               TString    str)     throws TException;

  /**
   * Reading methods.
   */

  public int readStructBegin  (TTransport in,
                               TStruct    struct)  throws TException;

  public int readStructEnd    (TTransport  in)     throws TException;

  public int readFieldBegin   (TTransport  in,
                               TField      field)  throws TException;
  
  public int readFieldEnd     (TTransport  in)     throws TException;
 
  public int readMapBegin     (TTransport  in,
                               TMap        map)    throws TException;

  public int readMapEnd       (TTransport  in)     throws TException;

  public int readListBegin    (TTransport  in,
                               TList       list)   throws TException;

  public int readListEnd      (TTransport  in)     throws TException;

  public int readSetBegin     (TTransport  in,
                               TSet        set)    throws TException;

  public int readSetEnd       (TTransport  in)     throws TException;

  public int readByte         (TTransport  in,
                               UInt8       b)      throws TException;

  public int readU32          (TTransport  in,
                               UInt32      u32)    throws TException;

  public int readI32          (TTransport  in,
                               Int32       i32)    throws TException;

  public int readU64          (TTransport  in,
                               UInt64      u64)    throws TException;
  
  public int readI64          (TTransport  in,
                               Int64       i64)    throws TException;

  public int readString       (TTransport  in,
                               TString     s)      throws TException;

}
