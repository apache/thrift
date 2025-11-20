package org.apache.thrift.protocol;

import java.nio.ByteBuffer;
import java.util.UUID;
import org.apache.thrift.TException;

public interface TReadProtocol {

  TMessage readMessageBegin() throws TException;

  void readMessageEnd() throws TException;

  TStruct readStructBegin() throws TException;

  void readStructEnd() throws TException;

  TField readFieldBegin() throws TException;

  void readFieldEnd() throws TException;

  TMap readMapBegin() throws TException;

  void readMapEnd() throws TException;

  TList readListBegin() throws TException;

  void readListEnd() throws TException;

  TSet readSetBegin() throws TException;

  void readSetEnd() throws TException;

  boolean readBool() throws TException;

  byte readByte() throws TException;

  short readI16() throws TException;

  int readI32() throws TException;

  long readI64() throws TException;

  UUID readUuid() throws TException;

  double readDouble() throws TException;

  String readString() throws TException;

  ByteBuffer readBinary() throws TException;
}
