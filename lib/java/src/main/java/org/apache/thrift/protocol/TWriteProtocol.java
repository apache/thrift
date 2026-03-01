package org.apache.thrift.protocol;

import java.nio.ByteBuffer;
import java.util.UUID;
import org.apache.thrift.TException;

public interface TWriteProtocol {

  void writeMessageBegin(TMessage message) throws TException;

  void writeMessageEnd() throws TException;

  void writeStructBegin(TStruct struct) throws TException;

  void writeStructEnd() throws TException;

  void writeFieldBegin(TField field) throws TException;

  void writeFieldEnd() throws TException;

  void writeFieldStop() throws TException;

  void writeMapBegin(TMap map) throws TException;

  void writeMapEnd() throws TException;

  void writeListBegin(TList list) throws TException;

  void writeListEnd() throws TException;

  void writeSetBegin(TSet set) throws TException;

  void writeSetEnd() throws TException;

  void writeBool(boolean b) throws TException;

  void writeByte(byte b) throws TException;

  void writeI16(short i16) throws TException;

  void writeI32(int i32) throws TException;

  void writeI64(long i64) throws TException;

  void writeUuid(UUID uuid) throws TException;

  void writeDouble(double dub) throws TException;

  void writeString(String str) throws TException;

  void writeBinary(ByteBuffer buf) throws TException;
}
