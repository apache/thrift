/// Licensed to the Apache Software Foundation (ASF) under one
/// or more contributor license agreements. See the NOTICE file
/// distributed with this work for additional information
/// regarding copyright ownership. The ASF licenses this file
/// to you under the Apache License, Version 2.0 (the
/// "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at
///
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// Unless required by applicable law or agreed to in writing,
/// software distributed under the License is distributed on an
/// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
/// KIND, either express or implied. See the License for the
/// specific language governing permissions and limitations
/// under the License.

part of thrift;

/// Protocol interface definition
abstract class TProtocol {

  final TTransport transport;

  TProtocol(this.transport);

  /// Write
  writeMessageBegin(TMessage message);
  writeMessageEnd();

  writeStructBegin(TStruct struct);
  writeStructEnd();

  writeFieldBegin(TField field);
  writeFieldEnd();
  writeFieldStop();

  writeMapBegin(TMap map);
  writeMapEnd();

  writeListBegin(TList list);
  writeListEnd();

  writeSetBegin(TSet set);
  writeSetEnd();

  writeBool(bool b);

  writeByte(int b);

  writeI16(int i16);

  writeI32(int i32);

  writeI64(num i64);

  writeDouble(num dub);

  writeString(String str);

  writeBinary(ByteBuffer bin);

  /// Read
  TMessage readMessageBegin();
  readMessageEnd();

  TStruct readStructBegin();
  readStructEnd();

  TField readFieldBegin();
  readFieldEnd();

  TMap readMapBegin();
  readMapEnd();

  TList readListBegin();
  readListEnd();

  TSet readSetBegin();
  readSetEnd();

  bool readBool();

  int readByte();

  int readI16();

  int readI32();

  num readI64();

  num readDouble();

  String readString();

  ByteBuffer readBinary();
}
