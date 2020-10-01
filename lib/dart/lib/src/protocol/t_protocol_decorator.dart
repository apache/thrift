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

/// Forward all operations to the wrapped protocol.  Used as a base class.
///
/// Adapted from the C# version.
class TProtocolDecorator extends TProtocol {
  final TProtocol _protocol;

  TProtocolDecorator(TProtocol protocol)
      : _protocol = protocol,
        super(protocol.transport);

  /// Write

  @override
  void writeMessageBegin(TMessage message) {
    _protocol.writeMessageBegin(message);
  }

  @override
  void writeMessageEnd() {
    _protocol.writeMessageEnd();
  }

  @override
  void writeStructBegin(TStruct struct) {
    _protocol.writeStructBegin(struct);
  }

  @override
  void writeStructEnd() {
    _protocol.writeStructEnd();
  }

  @override
  void writeFieldBegin(TField field) {
    _protocol.writeFieldBegin(field);
  }

  @override
  void writeFieldEnd() {
    _protocol.writeFieldEnd();
  }

  @override
  void writeFieldStop() {
    _protocol.writeFieldStop();
  }

  @override
  void writeMapBegin(TMap map) {
    _protocol.writeMapBegin(map);
  }

  @override
  void writeMapEnd() {
    _protocol.writeMapEnd();
  }

  @override
  void writeListBegin(TList list) {
    _protocol.writeListBegin(list);
  }

  @override
  void writeListEnd() {
    _protocol.writeListEnd();
  }

  @override
  void writeSetBegin(TSet set) {
    _protocol.writeSetBegin(set);
  }

  @override
  void writeSetEnd() {
    _protocol.writeSetEnd();
  }

  @override
  void writeBool(bool b) {
    _protocol.writeBool(b);
  }

  @override
  void writeByte(int b) {
    _protocol.writeByte(b);
  }

  @override
  void writeI16(int i16) {
    _protocol.writeI16(i16);
  }

  @override
  void writeI32(int i32) {
    _protocol.writeI32(i32);
  }

  @override
  void writeI64(int i64) {
    _protocol.writeI64(i64);
  }

  @override
  void writeDouble(double d) {
    _protocol.writeDouble(d);
  }

  @override
  void writeString(String str) {
    _protocol.writeString(str);
  }

  @override
  void writeBinary(Uint8List bytes) {
    _protocol.writeBinary(bytes);
  }

  /// Read
  @override
  TMessage readMessageBegin() => _protocol.readMessageBegin();
  @override
  void readMessageEnd() => _protocol.readMessageEnd();

  @override
  TStruct readStructBegin() => _protocol.readStructBegin();
  @override
  void readStructEnd() => _protocol.readStructEnd();

  @override
  TField readFieldBegin() => _protocol.readFieldBegin();
  @override
  void readFieldEnd() => _protocol.readFieldEnd();

  @override
  TMap readMapBegin() => _protocol.readMapBegin();
  @override
  void readMapEnd() => _protocol.readMapEnd();

  @override
  TList readListBegin() => _protocol.readListBegin();
  @override
  void readListEnd() => _protocol.readListEnd();

  @override
  TSet readSetBegin() => _protocol.readSetBegin();
  @override
  void readSetEnd() => _protocol.readSetEnd();

  @override
  bool readBool() => _protocol.readBool();

  @override
  int readByte() => _protocol.readByte();

  @override
  int readI16() => _protocol.readI16();

  @override
  int readI32() => _protocol.readI32();

  @override
  int readI64() => _protocol.readI64();

  @override
  double readDouble() => _protocol.readDouble();

  @override
  String readString() => _protocol.readString();

  @override
  Uint8List readBinary() => _protocol.readBinary();
}
