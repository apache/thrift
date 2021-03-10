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

class TBinaryProtocolFactory implements TProtocolFactory<TBinaryProtocol> {
  TBinaryProtocolFactory({this.strictRead = false, this.strictWrite = true});

  final bool strictRead;
  final bool strictWrite;

  @override
  TBinaryProtocol getProtocol(TTransport transport) {
    return TBinaryProtocol(transport,
        strictRead: strictRead, strictWrite: strictWrite);
  }
}

/// Binary protocol implementation for Thrift.
///
/// Adapted from the C# version.
class TBinaryProtocol extends TProtocol {
  static const int VERSION_MASK = 0xffff0000;
  static const int VERSION_1 = 0x80010000;

  static const Utf8Codec _utf8Codec = Utf8Codec();

  final bool strictRead;
  final bool strictWrite;

  TBinaryProtocol(TTransport transport,
      {this.strictRead = false, this.strictWrite = true})
      : super(transport);

  /// write
  @override
  void writeMessageBegin(TMessage message) {
    if (strictWrite) {
      int version = VERSION_1 | message.type;
      writeI32(version);
      writeString(message.name);
      writeI32(message.seqid);
    } else {
      writeString(message.name);
      writeByte(message.type);
      writeI32(message.seqid);
    }
  }

  @override
  void writeMessageEnd() {}

  @override
  void writeStructBegin(TStruct struct) {}

  @override
  void writeStructEnd() {}

  @override
  void writeFieldBegin(TField field) {
    writeByte(field.type);
    writeI16(field.id);
  }

  @override
  void writeFieldEnd() {}

  @override
  void writeFieldStop() {
    writeByte(TType.STOP);
  }

  @override
  void writeMapBegin(TMap map) {
    writeByte(map.keyType);
    writeByte(map.valueType);
    writeI32(map.length);
  }

  @override
  void writeMapEnd() {}

  @override
  void writeListBegin(TList list) {
    writeByte(list.elementType);
    writeI32(list.length);
  }

  @override
  void writeListEnd() {}

  @override
  void writeSetBegin(TSet set) {
    writeByte(set.elementType);
    writeI32(set.length);
  }

  @override
  void writeSetEnd() {}

  @override
  void writeBool(bool b) {
    if (b == null) b = false;
    writeByte(b ? 1 : 0);
  }

  final ByteData _byteOut = ByteData(1);

  @override
  void writeByte(int byte) {
    if (byte == null) byte = 0;
    _byteOut.setUint8(0, byte);
    transport.write(_byteOut.buffer.asUint8List(), 0, 1);
  }

  final ByteData _i16Out = ByteData(2);

  @override
  void writeI16(int i16) {
    if (i16 == null) i16 = 0;
    _i16Out.setInt16(0, i16);
    transport.write(_i16Out.buffer.asUint8List(), 0, 2);
  }

  final ByteData _i32Out = ByteData(4);

  @override
  void writeI32(int i32) {
    if (i32 == null) i32 = 0;
    _i32Out.setInt32(0, i32);
    transport.write(_i32Out.buffer.asUint8List(), 0, 4);
  }

  final Uint8List _i64Out = Uint8List(8);

  @override
  void writeI64(int i64) {
    if (i64 == null) i64 = 0;
    var i = Int64(i64);
    var bts = i.toBytes();
    for (var j = 0; j < 8; j++) {
      _i64Out[j] = bts[8 - j - 1];
    }
    transport.write(_i64Out, 0, 8);
  }

  @override
  void writeString(String s) {
    var bytes = s != null ? _utf8Codec.encode(s) : Uint8List.fromList([]);
    writeI32(bytes.length);
    transport.write(bytes, 0, bytes.length);
  }

  final ByteData _doubleOut = ByteData(8);

  @override
  void writeDouble(double d) {
    if (d == null) d = 0.0;
    _doubleOut.setFloat64(0, d);
    transport.write(_doubleOut.buffer.asUint8List(), 0, 8);
  }

  @override
  void writeBinary(Uint8List bytes) {
    var length = bytes.length;
    writeI32(length);
    transport.write(bytes, 0, length);
  }

  /// read
  @override
  TMessage readMessageBegin() {
    String name;
    int type;
    int seqid;

    int size = readI32();
    if (size < 0) {
      int version = size & VERSION_MASK;
      if (version != VERSION_1) {
        throw TProtocolError(TProtocolErrorType.BAD_VERSION,
            "Bad version in readMessageBegin: $version");
      }
      type = size & 0x000000ff;
      name = readString();
      seqid = readI32();
    } else {
      if (strictRead) {
        throw TProtocolError(TProtocolErrorType.BAD_VERSION,
            "Missing version in readMessageBegin");
      }
      name = _readString(size);
      type = readByte();
      seqid = readI32();
    }
    return TMessage(name, type, seqid);
  }

  @override
  void readMessageEnd() {}

  @override
  TStruct readStructBegin() {
    return TStruct();
  }

  @override
  void readStructEnd() {}

  @override
  TField readFieldBegin() {
    String name = "";
    int type = readByte();
    int id = type != TType.STOP ? readI16() : 0;

    return TField(name, type, id);
  }

  @override
  void readFieldEnd() {}

  @override
  TMap readMapBegin() {
    int keyType = readByte();
    int valueType = readByte();
    int length = readI32();

    return TMap(keyType, valueType, length);
  }

  @override
  void readMapEnd() {}

  @override
  TList readListBegin() {
    int elementType = readByte();
    int length = readI32();

    return TList(elementType, length);
  }

  @override
  void readListEnd() {}

  @override
  TSet readSetBegin() {
    int elementType = readByte();
    int length = readI32();

    return TSet(elementType, length);
  }

  @override
  void readSetEnd() {}

  @override
  bool readBool() => readByte() == 1;

  final Uint8List _byteIn = Uint8List(1);

  @override
  int readByte() {
    transport.readAll(_byteIn, 0, 1);
    return _byteIn.buffer.asByteData().getUint8(0);
  }

  final Uint8List _i16In = Uint8List(2);

  @override
  int readI16() {
    transport.readAll(_i16In, 0, 2);
    return _i16In.buffer.asByteData().getInt16(0);
  }

  final Uint8List _i32In = Uint8List(4);

  @override
  int readI32() {
    transport.readAll(_i32In, 0, 4);
    return _i32In.buffer.asByteData().getInt32(0);
  }

  final Uint8List _i64In = Uint8List(8);

  @override
  int readI64() {
    transport.readAll(_i64In, 0, 8);
    var i = Int64.fromBytesBigEndian(_i64In);
    return i.toInt();
  }

  final Uint8List _doubleIn = Uint8List(8);

  @override
  double readDouble() {
    transport.readAll(_doubleIn, 0, 8);
    return _doubleIn.buffer.asByteData().getFloat64(0);
  }

  @override
  String readString() {
    int size = readI32();
    return _readString(size);
  }

  String _readString(int size) {
    Uint8List stringIn = Uint8List(size);
    transport.readAll(stringIn, 0, size);
    return _utf8Codec.decode(stringIn);
  }

  @override
  Uint8List readBinary() {
    int length = readI32();
    Uint8List binaryIn = Uint8List(length);
    transport.readAll(binaryIn, 0, length);
    return binaryIn;
  }
}
