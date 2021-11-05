/// Licensed to the Apache Software Foundation (ASF) under one
/// or more contributor license agreements. See the NOTICE file
/// distributed with this work for additional information
/// regarding copyright ownership. The ASF licenses this file
/// to you under the Apache License, Version 2.0 (the
/// 'License'); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at
///
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// Unless required by applicable law or agreed to in writing,
/// software distributed under the License is distributed on an
/// 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
/// KIND, either express or implied. See the License for the
/// specific language governing permissions and limitations
/// under the License.

part of thrift;

class TCompactProtocolFactory implements TProtocolFactory<TCompactProtocol> {
  TCompactProtocolFactory();

  @override
  TCompactProtocol getProtocol(TTransport transport) {
    return TCompactProtocol(transport);
  }
}

/// Compact protocol implementation for Thrift.
///
/// Use of fixnum library is required due to bugs like
/// https://github.com/dart-lang/sdk/issues/15361
///
/// Adapted from the Java version.
class TCompactProtocol extends TProtocol {
  static const int PROTOCOL_ID = 0x82;
  static const int VERSION = 1;
  static const int VERSION_MASK = 0x1f;
  static const int TYPE_MASK = 0xE0;
  static const int TYPE_BITS = 0x07;
  static const int TYPE_SHIFT_AMOUNT = 5;
  static final TField TSTOP = TField("", TType.STOP, 0);

  static const int TYPE_BOOLEAN_TRUE = 0x01;
  static const int TYPE_BOOLEAN_FALSE = 0x02;
  static const int TYPE_BYTE = 0x03;
  static const int TYPE_I16 = 0x04;
  static const int TYPE_I32 = 0x05;
  static const int TYPE_I64 = 0x06;
  static const int TYPE_DOUBLE = 0x07;
  static const int TYPE_BINARY = 0x08;
  static const int TYPE_LIST = 0x09;
  static const int TYPE_SET = 0x0A;
  static const int TYPE_MAP = 0x0B;
  static const int TYPE_STRUCT = 0x0C;

  static final List<int> _typeMap = List.unmodifiable(List(16)
    ..[TType.STOP] = TType.STOP
    ..[TType.BOOL] = TYPE_BOOLEAN_TRUE
    ..[TType.BYTE] = TYPE_BYTE
    ..[TType.I16] = TYPE_I16
    ..[TType.I32] = TYPE_I32
    ..[TType.I64] = TYPE_I64
    ..[TType.DOUBLE] = TYPE_DOUBLE
    ..[TType.STRING] = TYPE_BINARY
    ..[TType.LIST] = TYPE_LIST
    ..[TType.SET] = TYPE_SET
    ..[TType.MAP] = TYPE_MAP
    ..[TType.STRUCT] = TYPE_STRUCT);

  static const Utf8Codec _utf8Codec = Utf8Codec();

  // Pretend this is a stack
  DoubleLinkedQueue<int> _lastField = DoubleLinkedQueue<int>();
  int _lastFieldId = 0;

  TField _booleanField;
  bool _boolValue;

  final Uint8List tempList = Uint8List(10);
  final ByteData tempBD = ByteData(10);

  TCompactProtocol(TTransport transport) : super(transport);

  /// Write
  @override
  void writeMessageBegin(TMessage message) {
    writeByte(PROTOCOL_ID);
    writeByte((VERSION & VERSION_MASK) |
        ((message.type << TYPE_SHIFT_AMOUNT) & TYPE_MASK));
    _writeVarInt32(Int32(message.seqid));
    writeString(message.name);
  }

  @override
  void writeMessageEnd() {}

  @override
  void writeStructBegin(TStruct struct) {
    _lastField.addLast(_lastFieldId);
    _lastFieldId = 0;
  }

  @override
  void writeStructEnd() {
    _lastFieldId = _lastField.removeLast();
  }

  @override
  void writeFieldBegin(TField field) {
    if (field.type == TType.BOOL) {
      _booleanField = field;
    } else {
      _writeFieldBegin(field, -1);
    }
  }

  void _writeFieldBegin(TField field, int typeOverride) {
    int typeToWrite =
        typeOverride == -1 ? _getCompactType(field.type) : typeOverride;

    if (field.id > _lastFieldId && field.id - _lastFieldId <= 15) {
      writeByte((field.id - _lastFieldId) << 4 | typeToWrite);
    } else {
      writeByte(typeToWrite);
      writeI16(field.id);
    }

    _lastFieldId = field.id;
  }

  @override
  void writeFieldEnd() {}

  @override
  void writeFieldStop() {
    writeByte(TType.STOP);
  }

  @override
  void writeMapBegin(TMap map) {
    if (map.length == 0) {
      writeByte(0);
    } else {
      _writeVarInt32(Int32(map.length));
      writeByte(
          _getCompactType(map.keyType) << 4 | _getCompactType(map.valueType));
    }
  }

  @override
  void writeMapEnd() {}

  @override
  void writeListBegin(TList list) {
    _writeCollectionBegin(list.elementType, list.length);
  }

  @override
  void writeListEnd() {}

  @override
  void writeSetBegin(TSet set) {
    _writeCollectionBegin(set.elementType, set.length);
  }

  @override
  void writeSetEnd() {}

  @override
  void writeBool(bool b) {
    if (b == null) b = false;
    if (_booleanField != null) {
      _writeFieldBegin(
          _booleanField, b ? TYPE_BOOLEAN_TRUE : TYPE_BOOLEAN_FALSE);
      _booleanField = null;
    } else {
      writeByte(b ? TYPE_BOOLEAN_TRUE : TYPE_BOOLEAN_FALSE);
    }
  }

  @override
  void writeByte(int b) {
    if (b == null) b = 0;
    tempList[0] = b;
    transport.write(tempList, 0, 1);
  }

  @override
  void writeI16(int i16) {
    if (i16 == null) i16 = 0;
    _writeVarInt32(_int32ToZigZag(Int32(i16)));
  }

  @override
  void writeI32(int i32) {
    if (i32 == null) i32 = 0;
    _writeVarInt32(_int32ToZigZag(Int32(i32)));
  }

  @override
  void writeI64(int i64) {
    if (i64 == null) i64 = 0;
    _writeVarInt64(_int64ToZigZag(Int64(i64)));
  }

  @override
  void writeDouble(double d) {
    if (d == null) d = 0.0;
    tempBD.setFloat64(0, d, Endian.little);
    transport.write(tempBD.buffer.asUint8List(), 0, 8);
  }

  @override
  void writeString(String str) {
    Uint8List bytes =
        str != null ? _utf8Codec.encode(str) : Uint8List.fromList([]);
    writeBinary(bytes);
  }

  @override
  void writeBinary(Uint8List bytes) {
    _writeVarInt32(Int32(bytes.length));
    transport.write(bytes, 0, bytes.length);
  }

  void _writeVarInt32(Int32 n) {
    int idx = 0;
    while (true) {
      if ((n & ~0x7F) == 0) {
        tempList[idx++] = (n & 0xFF).toInt();
        break;
      } else {
        tempList[idx++] = (((n & 0x7F) | 0x80) & 0xFF).toInt();
        n = n.shiftRightUnsigned(7);
      }
    }
    transport.write(tempList, 0, idx);
  }

  void _writeVarInt64(Int64 n) {
    int idx = 0;
    while (true) {
      if ((n & ~0x7F) == 0) {
        tempList[idx++] = (n & 0xFF).toInt();
        break;
      } else {
        tempList[idx++] = (((n & 0x7F) | 0x80) & 0xFF).toInt();
        n = n.shiftRightUnsigned(7);
      }
    }
    transport.write(tempList, 0, idx);
  }

  void _writeCollectionBegin(int elemType, int length) {
    if (length <= 14) {
      writeByte(length << 4 | _getCompactType(elemType));
    } else {
      writeByte(0xF0 | _getCompactType(elemType));
      _writeVarInt32(Int32(length));
    }
  }

  Int32 _int32ToZigZag(Int32 n) {
    return (n << 1) ^ (n >> 31);
  }

  Int64 _int64ToZigZag(Int64 n) {
    return (n << 1) ^ (n >> 63);
  }


  // Read
  @override
  TMessage readMessageBegin() {
    int protocolId = readByte();
    if (protocolId != PROTOCOL_ID) {
      throw TProtocolError(TProtocolErrorType.BAD_VERSION,
          'Expected protocol id $PROTOCOL_ID but got $protocolId');
    }
    int versionAndType = readByte();
    int version = versionAndType & VERSION_MASK;
    if (version != VERSION) {
      throw TProtocolError(TProtocolErrorType.BAD_VERSION,
          'Expected version $VERSION but got $version');
    }
    int type = (versionAndType >> TYPE_SHIFT_AMOUNT) & TYPE_BITS;
    int seqId = _readVarInt32().toInt();
    String messageName = readString();
    return TMessage(messageName, type, seqId);
  }

  @override
  void readMessageEnd() {}

  @override
  TStruct readStructBegin() {
    _lastField.addLast(_lastFieldId);
    _lastFieldId = 0;
    // TODO make this a constant?
    return TStruct();
  }

  @override
  void readStructEnd() {
    _lastFieldId = _lastField.removeLast();
  }

  @override
  TField readFieldBegin() {
    int type = readByte();
    if (type == TType.STOP) {
      return TSTOP;
    }

    int fieldId;
    int modifier = (type & 0xF0) >> 4;
    if (modifier == 0) {
      fieldId = readI16();
    } else {
      fieldId = _lastFieldId + modifier;
    }

    TField field = TField('', _getTType(type & 0x0F), fieldId);
    if (_isBoolType(type)) {
      _boolValue = (type & 0x0F) == TYPE_BOOLEAN_TRUE;
    }

    _lastFieldId = field.id;
    return field;
  }

  @override
  void readFieldEnd() {}

  @override
  TMap readMapBegin() {
    int length = _readVarInt32().toInt();
    _checkNegReadLength(length);

    int keyAndValueType = length == 0 ? 0 : readByte();
    int keyType = _getTType(keyAndValueType >> 4);
    int valueType = _getTType(keyAndValueType & 0x0F);
    return TMap(keyType, valueType, length);
  }

  @override
  void readMapEnd() {}

  @override
  TList readListBegin() {
    int lengthAndType = readByte();
    int length = (lengthAndType >> 4) & 0x0F;
    if (length == 15) {
      length = _readVarInt32().toInt();
    }
    _checkNegReadLength(length);
    int type = _getTType(lengthAndType);
    return TList(type, length);
  }

  @override
  void readListEnd() {}

  @override
  TSet readSetBegin() {
    TList tlist = readListBegin();
    return TSet(tlist.elementType, tlist.length);
  }

  @override
  void readSetEnd() {}

  @override
  bool readBool() {
    if (_boolValue != null) {
      bool result = _boolValue;
      _boolValue = null;
      return result;
    }
    return readByte() == TYPE_BOOLEAN_TRUE;
  }

  @override
  int readByte() {
    transport.readAll(tempList, 0, 1);
    return tempList.buffer.asByteData().getUint8(0);
  }

  @override
  int readI16() {
    return _zigzagToInt32(_readVarInt32()).toInt();
  }

  @override
  int readI32() {
    return _zigzagToInt32(_readVarInt32()).toInt();
  }

  @override
  int readI64() {
    return _zigzagToInt64(_readVarInt64()).toInt();
  }

  @override
  double readDouble() {
    transport.readAll(tempList, 0, 8);
    return tempList.buffer.asByteData().getFloat64(0, Endian.little);
  }

  @override
  String readString() {
    int length = _readVarInt32().toInt();
    _checkNegReadLength(length);

    // TODO look at using temp for small strings?
    Uint8List buff = Uint8List(length);
    transport.readAll(buff, 0, length);
    return _utf8Codec.decode(buff);
  }

  @override
  Uint8List readBinary() {
    int length = _readVarInt32().toInt();
    _checkNegReadLength(length);

    Uint8List buff = Uint8List(length);
    transport.readAll(buff, 0, length);
    return buff;
  }

  Int32 _readVarInt32() {
    Int32 result = Int32.ZERO;
    int shift = 0;
    while (true) {
      Int32 b = Int32(readByte());
      result |= (b & 0x7f) << shift;
      if ((b & 0x80) != 0x80) break;
      shift += 7;
    }
    return result;
  }

  Int64 _readVarInt64() {
    Int64 result = Int64.ZERO;
    int shift = 0;
    while (true) {
      Int64 b = Int64(readByte());
      result |= (b & 0x7f) << shift;
      if ((b & 0x80) != 0x80) break;
      shift += 7;
    }
    return result;
  }

  Int32 _zigzagToInt32(Int32 n) {
    return (n.shiftRightUnsigned(1)) ^ -(n & 1);
  }

  Int64 _zigzagToInt64(Int64 n) {
    return (n.shiftRightUnsigned(1)) ^ -(n & 1);
  }

  void _checkNegReadLength(int length) {
    if (length < 0) {
      throw TProtocolError(
          TProtocolErrorType.NEGATIVE_SIZE, 'Negative length: $length');
    }
  }

  int _getCompactType(int ttype) {
    return _typeMap[ttype];
  }

  int _getTType(int type) {
    switch (type & 0x0F) {
      case TType.STOP:
        return TType.STOP;
      case TYPE_BOOLEAN_FALSE:
      case TYPE_BOOLEAN_TRUE:
        return TType.BOOL;
      case TYPE_BYTE:
        return TType.BYTE;
      case TYPE_I16:
        return TType.I16;
      case TYPE_I32:
        return TType.I32;
      case TYPE_I64:
        return TType.I64;
      case TYPE_DOUBLE:
        return TType.DOUBLE;
      case TYPE_BINARY:
        return TType.STRING;
      case TYPE_LIST:
        return TType.LIST;
      case TYPE_SET:
        return TType.SET;
      case TYPE_MAP:
        return TType.MAP;
      case TYPE_STRUCT:
        return TType.STRUCT;
      default:
        throw TProtocolError(
            TProtocolErrorType.INVALID_DATA, "Unknown type: ${type & 0x0F}");
    }
  }

  bool _isBoolType(int b) {
    int lowerNibble = b & 0x0F;
    return lowerNibble == TYPE_BOOLEAN_TRUE ||
        lowerNibble == TYPE_BOOLEAN_FALSE;
  }
}
