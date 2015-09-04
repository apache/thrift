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

/// JSON protocol implementation for Thrift.
///
/// Adapted from the C# version.
class TJsonProtocol extends TProtocol {

  static const int VERSION_1 = 1;

  static const Utf8Codec utf8Codec = const Utf8Codec();

  _BaseContext _context;
  _LookaheadReader _reader;

  List<_BaseContext> contextStack = [];
  final List<int> _tempBuffer = new List(4);

  TJsonProtocol(TTransport transport) : super(transport) {
    _context = new _BaseContext(this);
    _reader = new _LookaheadReader(this);
  }

  void _pushContext(_BaseContext c) {
    contextStack.add(c);
    _context = c;
  }

  void _popContext() {
    _context = contextStack.removeLast();
  }

  /// Read a byte that must match [char]; otherwise throw a [TProtocolError].
  void readJsonSyntaxChar(String char) {
    int charByte = char.codeUnitAt(0);
    int byte = _reader.read();
    if (byte != charByte) {
      throw new TProtocolError(TProtocolErrorType.INVALID_DATA,
          "Unexpected character: ${new String.fromCharCode(byte)}");
    }
  }

  int _hexVal(int byte) {
    if (byte >= _Constants.HEX_0.codeUnitAt(0) &&
        byte <= _Constants.HEX_9.codeUnitAt(0)) {
      return byte - _Constants.HEX_0.codeUnitAt(0);
    } else if (byte >= _Constants.HEX_A.codeUnitAt(0) &&
               byte <= _Constants.HEX_F.codeUnitAt(0)) {
      byte += 10;
      return byte - _Constants.HEX_A.codeUnitAt(0);
    } else {
      throw new TProtocolError(
          TProtocolErrorType.INVALID_DATA, "Expected hex character");
    }
  }

  int _hexChar(int byte) {
    return byte.toRadixString(16).codeUnitAt(0);
  }

  /// write

  /// Write the [bytes] as JSON characters, escaping as needed.
  void _writeJsonString(List<int> bytes) {
    _context.write();
    transport.writeAll(_Constants.QUOTE.codeUnits);

    for (int i = 0; i < bytes.length; i++) {
      int byte = bytes[i];
      if ((byte & 0x00FF) >= 0x30) {
        if (byte == _Constants.BACKSLASH.codeUnitAt(0)) {
          transport.writeAll(_Constants.BACKSLASH.codeUnits);
          transport.writeAll(_Constants.BACKSLASH.codeUnits);
        } else {
          transport.write(bytes, i, 1);
        }
      } else {
        _tempBuffer[0] = _Constants.JSON_CHAR_TABLE[byte];
        if (_tempBuffer[0] == 1) {
          transport.write(bytes, i, 1);
        } else if (_tempBuffer[0] > 1) {
          transport.write(_Constants.BACKSLASH.codeUnits, i, 1);
          transport.write(_tempBuffer, 0, 1);
        } else {
          transport.writeAll(_Constants.ESCSEQ.codeUnits);
          _tempBuffer[0] = _hexChar(byte >> 4);
          _tempBuffer[1] = _hexChar(byte);
          transport.write(_tempBuffer, 0, 2);
        }
      }
    }

    transport.writeAll(_Constants.QUOTE.codeUnits);
  }

  void _writeJsonInteger(int i) {
    _context.write();
    String str = i.toString();

    if (_context.escapeNumbers) {
      transport.writeAll(_Constants.QUOTE.codeUnits);
    }
    transport.writeAll(utf8Codec.encode(str));
    if (_context.escapeNumbers) {
      transport.writeAll(_Constants.QUOTE.codeUnits);
    }
  }

  void _writeJsonDouble(double d) {
    _context.write();
    String str = d.toString();
    bool escapeNumbers = d.isNaN || d.isInfinite || _context.escapeNumbers;

    if (escapeNumbers) {
      transport.writeAll(_Constants.QUOTE.codeUnits);
    }
    transport.writeAll(utf8Codec.encode(str));
    if (escapeNumbers) {
      transport.writeAll(_Constants.QUOTE.codeUnits);
    }
  }

  void _writeJsonBase64(List<int> bytes) {
    _context.write();
    transport.writeAll(_Constants.QUOTE.codeUnits);

    String base64 = CryptoUtils.bytesToBase64(bytes);
    transport.writeAll(utf8Codec.encode(base64));

    transport.writeAll(_Constants.QUOTE.codeUnits);
  }

  void _writeJsonObjectStart() {
    _context.write();
    transport.writeAll(_Constants.LBRACE.codeUnits);
    _pushContext(new _PairContext(this));
  }

  void _writeJsonObjectEnd() {
    _popContext();
    transport.writeAll(_Constants.RBRACE.codeUnits);
  }

  void _writeJsonArrayStart() {
    _context.write();
    transport.writeAll(_Constants.LBRACKET.codeUnits);
    _pushContext(new _ListContext(this));
  }

  void _writeJsonArrayEnd() {
    _popContext();
    transport.writeAll(_Constants.RBRACKET.codeUnits);
  }

  void writeMessageBegin(TMessage message) {
    _writeJsonArrayStart();
    _writeJsonInteger(VERSION_1);

    _writeJsonString(utf8Codec.encode(message.name));
    _writeJsonInteger(message.type);
    _writeJsonInteger(message.seqid);
  }

  void writeMessageEnd() {
    _writeJsonArrayEnd();
  }

  void writeStructBegin(TStruct struct) {
    _writeJsonObjectStart();
  }

  void writeStructEnd() {
    _writeJsonObjectEnd();
  }

  void writeFieldBegin(TField field) {
    _writeJsonInteger(field.id);
    _writeJsonObjectStart();
    _writeJsonString(_Constants.getTypeNameForTypeId(field.type).codeUnits);
  }

  void writeFieldEnd() {
    _writeJsonObjectEnd();
  }

  void writeFieldStop() {}

  void writeMapBegin(TMap map) {
    _writeJsonArrayStart();
    _writeJsonString(_Constants.getTypeNameForTypeId(map.keyType).codeUnits);
    _writeJsonString(_Constants.getTypeNameForTypeId(map.valueType).codeUnits);
    _writeJsonInteger(map.length);
    _writeJsonObjectStart();
  }

  void writeMapEnd() {
    _writeJsonObjectEnd();
    _writeJsonArrayEnd();
  }

  void writeListBegin(TList list) {
    _writeJsonArrayStart();
    _writeJsonString(
        _Constants.getTypeNameForTypeId(list.elementType).codeUnits);
    _writeJsonInteger(list.length);
  }

  void writeListEnd() {
    _writeJsonArrayEnd();
  }

  void writeSetBegin(TSet set) {
    _writeJsonArrayStart();
    _writeJsonString(
        _Constants.getTypeNameForTypeId(set.elementType).codeUnits);
    _writeJsonInteger(set.length);
  }

  void writeSetEnd() {
    _writeJsonArrayEnd();
  }

  void writeBool(bool b) {
    _writeJsonInteger(b ? 1 : 0);
  }

  void writeByte(int b) {
    _writeJsonInteger(b);
  }

  void writeI16(int i16) {
    _writeJsonInteger(i16);
  }

  void writeI32(int i32) {
    _writeJsonInteger(i32);
  }

  void writeI64(int i64) {
    _writeJsonInteger(i64);
  }

  void writeDouble(double d) {
    _writeJsonDouble(d);
  }

  void writeString(String s) {
    _writeJsonString(utf8Codec.encode(s));
  }

  void writeBinary(ByteData bytes) {
    _writeJsonBase64(bytes.buffer.asUint8List());
  }


  /// read

  List<int> _readJsonString({bool skipContext: false}) {
    List<int> bytes = [];

    if (!skipContext) {
      _context.read();
    }

    readJsonSyntaxChar(_Constants.QUOTE);
    while (true) {
      int byte = _reader.read();
      if (byte == _Constants.QUOTE.codeUnitAt(0)) {
        break;
      }

      // escaped?
      if (byte != _Constants.ESCSEQ.codeUnitAt(0)) {
        bytes.add(byte);
        continue;
      }

      byte = _reader.read();

      // distinguish between \u00XX and control chars like \n
      if (byte != _Constants.ESCSEQ.codeUnitAt(1)) {
        String char = new String.fromCharCode(byte);
        int offset = _Constants.ESCAPE_CHARS.indexOf(char);
        if (offset == -1) {
          throw new TProtocolError(
              TProtocolErrorType.INVALID_DATA, "Expected control char");
        }
        byte = _Constants.ESCAPE_CHAR_VALS.codeUnitAt(offset);
        bytes.add(byte);
        continue;
      }

      // it's \u00XX
      readJsonSyntaxChar(_Constants.HEX_0);
      readJsonSyntaxChar(_Constants.HEX_0);
      transport.readAll(_tempBuffer, 0, 2);
      byte = _hexVal(_tempBuffer[0]) << 4 + _hexVal(_tempBuffer[1]);
      bytes.add(byte);
    }

    return bytes;
  }

  String _readJsonNumericChars() {
    StringBuffer buffer = new StringBuffer();
    while (true) {
      int byte = _reader.peek();
      if (!_Constants.isJsonNumeric(byte)) {
        break;
      }
      buffer.write(new String.fromCharCode(byte));
    }
    return buffer.toString();
  }

  int _readJsonInteger() {
    _context.read();

    if (_context.escapeNumbers) {
      readJsonSyntaxChar(_Constants.QUOTE);
    }
    String str = _readJsonNumericChars();
    if (_context.escapeNumbers) {
      readJsonSyntaxChar(_Constants.QUOTE);
    }

    try {
      return int.parse(str);
    } on FormatException catch(_) {
      throw new TProtocolError(TProtocolErrorType.INVALID_DATA,
          "Bad data encounted in numeric data");
    }
  }

  double _readJsonDouble() {
    _context.read();

    if (_reader.peek() == _Constants.QUOTE.codeUnitAt(0)) {
      List<int> bytes = _readJsonString(skipContext: true);
      double d = double.parse(utf8Codec.decode(bytes), (_) {
        throw new TProtocolError(TProtocolErrorType.INVALID_DATA,
                                 "Bad data encounted in numeric data");
      });
      if (!_context.escapeNumbers && !d.isNaN && !d.isInfinite) {
        throw new TProtocolError(TProtocolErrorType.INVALID_DATA,
                                 "Numeric data unexpectedly quoted");
      }
      return d;
    } else {
      if (_context.escapeNumbers) {
        // This will throw - we should have had a quote if escapeNumbers == true
        readJsonSyntaxChar(_Constants.QUOTE);
      }
      return double.parse(_readJsonNumericChars(), (_) {
        throw new TProtocolError(TProtocolErrorType.INVALID_DATA,
                                 "Bad data encounted in numeric data");
      });
    }
  }

  List<int> _readJsonBase64() {
    List<int> bytes = _readJsonString();
    String base64 = utf8Codec.decode(bytes);
    return CryptoUtils.base64StringToBytes(base64);
  }

  void _readJsonObjectStart() {
    _context.read();
    readJsonSyntaxChar(_Constants.LBRACE);
    _pushContext(new _PairContext(this));
  }

  void _readJsonObjectEnd() {
    readJsonSyntaxChar(_Constants.RBRACE);
    _popContext();
  }

  void _readJsonArrayStart() {
    _context.read();
    readJsonSyntaxChar(_Constants.LBRACKET);
    _pushContext(new _ListContext(this));
  }

  void _readJsonArrayEnd() {
    readJsonSyntaxChar(_Constants.RBRACKET);
    _popContext();
  }

  TMessage readMessageBegin() {
    _readJsonArrayStart();
    if (_readJsonInteger() != VERSION_1) {
      throw new TProtocolError(TProtocolErrorType.BAD_VERSION,
                               "Message contained bad version.");
    }

    List<int> buffer = _readJsonString();
    String name = utf8Codec.decode(buffer);
    int type = _readJsonInteger();
    int seqid = _readJsonInteger();

    return new TMessage(name, type, seqid);
  }

  void readMessageEnd() {
    _readJsonArrayEnd();
  }

  TStruct readStructBegin() {
    _readJsonObjectStart();
    return new TStruct();
  }

  void readStructEnd() {
    _readJsonObjectEnd();
  }

  TField readFieldBegin() {
    int byte = _reader.peek();

    String name = "";
    int type = TType.STOP;
    int id = 0;

    if (byte != _Constants.RBRACE.codeUnitAt(0)) {
      id = _readJsonInteger();
      type = _Constants.getTypeIdForTypeName(_readJsonString());
    }

    return new TField(name, type, id);
  }

  void readFieldEnd() {
    _readJsonObjectEnd();
  }

  TMap readMapBegin() {
    _readJsonArrayStart();
    int keyType = _Constants.getTypeIdForTypeName(_readJsonString());
    int valueType = _Constants.getTypeIdForTypeName(_readJsonString());
    int length = _readJsonInteger();
    _readJsonObjectStart();

    return new TMap(keyType, valueType, length);
  }

  void readMapEnd() {
    _readJsonObjectEnd();
    _readJsonArrayEnd();
  }

  TList readListBegin() {
    _readJsonArrayStart();
    int elementType = _Constants.getTypeIdForTypeName(_readJsonString());
    int length = _readJsonInteger();

    return new TList(elementType, length);
  }

  void readListEnd() {
    _readJsonArrayEnd();
  }

  TSet readSetBegin() {
    _readJsonArrayStart();
    int elementType = _Constants.getTypeIdForTypeName(_readJsonString());
    int length = _readJsonInteger();

    return new TSet(elementType, length);
  }

  void readSetEnd() {
    _readJsonArrayEnd();
  }

  bool readBool() {
    return _readJsonInteger() == 0 ? false : true;
  }

  int readByte() {
    return _readJsonInteger();
  }

  int readI16() {
    return _readJsonInteger();
  }

  int readI32() {
    return _readJsonInteger();
  }

  int readI64() {
    return _readJsonInteger();
  }

  double readDouble() {
    return _readJsonDouble();
  }

  String readString() {
    return utf8Codec.decode(_readJsonString());
  }

  ByteData readBinary() {
    return new Uint8List.fromList(_readJsonBase64()).buffer.asByteData();
  }

}

class _Constants {

  static const utf8codec = const Utf8Codec();

  static const String HEX_0 = '0';
  static const String HEX_9 = '9';
  static const String HEX_A = 'a';
  static const String HEX_F = 'f';
  static const String COMMA = ',';
  static const String COLON = ':';
  static const String LBRACE = '{';
  static const String RBRACE = '}';
  static const String LBRACKET = '[';
  static const String RBRACKET = ']';
  static const String QUOTE = '"';
  static const String BACKSLASH = r'\';

  static const String ESCSEQ = r'\u00';

  static final List<int> JSON_CHAR_TABLE = new List.unmodifiable([
    0,  0,  0,  0,  0,  0,  0,  0, 'b'.codeUnitAt(0), 't'.codeUnitAt(0), 'n'.codeUnitAt(0), 0, 'f'.codeUnitAt(0), 'r'.codeUnitAt(0), 0, 0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    1,  1, '"'.codeUnitAt(0),  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1
  ]);

  static const String ESCAPE_CHARS = r'"\/bfnrt';
  static const String ESCAPE_CHAR_VALS = '"\\/\b\f\n\r\t';

  static const String NAME_BOOL = 'tf';
  static const String NAME_BYTE = 'i8';
  static const String NAME_I16 = 'i16';
  static const String NAME_I32 = 'i32';
  static const String NAME_I64 = 'i64';
  static const String NAME_DOUBLE = 'dbl';
  static const String NAME_STRUCT = 'rec';
  static const String NAME_STRING = 'str';
  static const String NAME_MAP = 'map';
  static const String NAME_LIST = 'lst';
  static const String NAME_SET = 'set';

  static final Map<int, String> _TYPE_ID_TO_NAME = new Map.unmodifiable({
    TType.BOOL: NAME_BOOL,
    TType.BYTE: NAME_BYTE,
    TType.I16: NAME_I16,
    TType.I32: NAME_I32,
    TType.I64: NAME_I64,
    TType.DOUBLE: NAME_DOUBLE,
    TType.STRING: NAME_STRING,
    TType.STRUCT: NAME_STRUCT,
    TType.MAP: NAME_MAP,
    TType.SET: NAME_SET,
    TType.LIST: NAME_LIST
  });

  static String getTypeNameForTypeId(int typeId) {
    if (!_TYPE_ID_TO_NAME.containsKey(typeId)) {
      throw new TProtocolError(
          TProtocolErrorType.NOT_IMPLEMENTED, "Unrecognized type");
    }

    return _TYPE_ID_TO_NAME[typeId];
  }

  static final Map<List<int>, int> _NAME_TO_TYPE_ID = new Map.unmodifiable({
    NAME_BOOL.codeUnits: TType.BOOL,
    NAME_BYTE.codeUnits: TType.BYTE,
    NAME_I16.codeUnits: TType.I16,
    NAME_I32.codeUnits: TType.I32,
    NAME_I64.codeUnits: TType.I64,
    NAME_DOUBLE.codeUnits: TType.DOUBLE,
    NAME_STRING.codeUnits: TType.STRING,
    NAME_STRUCT.codeUnits: TType.STRUCT,
    NAME_MAP.codeUnits: TType.MAP,
    NAME_SET.codeUnits: TType.SET,
    NAME_LIST.codeUnits: TType.LIST
  });

  static int getTypeIdForTypeName(List<int> bytes) {
    String name = utf8codec.decode(bytes);
    if (!_NAME_TO_TYPE_ID.containsKey(name)) {
      throw new TProtocolError(
          TProtocolErrorType.NOT_IMPLEMENTED, "Unrecognized type");
    }

    return _NAME_TO_TYPE_ID[name];
  }

  static final Set<int> _JSON_NUMERICS = new Set.from([
    '+', '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'E', 'e'
  ].map((String s) => s.codeUnitAt(0)));

  static bool isJsonNumeric(int byte) {
    return _JSON_NUMERICS.contains(byte);
  }

}

class _LookaheadReader {

  final TJsonProtocol protocol;

  _LookaheadReader(this.protocol);

  bool _hasData;
  final List<int> _data = new List(1);

  int read() {
    if (_hasData) {
      _hasData = false;
    } else {
      protocol.transport.readAll(_data, 0, 1);
    }

    return _data[0];
  }

  int peek() {
    if (!_hasData) {
      protocol.transport.readAll(_data, 0, 1);
    }
    _hasData = true;

    return _data[0];
  }

}

class _BaseContext {

  final TJsonProtocol protocol;

  _BaseContext(this.protocol);

  void write() {}

  void read() {}

  bool get escapeNumbers => false;

}

class _ListContext extends _BaseContext {

  _ListContext(TJsonProtocol protocol) : super(protocol);

  bool _first = true;

  void write() {
    if (_first) {
      _first = false;
    } else {
      protocol.transport.writeAll(_Constants.COMMA.codeUnits);
    }
  }

  void read() {
    if (_first) {
      _first = false;
    } else {
      protocol.readJsonSyntaxChar(_Constants.COMMA);
    }
  }

}

class _PairContext extends _BaseContext {

  _PairContext(TJsonProtocol protocol) : super(protocol);

  bool _first = true;
  bool _colon = true;

  String get symbol => _colon ? _Constants.COLON : _Constants.COMMA;

  void write() {
    if (_first) {
      _first = false;
      _colon = true;
    } else {
      protocol.transport.writeAll(symbol.codeUnits);
      _colon = !_colon;
    }
  }

  void read() {
    if (_first) {
      _first = false;
      _colon = true;
    } else {
      protocol.readJsonSyntaxChar(symbol);
      _colon = !_colon;
    }
  }

  bool get escapeNumbers => _colon;

}
