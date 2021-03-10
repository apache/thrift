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

class TJsonProtocolFactory implements TProtocolFactory<TJsonProtocol> {
  @override
  TJsonProtocol getProtocol(TTransport transport) {
    return TJsonProtocol(transport);
  }
}

/// JSON protocol implementation for Thrift.
///
/// Adapted from the C# version.
class TJsonProtocol extends TProtocol {
  static const int VERSION_1 = 1;

  static const Utf8Codec utf8Codec = Utf8Codec();

  _BaseContext _context;
  _BaseContext _rootContext;
  _LookaheadReader _reader;

  final List<_BaseContext> _contextStack = [];
  final Uint8List _tempBuffer = Uint8List(4);

  TJsonProtocol(TTransport transport) : super(transport) {
    _rootContext = _BaseContext(this);
    _reader = _LookaheadReader(this);
    _resetContext();
  }

  void _pushContext(_BaseContext c) {
    _contextStack.add(c);
    _context = c;
  }

  void _popContext() {
    _contextStack.removeLast();
    _context = _contextStack.isEmpty ? _rootContext : _contextStack.last;
  }

  void _resetContext() {
    _contextStack.clear();
    _context = _rootContext;
  }

  /// Read a byte that must match [char]; otherwise throw a [TProtocolError].
  void _readJsonSyntaxChar(int charByte) {
    int byte = _reader.read();
    if (byte != charByte) {
      throw TProtocolError(TProtocolErrorType.INVALID_DATA,
          "Expected character ${String.fromCharCode(charByte)} but found: ${String.fromCharCode(byte)}");
    }
  }

  int _hexVal(int byte) {
    if (byte >= _Constants.HEX_0_BYTES[0] &&
        byte <= _Constants.HEX_9_BYTES[0]) {
      return byte - _Constants.HEX_0_BYTES[0];
    } else if (byte >= _Constants.HEX_A_BYTES[0] &&
        byte <= _Constants.HEX_F_BYTES[0]) {
      byte += 10;
      return byte - _Constants.HEX_A_BYTES[0];
    } else {
      throw TProtocolError(
          TProtocolErrorType.INVALID_DATA, "Expected hex character");
    }
  }

  int _hexChar(int byte) => byte.toRadixString(16).codeUnitAt(0);

  /// write

  /// Write the [bytes] as JSON characters, escaping as needed.
  void _writeJsonString(Uint8List bytes) {
    _context.write();
    transport.writeAll(_Constants.QUOTE_BYTES);

    int length = bytes.length;
    for (int i = 0; i < length; i++) {
      int byte = bytes[i];
      if ((byte & 0x00FF) >= 0x30) {
        if (byte == _Constants.BACKSLASH_BYTES[0]) {
          transport.writeAll(_Constants.BACKSLASH_BYTES);
          transport.writeAll(_Constants.BACKSLASH_BYTES);
        } else {
          transport.write(bytes, i, 1);
        }
      } else {
        _tempBuffer[0] = _Constants.JSON_CHAR_TABLE[byte];
        if (_tempBuffer[0] == 1) {
          transport.write(bytes, i, 1);
        } else if (_tempBuffer[0] > 1) {
          transport.writeAll(_Constants.BACKSLASH_BYTES);
          transport.write(_tempBuffer, 0, 1);
        } else {
          transport.writeAll(_Constants.ESCSEQ_BYTES);
          _tempBuffer[0] = _hexChar(byte >> 4);
          _tempBuffer[1] = _hexChar(byte);
          transport.write(_tempBuffer, 0, 2);
        }
      }
    }

    transport.writeAll(_Constants.QUOTE_BYTES);
  }

  void _writeJsonInteger(int i) {
    if (i == null) i = 0;

    _context.write();
    String str = i.toString();

    if (_context.escapeNumbers) {
      transport.writeAll(_Constants.QUOTE_BYTES);
    }
    transport.writeAll(utf8Codec.encode(str));
    if (_context.escapeNumbers) {
      transport.writeAll(_Constants.QUOTE_BYTES);
    }
  }

  void _writeJsonDouble(double d) {
    if (d == null) d = 0.0;

    _context.write();
    String str = d.toString();
    bool escapeNumbers = d.isNaN || d.isInfinite || _context.escapeNumbers;

    if (escapeNumbers) {
      transport.writeAll(_Constants.QUOTE_BYTES);
    }
    transport.writeAll(utf8Codec.encode(str));
    if (escapeNumbers) {
      transport.writeAll(_Constants.QUOTE_BYTES);
    }
  }

  void _writeJsonBase64(Uint8List bytes) {
    _context.write();
    transport.writeAll(_Constants.QUOTE_BYTES);

    String base64text = base64.encode(bytes);
    transport.writeAll(utf8Codec.encode(base64text));

    transport.writeAll(_Constants.QUOTE_BYTES);
  }

  void _writeJsonObjectStart() {
    _context.write();
    transport.writeAll(_Constants.LBRACE_BYTES);
    _pushContext(_PairContext(this));
  }

  void _writeJsonObjectEnd() {
    _popContext();
    transport.writeAll(_Constants.RBRACE_BYTES);
  }

  void _writeJsonArrayStart() {
    _context.write();
    transport.writeAll(_Constants.LBRACKET_BYTES);
    _pushContext(_ListContext(this));
  }

  void _writeJsonArrayEnd() {
    _popContext();
    transport.writeAll(_Constants.RBRACKET_BYTES);
  }

  @override
  void writeMessageBegin(TMessage message) {
    _resetContext();

    _writeJsonArrayStart();
    _writeJsonInteger(VERSION_1);

    _writeJsonString(utf8Codec.encode(message.name));
    _writeJsonInteger(message.type);
    _writeJsonInteger(message.seqid);
  }

  @override
  void writeMessageEnd() {
    _writeJsonArrayEnd();
  }

  @override
  void writeStructBegin(TStruct struct) {
    _writeJsonObjectStart();
  }

  @override
  void writeStructEnd() {
    _writeJsonObjectEnd();
  }

  @override
  void writeFieldBegin(TField field) {
    _writeJsonInteger(field.id);
    _writeJsonObjectStart();
    _writeJsonString(_Constants.getTypeNameBytesForTypeId(field.type));
  }

  @override
  void writeFieldEnd() {
    _writeJsonObjectEnd();
  }

  @override
  void writeFieldStop() {}

  @override
  void writeMapBegin(TMap map) {
    _writeJsonArrayStart();
    _writeJsonString(_Constants.getTypeNameBytesForTypeId(map.keyType));
    _writeJsonString(_Constants.getTypeNameBytesForTypeId(map.valueType));
    _writeJsonInteger(map.length);
    _writeJsonObjectStart();
  }

  @override
  void writeMapEnd() {
    _writeJsonObjectEnd();
    _writeJsonArrayEnd();
  }

  @override
  void writeListBegin(TList list) {
    _writeJsonArrayStart();
    _writeJsonString(_Constants.getTypeNameBytesForTypeId(list.elementType));
    _writeJsonInteger(list.length);
  }

  @override
  void writeListEnd() {
    _writeJsonArrayEnd();
  }

  @override
  void writeSetBegin(TSet set) {
    _writeJsonArrayStart();
    _writeJsonString(_Constants.getTypeNameBytesForTypeId(set.elementType));
    _writeJsonInteger(set.length);
  }

  @override
  void writeSetEnd() {
    _writeJsonArrayEnd();
  }

  @override
  void writeBool(bool b) {
    if (b == null) b = false;
    _writeJsonInteger(b ? 1 : 0);
  }

  @override
  void writeByte(int b) {
    _writeJsonInteger(b);
  }

  @override
  void writeI16(int i16) {
    _writeJsonInteger(i16);
  }

  @override
  void writeI32(int i32) {
    _writeJsonInteger(i32);
  }

  @override
  void writeI64(int i64) {
    _writeJsonInteger(i64);
  }

  @override
  void writeDouble(double d) {
    _writeJsonDouble(d);
  }

  @override
  void writeString(String s) {
    var bytes = s != null ? utf8Codec.encode(s) : Uint8List.fromList([]);
    _writeJsonString(bytes);
  }

  @override
  void writeBinary(Uint8List bytes) {
    _writeJsonBase64(bytes);
  }

  bool _isHighSurrogate(int b) => b >= 0xD800 && b <= 0xDBFF;

  bool _isLowSurrogate(int b) => b >= 0xDC00 && b <= 0xDFFF;

  /// read
  Uint8List _readJsonString({bool skipContext = false}) {
    List<int> bytes = [];
    List<int> codeunits = [];

    if (!skipContext) {
      _context.read();
    }

    _readJsonSyntaxChar(_Constants.QUOTE_BYTES[0]);
    while (true) {
      int byte = _reader.read();
      if (byte == _Constants.QUOTE_BYTES[0]) {
        break;
      }

      // escaped?
      if (byte != _Constants.ESCSEQ_BYTES[0]) {
        bytes.add(byte);
        continue;
      }

      byte = _reader.read();

      // distinguish between \uXXXX and control chars like \n
      if (byte != _Constants.ESCSEQ_BYTES[1]) {
        String char = String.fromCharCode(byte);
        int offset = _Constants.ESCAPE_CHARS.indexOf(char);
        if (offset == -1) {
          throw TProtocolError(
              TProtocolErrorType.INVALID_DATA, "Expected control char");
        }
        byte = _Constants.ESCAPE_CHAR_VALS.codeUnitAt(offset);
        bytes.add(byte);
        continue;
      }

      // it's \uXXXX
      transport.readAll(_tempBuffer, 0, 4);
      byte = (_hexVal(_tempBuffer[0]) << 12) +
          (_hexVal(_tempBuffer[1]) << 8) +
          (_hexVal(_tempBuffer[2]) << 4) +
          _hexVal(_tempBuffer[3]);
      if (_isHighSurrogate(byte)) {
        if (codeunits.isNotEmpty) {
          throw TProtocolError(
              TProtocolErrorType.INVALID_DATA, "Expected low surrogate");
        }
        codeunits.add(byte);
      } else if (_isLowSurrogate(byte)) {
        if (codeunits.isEmpty) {
          throw TProtocolError(
              TProtocolErrorType.INVALID_DATA, "Expected high surrogate");
        }
        codeunits.add(byte);
        bytes.addAll(utf8Codec.encode(String.fromCharCodes(codeunits)));
        codeunits.clear();
      } else {
        bytes.addAll(utf8Codec.encode(String.fromCharCode(byte)));
      }
    }

    if (codeunits.isNotEmpty) {
      throw TProtocolError(
          TProtocolErrorType.INVALID_DATA, "Expected low surrogate");
    }

    return Uint8List.fromList(bytes);
  }

  String _readJsonNumericChars() {
    StringBuffer buffer = StringBuffer();
    while (true) {
      if (!_Constants.isJsonNumeric(_reader.peek())) {
        break;
      }
      buffer.write(String.fromCharCode(_reader.read()));
    }
    return buffer.toString();
  }

  int _readJsonInteger() {
    _context.read();

    if (_context.escapeNumbers) {
      _readJsonSyntaxChar(_Constants.QUOTE_BYTES[0]);
    }
    String str = _readJsonNumericChars();
    if (_context.escapeNumbers) {
      _readJsonSyntaxChar(_Constants.QUOTE_BYTES[0]);
    }

    try {
      return int.parse(str);
    } on FormatException catch (_) {
      throw TProtocolError(TProtocolErrorType.INVALID_DATA,
          "Bad data encounted in numeric data");
    }
  }

  double _readJsonDouble() {
    _context.read();

    if (_reader.peek() == _Constants.QUOTE_BYTES[0]) {
      Uint8List bytes = _readJsonString(skipContext: true);
      double d;
      try {
        d = double.tryParse(utf8Codec.decode(bytes));
      } catch (_) {
        throw TProtocolError(TProtocolErrorType.INVALID_DATA,
            "Bad data encounted in numeric data");
      }
      if (!_context.escapeNumbers && !d.isNaN && !d.isInfinite) {
        throw TProtocolError(TProtocolErrorType.INVALID_DATA,
            "Numeric data unexpectedly quoted");
      }
      return d;
    } else {
      if (_context.escapeNumbers) {
        // This will throw - we should have had a quote if escapeNumbers == true
        _readJsonSyntaxChar(_Constants.QUOTE_BYTES[0]);
      }
      try {
        return double.parse(_readJsonNumericChars());
      } on FormatException catch (_) {
        throw TProtocolError(TProtocolErrorType.INVALID_DATA,
            "Bad data encounted in numeric data");
      }
    }
  }

  Uint8List _readJsonBase64() {
    // convert UTF-8 bytes of a Base 64 encoded string to binary bytes
    Uint8List base64Bytes = _readJsonString();
    String base64text = utf8Codec.decode(base64Bytes);

    return Uint8List.fromList(base64.decode(base64text));
  }

  void _readJsonObjectStart() {
    _context.read();
    _readJsonSyntaxChar(_Constants.LBRACE_BYTES[0]);
    _pushContext(_PairContext(this));
  }

  void _readJsonObjectEnd() {
    _readJsonSyntaxChar(_Constants.RBRACE_BYTES[0]);
    _popContext();
  }

  void _readJsonArrayStart() {
    _context.read();
    _readJsonSyntaxChar(_Constants.LBRACKET_BYTES[0]);
    _pushContext(_ListContext(this));
  }

  void _readJsonArrayEnd() {
    _readJsonSyntaxChar(_Constants.RBRACKET_BYTES[0]);
    _popContext();
  }

  @override
  TMessage readMessageBegin() {
    _resetContext();

    _readJsonArrayStart();
    if (_readJsonInteger() != VERSION_1) {
      throw TProtocolError(
          TProtocolErrorType.BAD_VERSION, "Message contained bad version.");
    }

    Uint8List buffer = _readJsonString();
    String name = utf8Codec.decode(buffer);
    int type = _readJsonInteger();
    int seqid = _readJsonInteger();

    return TMessage(name, type, seqid);
  }

  @override
  void readMessageEnd() {
    _readJsonArrayEnd();
  }

  @override
  TStruct readStructBegin() {
    _readJsonObjectStart();
    return TStruct();
  }

  @override
  void readStructEnd() {
    _readJsonObjectEnd();
  }

  @override
  TField readFieldBegin() {
    String name = "";
    int type = TType.STOP;
    int id = 0;

    if (_reader.peek() != _Constants.RBRACE_BYTES[0]) {
      id = _readJsonInteger();
      _readJsonObjectStart();
      type = _Constants.getTypeIdForTypeName(_readJsonString());
    }

    return TField(name, type, id);
  }

  @override
  void readFieldEnd() {
    _readJsonObjectEnd();
  }

  @override
  TMap readMapBegin() {
    _readJsonArrayStart();
    int keyType = _Constants.getTypeIdForTypeName(_readJsonString());
    int valueType = _Constants.getTypeIdForTypeName(_readJsonString());
    int length = _readJsonInteger();
    _readJsonObjectStart();

    return TMap(keyType, valueType, length);
  }

  @override
  void readMapEnd() {
    _readJsonObjectEnd();
    _readJsonArrayEnd();
  }

  @override
  TList readListBegin() {
    _readJsonArrayStart();
    int elementType = _Constants.getTypeIdForTypeName(_readJsonString());
    int length = _readJsonInteger();

    return TList(elementType, length);
  }

  @override
  void readListEnd() {
    _readJsonArrayEnd();
  }

  @override
  TSet readSetBegin() {
    _readJsonArrayStart();
    int elementType = _Constants.getTypeIdForTypeName(_readJsonString());
    int length = _readJsonInteger();

    return TSet(elementType, length);
  }

  @override
  void readSetEnd() {
    _readJsonArrayEnd();
  }

  @override
  bool readBool() {
    return _readJsonInteger() == 0 ? false : true;
  }

  @override
  int readByte() {
    return _readJsonInteger();
  }

  @override
  int readI16() {
    return _readJsonInteger();
  }

  @override
  int readI32() {
    return _readJsonInteger();
  }

  @override
  int readI64() {
    return _readJsonInteger();
  }

  @override
  double readDouble() {
    return _readJsonDouble();
  }

  @override
  String readString() {
    return utf8Codec.decode(_readJsonString());
  }

  @override
  Uint8List readBinary() {
    return Uint8List.fromList(_readJsonBase64());
  }
}

class _Constants {
  static const utf8codec = Utf8Codec();

  static final Uint8List HEX_0_BYTES = Uint8List.fromList('0'.codeUnits);
  static final Uint8List HEX_9_BYTES = Uint8List.fromList('9'.codeUnits);
  static final Uint8List HEX_A_BYTES = Uint8List.fromList('a'.codeUnits);
  static final Uint8List HEX_F_BYTES = Uint8List.fromList('f'.codeUnits);
  static final Uint8List COMMA_BYTES = Uint8List.fromList(','.codeUnits);
  static final Uint8List COLON_BYTES = Uint8List.fromList(':'.codeUnits);
  static final Uint8List LBRACE_BYTES = Uint8List.fromList('{'.codeUnits);
  static final Uint8List RBRACE_BYTES = Uint8List.fromList('}'.codeUnits);
  static final Uint8List LBRACKET_BYTES = Uint8List.fromList('['.codeUnits);
  static final Uint8List RBRACKET_BYTES = Uint8List.fromList(']'.codeUnits);
  static final Uint8List QUOTE_BYTES = Uint8List.fromList('"'.codeUnits);
  static final Uint8List BACKSLASH_BYTES = Uint8List.fromList(r'\'.codeUnits);

  static final ESCSEQ_BYTES = Uint8List.fromList(r'\u00'.codeUnits);

  static final Uint8List JSON_CHAR_TABLE = Uint8List.fromList([
    0, 0, 0, 0, 0, 0, 0, 0, // 8 bytes
    'b'.codeUnitAt(0), 't'.codeUnitAt(0), 'n'.codeUnitAt(0), 0, // 4 bytes
    'f'.codeUnitAt(0), 'r'.codeUnitAt(0), 0, 0, // 4 bytes
    0, 0, 0, 0, 0, 0, 0, 0, // 8 bytes
    0, 0, 0, 0, 0, 0, 0, 0, // 8 bytes
    1, 1, '"'.codeUnitAt(0), 1, 1, 1, 1, 1, // 8 bytes
    1, 1, 1, 1, 1, 1, 1, 1 // 8 bytes
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

  static final Map<int, Uint8List> _TYPE_ID_TO_NAME_BYTES = Map.unmodifiable({
    TType.BOOL: Uint8List.fromList(NAME_BOOL.codeUnits),
    TType.BYTE: Uint8List.fromList(NAME_BYTE.codeUnits),
    TType.I16: Uint8List.fromList(NAME_I16.codeUnits),
    TType.I32: Uint8List.fromList(NAME_I32.codeUnits),
    TType.I64: Uint8List.fromList(NAME_I64.codeUnits),
    TType.DOUBLE: Uint8List.fromList(NAME_DOUBLE.codeUnits),
    TType.STRING: Uint8List.fromList(NAME_STRING.codeUnits),
    TType.STRUCT: Uint8List.fromList(NAME_STRUCT.codeUnits),
    TType.MAP: Uint8List.fromList(NAME_MAP.codeUnits),
    TType.SET: Uint8List.fromList(NAME_SET.codeUnits),
    TType.LIST: Uint8List.fromList(NAME_LIST.codeUnits)
  });

  static Uint8List getTypeNameBytesForTypeId(int typeId) {
    if (!_TYPE_ID_TO_NAME_BYTES.containsKey(typeId)) {
      throw TProtocolError(
          TProtocolErrorType.NOT_IMPLEMENTED, "Unrecognized type");
    }

    return _TYPE_ID_TO_NAME_BYTES[typeId];
  }

  static final Map<String, int> _NAME_TO_TYPE_ID = Map.unmodifiable({
    NAME_BOOL: TType.BOOL,
    NAME_BYTE: TType.BYTE,
    NAME_I16: TType.I16,
    NAME_I32: TType.I32,
    NAME_I64: TType.I64,
    NAME_DOUBLE: TType.DOUBLE,
    NAME_STRING: TType.STRING,
    NAME_STRUCT: TType.STRUCT,
    NAME_MAP: TType.MAP,
    NAME_SET: TType.SET,
    NAME_LIST: TType.LIST
  });

  static int getTypeIdForTypeName(Uint8List bytes) {
    String name = utf8codec.decode(bytes);
    if (!_NAME_TO_TYPE_ID.containsKey(name)) {
      throw TProtocolError(
          TProtocolErrorType.NOT_IMPLEMENTED, "Unrecognized type");
    }

    return _NAME_TO_TYPE_ID[name];
  }

  static final Set<int> _JSON_NUMERICS = Set.from([
    '+'.codeUnitAt(0),
    '-'.codeUnitAt(0),
    '.'.codeUnitAt(0),
    '0'.codeUnitAt(0),
    '1'.codeUnitAt(0),
    '2'.codeUnitAt(0),
    '3'.codeUnitAt(0),
    '4'.codeUnitAt(0),
    '5'.codeUnitAt(0),
    '6'.codeUnitAt(0),
    '7'.codeUnitAt(0),
    '8'.codeUnitAt(0),
    '9'.codeUnitAt(0),
    'E'.codeUnitAt(0),
    'e'.codeUnitAt(0)
  ]);

  static bool isJsonNumeric(int byte) {
    return _JSON_NUMERICS.contains(byte);
  }
}

class _LookaheadReader {
  final TJsonProtocol protocol;

  _LookaheadReader(this.protocol);

  bool _hasData = false;
  final Uint8List _data = Uint8List(1);

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

  @override
  String toString() => 'BaseContext';
}

class _ListContext extends _BaseContext {
  _ListContext(TJsonProtocol protocol) : super(protocol);

  bool _first = true;

  @override
  void write() {
    if (_first) {
      _first = false;
    } else {
      protocol.transport.writeAll(_Constants.COMMA_BYTES);
    }
  }

  @override
  void read() {
    if (_first) {
      _first = false;
    } else {
      protocol._readJsonSyntaxChar(_Constants.COMMA_BYTES[0]);
    }
  }

  @override
  String toString() => 'ListContext';
}

class _PairContext extends _BaseContext {
  _PairContext(TJsonProtocol protocol) : super(protocol);

  bool _first = true;
  bool _colon = true;

  Uint8List get symbolBytes =>
      _colon ? _Constants.COLON_BYTES : _Constants.COMMA_BYTES;

  @override
  void write() {
    if (_first) {
      _first = false;
      _colon = true;
    } else {
      protocol.transport.writeAll(symbolBytes);
      _colon = !_colon;
    }
  }

  @override
  void read() {
    if (_first) {
      _first = false;
      _colon = true;
    } else {
      protocol._readJsonSyntaxChar(symbolBytes[0]);
      _colon = !_colon;
    }
  }

  @override
  bool get escapeNumbers => _colon;

  @override
  String toString() => 'PairContext';
}
