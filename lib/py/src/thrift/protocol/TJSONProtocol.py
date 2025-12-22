#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

from __future__ import annotations

import base64
import math

from thrift.protocol.TProtocol import TProtocolBase, TProtocolException, TProtocolFactory, checkIntegerLimits
from thrift.Thrift import TType
from thrift.transport.TTransport import TTransportBase


__all__ = ['TJSONProtocol',
           'TJSONProtocolFactory',
           'TSimpleJSONProtocol',
           'TSimpleJSONProtocolFactory']

VERSION: int = 1

COMMA: bytes = b','
COLON: bytes = b':'
LBRACE: bytes = b'{'
RBRACE: bytes = b'}'
LBRACKET: bytes = b'['
RBRACKET: bytes = b']'
QUOTE: bytes = b'"'
BACKSLASH: bytes = b'\\'
ZERO: bytes = b'0'

ESCSEQ0: int = ord('\\')
ESCSEQ1: int = ord('u')
ESCAPE_CHAR_VALS: dict[str, str] = {
    '"': '\\"',
    '\\': '\\\\',
    '\b': '\\b',
    '\f': '\\f',
    '\n': '\\n',
    '\r': '\\r',
    '\t': '\\t',
    # '/': '\\/',
}
ESCAPE_CHARS: dict[bytes, str] = {
    b'"': '"',
    b'\\': '\\',
    b'b': '\b',
    b'f': '\f',
    b'n': '\n',
    b'r': '\r',
    b't': '\t',
    b'/': '/',
}
NUMERIC_CHAR: bytes = b'+-.0123456789Ee'

CTYPES: dict[int, str] = {
    TType.BOOL: 'tf',
    TType.BYTE: 'i8',
    TType.I16: 'i16',
    TType.I32: 'i32',
    TType.I64: 'i64',
    TType.DOUBLE: 'dbl',
    TType.STRING: 'str',
    TType.STRUCT: 'rec',
    TType.LIST: 'lst',
    TType.SET: 'set',
    TType.MAP: 'map',
}

JTYPES: dict[str, int] = {}
for key in CTYPES.keys():
    JTYPES[CTYPES[key]] = key


class JSONBaseContext:
    protocol: TJSONProtocolBase
    first: bool

    def __init__(self, protocol: TJSONProtocolBase) -> None:
        self.protocol = protocol
        self.first = True

    def doIO(self, function: object) -> None:
        pass

    def write(self) -> None:
        pass

    def read(self) -> None:
        pass

    def escapeNum(self) -> bool:
        return False

    def __str__(self) -> str:
        return self.__class__.__name__


class JSONListContext(JSONBaseContext):

    def doIO(self, function: object) -> None:
        if self.first is True:
            self.first = False
        else:
            function(COMMA)  # type: ignore[operator]

    def write(self) -> None:
        self.doIO(self.protocol.trans.write)

    def read(self) -> None:
        self.doIO(self.protocol.readJSONSyntaxChar)


class JSONPairContext(JSONBaseContext):
    colon: bool

    def __init__(self, protocol: TJSONProtocolBase) -> None:
        super(JSONPairContext, self).__init__(protocol)
        self.colon = True

    def doIO(self, function: object) -> None:
        if self.first:
            self.first = False
            self.colon = True
        else:
            function(COLON if self.colon else COMMA)  # type: ignore[operator]
            self.colon = not self.colon

    def write(self) -> None:
        self.doIO(self.protocol.trans.write)

    def read(self) -> None:
        self.doIO(self.protocol.readJSONSyntaxChar)

    def escapeNum(self) -> bool:
        return self.colon

    def __str__(self) -> str:
        return '%s, colon=%s' % (self.__class__.__name__, self.colon)


class LookaheadReader:
    hasData: bool = False
    data: bytes = b''
    protocol: TJSONProtocolBase

    def __init__(self, protocol: TJSONProtocolBase) -> None:
        self.protocol = protocol

    def read(self) -> bytes:
        if self.hasData is True:
            self.hasData = False
        else:
            self.data = self.protocol.trans.read(1)
        return self.data

    def peek(self) -> bytes:
        if self.hasData is False:
            self.data = self.protocol.trans.read(1)
        self.hasData = True
        return self.data


class TJSONProtocolBase(TProtocolBase):
    context: JSONBaseContext
    contextStack: list[JSONBaseContext]
    reader: LookaheadReader

    def __init__(self, trans: TTransportBase) -> None:
        TProtocolBase.__init__(self, trans)
        self.resetWriteContext()
        self.resetReadContext()

    # We don't have length limit implementation for JSON protocols
    @property
    def string_length_limit(self) -> None:
        return None

    @property
    def container_length_limit(self) -> None:
        return None

    def resetWriteContext(self) -> None:
        self.context = JSONBaseContext(self)
        self.contextStack = [self.context]

    def resetReadContext(self) -> None:
        self.resetWriteContext()
        self.reader = LookaheadReader(self)

    def pushContext(self, ctx: JSONBaseContext) -> None:
        self.contextStack.append(ctx)
        self.context = ctx

    def popContext(self) -> None:
        self.contextStack.pop()
        if self.contextStack:
            self.context = self.contextStack[-1]
        else:
            self.context = JSONBaseContext(self)

    def writeJSONString(self, string: str) -> None:
        self.context.write()
        json_str = ['"']
        for s in string:
            escaped = ESCAPE_CHAR_VALS.get(s, s)
            json_str.append(escaped)
        json_str.append('"')
        self.trans.write(bytes(''.join(json_str), 'utf-8'))

    def writeJSONNumber(self, number: int | float, formatter: str = '{0}') -> None:
        self.context.write()
        jsNumber = str(formatter.format(number)).encode('ascii')
        if self.context.escapeNum():
            self.trans.write(QUOTE)
            self.trans.write(jsNumber)
            self.trans.write(QUOTE)
        else:
            self.trans.write(jsNumber)

    def writeJSONBase64(self, binary: bytes) -> None:
        self.context.write()
        self.trans.write(QUOTE)
        self.trans.write(base64.b64encode(binary))
        self.trans.write(QUOTE)

    def writeJSONObjectStart(self) -> None:
        self.context.write()
        self.trans.write(LBRACE)
        self.pushContext(JSONPairContext(self))

    def writeJSONObjectEnd(self) -> None:
        self.popContext()
        self.trans.write(RBRACE)

    def writeJSONArrayStart(self) -> None:
        self.context.write()
        self.trans.write(LBRACKET)
        self.pushContext(JSONListContext(self))

    def writeJSONArrayEnd(self) -> None:
        self.popContext()
        self.trans.write(RBRACKET)

    def readJSONSyntaxChar(self, character: bytes) -> None:
        current = self.reader.read()
        if character != current:
            raise TProtocolException(TProtocolException.INVALID_DATA,
                                     "Unexpected character: %s" % current)

    def _isHighSurrogate(self, codeunit: int) -> bool:
        return codeunit >= 0xd800 and codeunit <= 0xdbff

    def _isLowSurrogate(self, codeunit: int) -> bool:
        return codeunit >= 0xdc00 and codeunit <= 0xdfff

    def _toChar(self, high: int, low: int | None = None) -> str:
        if not low:
            return chr(high)
        else:
            codepoint = (1 << 16) + ((high & 0x3ff) << 10)
            codepoint += low & 0x3ff
            return chr(codepoint)

    def readJSONString(self, skipContext: bool) -> str:
        highSurrogate: int | None = None
        string: list[str] = []
        if skipContext is False:
            self.context.read()
        self.readJSONSyntaxChar(QUOTE)
        while True:
            character = self.reader.read()
            if character == QUOTE:
                break
            if ord(character) == ESCSEQ0:
                character = self.reader.read()
                if ord(character) == ESCSEQ1:
                    char_str = self.trans.read(4).decode('ascii')
                    codeunit = int(char_str, 16)
                    if self._isHighSurrogate(codeunit):
                        if highSurrogate:
                            raise TProtocolException(
                                TProtocolException.INVALID_DATA,
                                "Expected low surrogate char")
                        highSurrogate = codeunit
                        continue
                    elif self._isLowSurrogate(codeunit):
                        if not highSurrogate:
                            raise TProtocolException(
                                TProtocolException.INVALID_DATA,
                                "Expected high surrogate char")
                        char_result = self._toChar(highSurrogate, codeunit)
                        highSurrogate = None
                    else:
                        char_result = self._toChar(codeunit)
                else:
                    if character not in ESCAPE_CHARS:
                        raise TProtocolException(
                            TProtocolException.INVALID_DATA,
                            "Expected control char")
                    char_result = ESCAPE_CHARS[character]
            elif character.decode('latin-1') in ESCAPE_CHAR_VALS:
                raise TProtocolException(TProtocolException.INVALID_DATA,
                                         "Unescaped control char")
            else:
                utf8_bytes = bytearray([ord(character)])
                while ord(self.reader.peek()) >= 0x80:
                    utf8_bytes.append(ord(self.reader.read()))
                char_result = utf8_bytes.decode('utf-8')
            string.append(char_result)

            if highSurrogate:
                raise TProtocolException(TProtocolException.INVALID_DATA,
                                         "Expected low surrogate char")
        return ''.join(string)

    def isJSONNumeric(self, character: bytes) -> bool:
        return True if NUMERIC_CHAR.find(character) != -1 else False

    def readJSONQuotes(self) -> None:
        if self.context.escapeNum():
            self.readJSONSyntaxChar(QUOTE)

    def readJSONNumericChars(self) -> str:
        numeric: list[bytes] = []
        while True:
            character = self.reader.peek()
            if self.isJSONNumeric(character) is False:
                break
            numeric.append(self.reader.read())
        return b''.join(numeric).decode('ascii')

    def readJSONInteger(self) -> int:
        self.context.read()
        self.readJSONQuotes()
        numeric = self.readJSONNumericChars()
        self.readJSONQuotes()
        try:
            return int(numeric)
        except ValueError:
            raise TProtocolException(TProtocolException.INVALID_DATA,
                                     "Bad data encounted in numeric data")

    def readJSONDouble(self) -> float:
        self.context.read()
        if self.reader.peek() == QUOTE:
            string = self.readJSONString(True)
            try:
                double = float(string)
                if (self.context.escapeNum is False and  # type: ignore[comparison-overlap]
                        not math.isinf(double) and
                        not math.isnan(double)):
                    raise TProtocolException(
                        TProtocolException.INVALID_DATA,
                        "Numeric data unexpectedly quoted")
                return double
            except ValueError:
                raise TProtocolException(TProtocolException.INVALID_DATA,
                                         "Bad data encounted in numeric data")
        else:
            if self.context.escapeNum() is True:
                self.readJSONSyntaxChar(QUOTE)
            try:
                return float(self.readJSONNumericChars())
            except ValueError:
                raise TProtocolException(TProtocolException.INVALID_DATA,
                                         "Bad data encounted in numeric data")

    def readJSONBase64(self) -> bytes:
        string = self.readJSONString(False)
        size = len(string)
        m = size % 4
        # Force padding since b64encode method does not allow it
        if m != 0:
            for i in range(4 - m):
                string += '='
        return base64.b64decode(string)

    def readJSONObjectStart(self) -> None:
        self.context.read()
        self.readJSONSyntaxChar(LBRACE)
        self.pushContext(JSONPairContext(self))

    def readJSONObjectEnd(self) -> None:
        self.readJSONSyntaxChar(RBRACE)
        self.popContext()

    def readJSONArrayStart(self) -> None:
        self.context.read()
        self.readJSONSyntaxChar(LBRACKET)
        self.pushContext(JSONListContext(self))

    def readJSONArrayEnd(self) -> None:
        self.readJSONSyntaxChar(RBRACKET)
        self.popContext()


class TJSONProtocol(TJSONProtocolBase):

    def readMessageBegin(self) -> tuple[str, int, int]:
        self.resetReadContext()
        self.readJSONArrayStart()
        if self.readJSONInteger() != VERSION:
            raise TProtocolException(TProtocolException.BAD_VERSION,
                                     "Message contained bad version.")
        name = self.readJSONString(False)
        typen = self.readJSONInteger()
        seqid = self.readJSONInteger()
        return (name, typen, seqid)

    def readMessageEnd(self) -> None:
        self.readJSONArrayEnd()

    def readStructBegin(self) -> str | None:
        self.readJSONObjectStart()
        return None

    def readStructEnd(self) -> None:
        self.readJSONObjectEnd()

    def readFieldBegin(self) -> tuple[str | None, int, int]:
        character = self.reader.peek()
        ttype = 0
        id = 0
        if character == RBRACE:
            ttype = TType.STOP
        else:
            id = self.readJSONInteger()
            self.readJSONObjectStart()
            ttype = JTYPES[self.readJSONString(False)]
        return (None, ttype, id)

    def readFieldEnd(self) -> None:
        self.readJSONObjectEnd()

    def readMapBegin(self) -> tuple[int, int, int]:
        self.readJSONArrayStart()
        keyType = JTYPES[self.readJSONString(False)]
        valueType = JTYPES[self.readJSONString(False)]
        size = self.readJSONInteger()
        self.readJSONObjectStart()
        return (keyType, valueType, size)

    def readMapEnd(self) -> None:
        self.readJSONObjectEnd()
        self.readJSONArrayEnd()

    def readCollectionBegin(self) -> tuple[int, int]:
        self.readJSONArrayStart()
        elemType = JTYPES[self.readJSONString(False)]
        size = self.readJSONInteger()
        return (elemType, size)

    readListBegin = readCollectionBegin
    readSetBegin = readCollectionBegin

    def readCollectionEnd(self) -> None:
        self.readJSONArrayEnd()

    readSetEnd = readCollectionEnd
    readListEnd = readCollectionEnd

    def readBool(self) -> bool:
        return False if self.readJSONInteger() == 0 else True

    def readNumber(self) -> int:
        return self.readJSONInteger()

    readByte = readNumber
    readI16 = readNumber
    readI32 = readNumber
    readI64 = readNumber

    def readDouble(self) -> float:
        return self.readJSONDouble()

    def readString(self) -> str:
        return self.readJSONString(False)

    def readBinary(self) -> bytes:
        return self.readJSONBase64()

    def writeMessageBegin(self, name: str, ttype: int, seqid: int) -> None:
        self.resetWriteContext()
        self.writeJSONArrayStart()
        self.writeJSONNumber(VERSION)
        self.writeJSONString(name)
        self.writeJSONNumber(ttype)
        self.writeJSONNumber(seqid)

    def writeMessageEnd(self) -> None:
        self.writeJSONArrayEnd()

    def writeStructBegin(self, name: str) -> None:
        self.writeJSONObjectStart()

    def writeStructEnd(self) -> None:
        self.writeJSONObjectEnd()

    def writeFieldBegin(self, name: str, ttype: int, fid: int) -> None:
        self.writeJSONNumber(fid)
        self.writeJSONObjectStart()
        self.writeJSONString(CTYPES[ttype])

    def writeFieldEnd(self) -> None:
        self.writeJSONObjectEnd()

    def writeFieldStop(self) -> None:
        pass

    def writeMapBegin(self, ktype: int, vtype: int, size: int) -> None:
        self.writeJSONArrayStart()
        self.writeJSONString(CTYPES[ktype])
        self.writeJSONString(CTYPES[vtype])
        self.writeJSONNumber(size)
        self.writeJSONObjectStart()

    def writeMapEnd(self) -> None:
        self.writeJSONObjectEnd()
        self.writeJSONArrayEnd()

    def writeListBegin(self, etype: int, size: int) -> None:
        self.writeJSONArrayStart()
        self.writeJSONString(CTYPES[etype])
        self.writeJSONNumber(size)

    def writeListEnd(self) -> None:
        self.writeJSONArrayEnd()

    def writeSetBegin(self, etype: int, size: int) -> None:
        self.writeJSONArrayStart()
        self.writeJSONString(CTYPES[etype])
        self.writeJSONNumber(size)

    def writeSetEnd(self) -> None:
        self.writeJSONArrayEnd()

    def writeBool(self, bool_val: bool) -> None:
        self.writeJSONNumber(1 if bool_val is True else 0)

    def writeByte(self, byte: int) -> None:
        checkIntegerLimits(byte, 8)
        self.writeJSONNumber(byte)

    def writeI16(self, i16: int) -> None:
        checkIntegerLimits(i16, 16)
        self.writeJSONNumber(i16)

    def writeI32(self, i32: int) -> None:
        checkIntegerLimits(i32, 32)
        self.writeJSONNumber(i32)

    def writeI64(self, i64: int) -> None:
        checkIntegerLimits(i64, 64)
        self.writeJSONNumber(i64)

    def writeDouble(self, dub: float) -> None:
        # 17 significant digits should be just enough for any double precision
        # value.
        self.writeJSONNumber(dub, '{0:.17g}')

    def writeString(self, str_val: str) -> None:
        self.writeJSONString(str_val)

    def writeBinary(self, str_val: bytes) -> None:
        self.writeJSONBase64(str_val)


class TJSONProtocolFactory(TProtocolFactory):
    def getProtocol(self, trans: TTransportBase) -> TJSONProtocol:
        return TJSONProtocol(trans)

    @property
    def string_length_limit(self) -> None:
        return None

    @property
    def container_length_limit(self) -> None:
        return None


class TSimpleJSONProtocol(TJSONProtocolBase):
    """Simple, readable, write-only JSON protocol.

    Useful for interacting with scripting languages.
    """

    def readMessageBegin(self) -> tuple[str, int, int]:
        raise NotImplementedError()

    def readMessageEnd(self) -> None:
        raise NotImplementedError()

    def readStructBegin(self) -> str | None:
        raise NotImplementedError()

    def readStructEnd(self) -> None:
        raise NotImplementedError()

    def writeMessageBegin(self, name: str, ttype: int, seqid: int) -> None:
        self.resetWriteContext()

    def writeMessageEnd(self) -> None:
        pass

    def writeStructBegin(self, name: str) -> None:
        self.writeJSONObjectStart()

    def writeStructEnd(self) -> None:
        self.writeJSONObjectEnd()

    def writeFieldBegin(self, name: str, ttype: int, fid: int) -> None:
        self.writeJSONString(name)

    def writeFieldEnd(self) -> None:
        pass

    def writeMapBegin(self, ktype: int, vtype: int, size: int) -> None:
        self.writeJSONObjectStart()

    def writeMapEnd(self) -> None:
        self.writeJSONObjectEnd()

    def _writeCollectionBegin(self, etype: int, size: int) -> None:
        self.writeJSONArrayStart()

    def _writeCollectionEnd(self) -> None:
        self.writeJSONArrayEnd()

    writeListBegin = _writeCollectionBegin
    writeListEnd = _writeCollectionEnd
    writeSetBegin = _writeCollectionBegin
    writeSetEnd = _writeCollectionEnd

    def writeByte(self, byte: int) -> None:
        checkIntegerLimits(byte, 8)
        self.writeJSONNumber(byte)

    def writeI16(self, i16: int) -> None:
        checkIntegerLimits(i16, 16)
        self.writeJSONNumber(i16)

    def writeI32(self, i32: int) -> None:
        checkIntegerLimits(i32, 32)
        self.writeJSONNumber(i32)

    def writeI64(self, i64: int) -> None:
        checkIntegerLimits(i64, 64)
        self.writeJSONNumber(i64)

    def writeBool(self, bool_val: bool) -> None:
        self.writeJSONNumber(1 if bool_val is True else 0)

    def writeDouble(self, dub: float) -> None:
        self.writeJSONNumber(dub)

    def writeString(self, str_val: str) -> None:
        self.writeJSONString(str_val)

    def writeBinary(self, str_val: bytes) -> None:
        self.writeJSONBase64(str_val)


class TSimpleJSONProtocolFactory(TProtocolFactory):

    def getProtocol(self, trans: TTransportBase) -> TSimpleJSONProtocol:
        return TSimpleJSONProtocol(trans)
