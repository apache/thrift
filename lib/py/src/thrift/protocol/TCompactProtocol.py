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

from struct import pack, unpack
from io import BytesIO
from typing import Any, Callable

from thrift.protocol.TProtocol import TProtocolBase, TProtocolException, TProtocolFactory, checkIntegerLimits
from thrift.Thrift import TType
from thrift.transport.TTransport import TTransportBase

__all__ = ['TCompactProtocol', 'TCompactProtocolFactory', 'writeVarint']

CLEAR: int = 0
FIELD_WRITE: int = 1
VALUE_WRITE: int = 2
CONTAINER_WRITE: int = 3
BOOL_WRITE: int = 4
FIELD_READ: int = 5
CONTAINER_READ: int = 6
VALUE_READ: int = 7
BOOL_READ: int = 8


def make_helper(v_from: int, container: int) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    def helper(func: Callable[..., Any]) -> Callable[..., Any]:
        def nested(self: TCompactProtocol, *args: Any, **kwargs: Any) -> Any:
            assert self.state in (v_from, container), (self.state, v_from, container)
            return func(self, *args, **kwargs)
        return nested
    return helper


writer = make_helper(VALUE_WRITE, CONTAINER_WRITE)
reader = make_helper(VALUE_READ, CONTAINER_READ)


def makeZigZag(n: int, bits: int) -> int:
    checkIntegerLimits(n, bits)
    return (n << 1) ^ (n >> (bits - 1))


def fromZigZag(n: int) -> int:
    return (n >> 1) ^ -(n & 1)


def writeVarint(trans: TTransportBase | BytesIO, n: int) -> None:
    assert n >= 0, "Input to TCompactProtocol writeVarint cannot be negative!"
    out = bytearray()
    while True:
        if n & ~0x7f == 0:
            out.append(n)
            break
        else:
            out.append((n & 0xff) | 0x80)
            n = n >> 7
    trans.write(bytes(out))


def readVarint(trans: TTransportBase) -> int:
    result = 0
    shift = 0
    while True:
        x = trans.readAll(1)
        byte = ord(x)
        result |= (byte & 0x7f) << shift
        if byte >> 7 == 0:
            return result
        shift += 7


class CompactType:
    STOP: int = 0x00
    TRUE: int = 0x01
    FALSE: int = 0x02
    BYTE: int = 0x03
    I16: int = 0x04
    I32: int = 0x05
    I64: int = 0x06
    DOUBLE: int = 0x07
    BINARY: int = 0x08
    LIST: int = 0x09
    SET: int = 0x0A
    MAP: int = 0x0B
    STRUCT: int = 0x0C


CTYPES: dict[int, int] = {
    TType.STOP: CompactType.STOP,
    TType.BOOL: CompactType.TRUE,  # used for collection
    TType.BYTE: CompactType.BYTE,
    TType.I16: CompactType.I16,
    TType.I32: CompactType.I32,
    TType.I64: CompactType.I64,
    TType.DOUBLE: CompactType.DOUBLE,
    TType.STRING: CompactType.BINARY,
    TType.STRUCT: CompactType.STRUCT,
    TType.LIST: CompactType.LIST,
    TType.SET: CompactType.SET,
    TType.MAP: CompactType.MAP,
}

TTYPES: dict[int, int] = {v: k for k, v in CTYPES.items()}
TTYPES[CompactType.FALSE] = TType.BOOL


class TCompactProtocol(TProtocolBase):
    """Compact implementation of the Thrift protocol driver."""

    PROTOCOL_ID: int = 0x82
    VERSION: int = 1
    VERSION_MASK: int = 0x1f
    TYPE_MASK: int = 0xe0
    TYPE_BITS: int = 0x07
    TYPE_SHIFT_AMOUNT: int = 5

    state: int
    string_length_limit: int | None
    container_length_limit: int | None
    __last_fid: int
    __bool_fid: int | None
    __bool_value: bool | int | None
    __structs: list[tuple[int, int]]
    __containers: list[int]

    def __init__(
        self,
        trans: TTransportBase,
        string_length_limit: int | None = None,
        container_length_limit: int | None = None,
    ) -> None:
        TProtocolBase.__init__(self, trans)
        self.state = CLEAR
        self.__last_fid = 0
        self.__bool_fid = None
        self.__bool_value = None
        self.__structs = []
        self.__containers = []
        self.string_length_limit = string_length_limit
        self.container_length_limit = container_length_limit

    def _check_string_length(self, length: int) -> None:
        self._check_length(self.string_length_limit, length)

    def _check_container_length(self, length: int) -> None:
        self._check_length(self.container_length_limit, length)

    def __writeVarint(self, n: int) -> None:
        writeVarint(self.trans, n)

    def writeMessageBegin(self, name: str, ttype: int, seqid: int) -> None:
        assert self.state == CLEAR
        self.__writeUByte(self.PROTOCOL_ID)
        self.__writeUByte(self.VERSION | (ttype << self.TYPE_SHIFT_AMOUNT))
        # The sequence id is a signed 32-bit integer but the compact protocol
        # writes this out as a "var int" which is always positive, and attempting
        # to write a negative number results in an infinite loop, so we may
        # need to do some conversion here...
        tseqid = seqid
        if tseqid < 0:
            tseqid = 2147483648 + (2147483648 + tseqid)
        self.__writeVarint(tseqid)
        self.__writeBinary(bytes(name, 'utf-8'))
        self.state = VALUE_WRITE

    def writeMessageEnd(self) -> None:
        assert self.state == VALUE_WRITE
        self.state = CLEAR

    def writeStructBegin(self, name: str) -> None:
        assert self.state in (CLEAR, CONTAINER_WRITE, VALUE_WRITE), self.state
        self.__structs.append((self.state, self.__last_fid))
        self.state = FIELD_WRITE
        self.__last_fid = 0

    def writeStructEnd(self) -> None:
        assert self.state == FIELD_WRITE
        self.state, self.__last_fid = self.__structs.pop()

    def writeFieldStop(self) -> None:
        self.__writeByte(0)

    def __writeFieldHeader(self, type: int, fid: int) -> None:
        delta = fid - self.__last_fid
        if 0 < delta <= 15:
            self.__writeUByte(delta << 4 | type)
        else:
            self.__writeByte(type)
            self.__writeI16(fid)
        self.__last_fid = fid

    def writeFieldBegin(self, name: str, ttype: int, fid: int) -> None:
        assert self.state == FIELD_WRITE, self.state
        if ttype == TType.BOOL:
            self.state = BOOL_WRITE
            self.__bool_fid = fid
        else:
            self.state = VALUE_WRITE
            self.__writeFieldHeader(CTYPES[ttype], fid)

    def writeFieldEnd(self) -> None:
        assert self.state in (VALUE_WRITE, BOOL_WRITE), self.state
        self.state = FIELD_WRITE

    def __writeUByte(self, byte: int) -> None:
        self.trans.write(pack('!B', byte))

    def __writeByte(self, byte: int) -> None:
        self.trans.write(pack('!b', byte))

    def __writeI16(self, i16: int) -> None:
        self.__writeVarint(makeZigZag(i16, 16))

    def __writeSize(self, i32: int) -> None:
        self.__writeVarint(i32)

    def writeCollectionBegin(self, etype: int, size: int) -> None:
        assert self.state in (VALUE_WRITE, CONTAINER_WRITE), self.state
        if size <= 14:
            self.__writeUByte(size << 4 | CTYPES[etype])
        else:
            self.__writeUByte(0xf0 | CTYPES[etype])
            self.__writeSize(size)
        self.__containers.append(self.state)
        self.state = CONTAINER_WRITE

    writeSetBegin = writeCollectionBegin
    writeListBegin = writeCollectionBegin

    def writeMapBegin(self, ktype: int, vtype: int, size: int) -> None:
        assert self.state in (VALUE_WRITE, CONTAINER_WRITE), self.state
        if size == 0:
            self.__writeByte(0)
        else:
            self.__writeSize(size)
            self.__writeUByte(CTYPES[ktype] << 4 | CTYPES[vtype])
        self.__containers.append(self.state)
        self.state = CONTAINER_WRITE

    def writeCollectionEnd(self) -> None:
        assert self.state == CONTAINER_WRITE, self.state
        self.state = self.__containers.pop()

    writeMapEnd = writeCollectionEnd
    writeSetEnd = writeCollectionEnd
    writeListEnd = writeCollectionEnd

    def writeBool(self, bool_val: bool) -> None:
        if self.state == BOOL_WRITE:
            if bool_val:
                ctype = CompactType.TRUE
            else:
                ctype = CompactType.FALSE
            self.__writeFieldHeader(ctype, self.__bool_fid)  # type: ignore[arg-type]
        elif self.state == CONTAINER_WRITE:
            if bool_val:
                self.__writeByte(CompactType.TRUE)
            else:
                self.__writeByte(CompactType.FALSE)
        else:
            raise AssertionError("Invalid state in compact protocol")

    writeByte = writer(__writeByte)
    writeI16 = writer(__writeI16)

    @writer
    def writeI32(self, i32: int) -> None:
        self.__writeVarint(makeZigZag(i32, 32))

    @writer
    def writeI64(self, i64: int) -> None:
        self.__writeVarint(makeZigZag(i64, 64))

    @writer
    def writeDouble(self, dub: float) -> None:
        self.trans.write(pack('<d', dub))

    def __writeBinary(self, s: bytes) -> None:
        self.__writeSize(len(s))
        self.trans.write(s)

    writeBinary = writer(__writeBinary)

    def readFieldBegin(self) -> tuple[str | None, int, int]:
        assert self.state == FIELD_READ, self.state
        type = self.__readUByte()
        if type & 0x0f == TType.STOP:
            return (None, 0, 0)
        delta = type >> 4
        if delta == 0:
            fid = self.__readI16()
        else:
            fid = self.__last_fid + delta
        self.__last_fid = fid
        type = type & 0x0f
        if type == CompactType.TRUE:
            self.state = BOOL_READ
            self.__bool_value = True
        elif type == CompactType.FALSE:
            self.state = BOOL_READ
            self.__bool_value = False
        else:
            self.state = VALUE_READ
        return (None, self.__getTType(type), fid)

    def readFieldEnd(self) -> None:
        assert self.state in (VALUE_READ, BOOL_READ), self.state
        self.state = FIELD_READ

    def __readUByte(self) -> int:
        result, = unpack('!B', self.trans.readAll(1))
        return result

    def __readByte(self) -> int:
        result, = unpack('!b', self.trans.readAll(1))
        return result

    def __readVarint(self) -> int:
        return readVarint(self.trans)

    def __readZigZag(self) -> int:
        return fromZigZag(self.__readVarint())

    def __readSize(self) -> int:
        result = self.__readVarint()
        if result < 0:
            raise TProtocolException(TProtocolException.NEGATIVE_SIZE, "Length < 0")
        return result

    def readMessageBegin(self) -> tuple[str, int, int]:
        assert self.state == CLEAR
        proto_id = self.__readUByte()
        if proto_id != self.PROTOCOL_ID:
            raise TProtocolException(TProtocolException.BAD_VERSION,
                                     'Bad protocol id in the message: %d' % proto_id)
        ver_type = self.__readUByte()
        type = (ver_type >> self.TYPE_SHIFT_AMOUNT) & self.TYPE_BITS
        version = ver_type & self.VERSION_MASK
        if version != self.VERSION:
            raise TProtocolException(TProtocolException.BAD_VERSION,
                                     'Bad version: %d (expect %d)' % (version, self.VERSION))
        seqid = self.__readVarint()
        # the sequence is a compact "var int" which is treaded as unsigned,
        # however the sequence is actually signed...
        if seqid > 2147483647:
            seqid = -2147483648 - (2147483648 - seqid)
        name = self.__readBinary().decode('utf-8')
        return (name, type, seqid)

    def readMessageEnd(self) -> None:
        assert self.state == CLEAR
        assert len(self.__structs) == 0

    def readStructBegin(self) -> str | None:
        assert self.state in (CLEAR, CONTAINER_READ, VALUE_READ), self.state
        self.__structs.append((self.state, self.__last_fid))
        self.state = FIELD_READ
        self.__last_fid = 0
        return None

    def readStructEnd(self) -> None:
        assert self.state == FIELD_READ
        self.state, self.__last_fid = self.__structs.pop()

    def readCollectionBegin(self) -> tuple[int, int]:
        assert self.state in (VALUE_READ, CONTAINER_READ), self.state
        size_type = self.__readUByte()
        size = size_type >> 4
        type = self.__getTType(size_type)
        if size == 15:
            size = self.__readSize()
        self._check_container_length(size)
        self.__containers.append(self.state)
        self.state = CONTAINER_READ
        return type, size

    readSetBegin = readCollectionBegin
    readListBegin = readCollectionBegin

    def readMapBegin(self) -> tuple[int, int, int]:
        assert self.state in (VALUE_READ, CONTAINER_READ), self.state
        size = self.__readSize()
        self._check_container_length(size)
        types = 0
        if size > 0:
            types = self.__readUByte()
        vtype = self.__getTType(types)
        ktype = self.__getTType(types >> 4)
        self.__containers.append(self.state)
        self.state = CONTAINER_READ
        return (ktype, vtype, size)

    def readCollectionEnd(self) -> None:
        assert self.state == CONTAINER_READ, self.state
        self.state = self.__containers.pop()

    readSetEnd = readCollectionEnd
    readListEnd = readCollectionEnd
    readMapEnd = readCollectionEnd

    def readBool(self) -> bool:
        if self.state == BOOL_READ:
            return self.__bool_value == True  # noqa: E712
        elif self.state == CONTAINER_READ:
            return self.__readByte() == CompactType.TRUE
        else:
            raise AssertionError("Invalid state in compact protocol: %d" %
                                 self.state)

    readByte = reader(__readByte)
    __readI16 = __readZigZag
    readI16 = reader(__readZigZag)
    readI32 = reader(__readZigZag)
    readI64 = reader(__readZigZag)

    @reader
    def readDouble(self) -> float:
        buff = self.trans.readAll(8)
        val, = unpack('<d', buff)
        return val

    def __readBinary(self) -> bytes:
        size = self.__readSize()
        self._check_string_length(size)
        return self.trans.readAll(size)

    readBinary = reader(__readBinary)

    def __getTType(self, byte: int) -> int:
        return TTYPES[byte & 0x0f]


class TCompactProtocolFactory(TProtocolFactory):
    string_length_limit: int | None
    container_length_limit: int | None

    def __init__(
        self,
        string_length_limit: int | None = None,
        container_length_limit: int | None = None,
    ) -> None:
        self.string_length_limit = string_length_limit
        self.container_length_limit = container_length_limit

    def getProtocol(self, trans: TTransportBase) -> TCompactProtocol:
        return TCompactProtocol(trans,
                                self.string_length_limit,
                                self.container_length_limit)


class TCompactProtocolAccelerated(TCompactProtocol):
    """C-Accelerated version of TCompactProtocol.

    This class does not override any of TCompactProtocol's methods,
    but the generated code recognizes it directly and will call into
    our C module to do the encoding, bypassing this object entirely.
    We inherit from TCompactProtocol so that the normal TCompactProtocol
    encoding can happen if the fastbinary module doesn't work for some
    reason.
    To disable this behavior, pass fallback=False constructor argument.

    In order to take advantage of the C module, just use
    TCompactProtocolAccelerated instead of TCompactProtocol.
    """

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        fallback = kwargs.pop('fallback', True)
        super(TCompactProtocolAccelerated, self).__init__(*args, **kwargs)
        try:
            from thrift.protocol import fastbinary  # type: ignore[attr-defined]
        except ImportError:
            if not fallback:
                raise
        else:
            self._fast_decode = fastbinary.decode_compact
            self._fast_encode = fastbinary.encode_compact


class TCompactProtocolAcceleratedFactory(TProtocolFactory):
    string_length_limit: int | None
    container_length_limit: int | None
    _fallback: bool

    def __init__(
        self,
        string_length_limit: int | None = None,
        container_length_limit: int | None = None,
        fallback: bool = True,
    ) -> None:
        self.string_length_limit = string_length_limit
        self.container_length_limit = container_length_limit
        self._fallback = fallback

    def getProtocol(self, trans: TTransportBase) -> TCompactProtocolAccelerated:
        return TCompactProtocolAccelerated(
            trans,
            string_length_limit=self.string_length_limit,
            container_length_limit=self.container_length_limit,
            fallback=self._fallback)
