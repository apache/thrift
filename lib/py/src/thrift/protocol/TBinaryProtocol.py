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
from typing import Any

from thrift.protocol.TProtocol import TProtocolBase, TProtocolException, TProtocolFactory
from thrift.Thrift import TType
from thrift.transport.TTransport import TTransportBase


class TBinaryProtocol(TProtocolBase):
    """Binary implementation of the Thrift protocol driver."""

    # NastyHaxx. Python 2.4+ on 32-bit machines forces hex constants to be
    # positive, converting this into a long. If we hardcode the int value
    # instead it'll stay in 32 bit-land.

    # VERSION_MASK = 0xffff0000
    VERSION_MASK: int = -65536

    # VERSION_1 = 0x80010000
    VERSION_1: int = -2147418112

    TYPE_MASK: int = 0x000000ff

    strictRead: bool
    strictWrite: bool
    string_length_limit: int | None
    container_length_limit: int | None

    def __init__(
        self,
        trans: TTransportBase,
        strictRead: bool = False,
        strictWrite: bool = True,
        **kwargs: Any,
    ) -> None:
        TProtocolBase.__init__(self, trans)
        self.strictRead = strictRead
        self.strictWrite = strictWrite
        self.string_length_limit = kwargs.get('string_length_limit', None)
        self.container_length_limit = kwargs.get('container_length_limit', None)

    def _check_string_length(self, length: int) -> None:
        self._check_length(self.string_length_limit, length)

    def _check_container_length(self, length: int) -> None:
        self._check_length(self.container_length_limit, length)

    def writeMessageBegin(self, name: str, ttype: int, seqid: int) -> None:
        if self.strictWrite:
            self.writeI32(TBinaryProtocol.VERSION_1 | ttype)
            self.writeString(name)
            self.writeI32(seqid)
        else:
            self.writeString(name)
            self.writeByte(ttype)
            self.writeI32(seqid)

    def writeMessageEnd(self) -> None:
        pass

    def writeStructBegin(self, name: str) -> None:
        pass

    def writeStructEnd(self) -> None:
        pass

    def writeFieldBegin(self, name: str, ttype: int, fid: int) -> None:
        self.writeByte(ttype)
        self.writeI16(fid)

    def writeFieldEnd(self) -> None:
        pass

    def writeFieldStop(self) -> None:
        self.writeByte(TType.STOP)

    def writeMapBegin(self, ktype: int, vtype: int, size: int) -> None:
        self.writeByte(ktype)
        self.writeByte(vtype)
        self.writeI32(size)

    def writeMapEnd(self) -> None:
        pass

    def writeListBegin(self, etype: int, size: int) -> None:
        self.writeByte(etype)
        self.writeI32(size)

    def writeListEnd(self) -> None:
        pass

    def writeSetBegin(self, etype: int, size: int) -> None:
        self.writeByte(etype)
        self.writeI32(size)

    def writeSetEnd(self) -> None:
        pass

    def writeBool(self, bool_val: bool) -> None:
        if bool_val:
            self.writeByte(1)
        else:
            self.writeByte(0)

    def writeByte(self, byte: int) -> None:
        buff = pack("!b", byte)
        self.trans.write(buff)

    def writeI16(self, i16: int) -> None:
        buff = pack("!h", i16)
        self.trans.write(buff)

    def writeI32(self, i32: int) -> None:
        buff = pack("!i", i32)
        self.trans.write(buff)

    def writeI64(self, i64: int) -> None:
        buff = pack("!q", i64)
        self.trans.write(buff)

    def writeDouble(self, dub: float) -> None:
        buff = pack("!d", dub)
        self.trans.write(buff)

    def writeBinary(self, str_val: bytes) -> None:
        self.writeI32(len(str_val))
        self.trans.write(str_val)

    def readMessageBegin(self) -> tuple[str, int, int]:
        sz = self.readI32()
        if sz < 0:
            version = sz & TBinaryProtocol.VERSION_MASK
            if version != TBinaryProtocol.VERSION_1:
                raise TProtocolException(
                    type=TProtocolException.BAD_VERSION,
                    message='Bad version in readMessageBegin: %d' % (sz))
            type = sz & TBinaryProtocol.TYPE_MASK
            name = self.readString()
            seqid = self.readI32()
        else:
            if self.strictRead:
                raise TProtocolException(type=TProtocolException.BAD_VERSION,
                                         message='No protocol version header')
            name = self.trans.readAll(sz).decode('utf-8')
            type = self.readByte()
            seqid = self.readI32()
        return (name, type, seqid)

    def readMessageEnd(self) -> None:
        pass

    def readStructBegin(self) -> str | None:
        pass

    def readStructEnd(self) -> None:
        pass

    def readFieldBegin(self) -> tuple[str | None, int, int]:
        type = self.readByte()
        if type == TType.STOP:
            return (None, type, 0)
        id = self.readI16()
        return (None, type, id)

    def readFieldEnd(self) -> None:
        pass

    def readMapBegin(self) -> tuple[int, int, int]:
        ktype = self.readByte()
        vtype = self.readByte()
        size = self.readI32()
        self._check_container_length(size)
        return (ktype, vtype, size)

    def readMapEnd(self) -> None:
        pass

    def readListBegin(self) -> tuple[int, int]:
        etype = self.readByte()
        size = self.readI32()
        self._check_container_length(size)
        return (etype, size)

    def readListEnd(self) -> None:
        pass

    def readSetBegin(self) -> tuple[int, int]:
        etype = self.readByte()
        size = self.readI32()
        self._check_container_length(size)
        return (etype, size)

    def readSetEnd(self) -> None:
        pass

    def readBool(self) -> bool:
        byte = self.readByte()
        if byte == 0:
            return False
        return True

    def readByte(self) -> int:
        buff = self.trans.readAll(1)
        val, = unpack('!b', buff)
        return val

    def readI16(self) -> int:
        buff = self.trans.readAll(2)
        val, = unpack('!h', buff)
        return val

    def readI32(self) -> int:
        buff = self.trans.readAll(4)
        val, = unpack('!i', buff)
        return val

    def readI64(self) -> int:
        buff = self.trans.readAll(8)
        val, = unpack('!q', buff)
        return val

    def readDouble(self) -> float:
        buff = self.trans.readAll(8)
        val, = unpack('!d', buff)
        return val

    def readBinary(self) -> bytes:
        size = self.readI32()
        self._check_string_length(size)
        s = self.trans.readAll(size)
        return s


class TBinaryProtocolFactory(TProtocolFactory):
    strictRead: bool
    strictWrite: bool
    string_length_limit: int | None
    container_length_limit: int | None

    def __init__(
        self,
        strictRead: bool = False,
        strictWrite: bool = True,
        **kwargs: Any,
    ) -> None:
        self.strictRead = strictRead
        self.strictWrite = strictWrite
        self.string_length_limit = kwargs.get('string_length_limit', None)
        self.container_length_limit = kwargs.get('container_length_limit', None)

    def getProtocol(self, trans: TTransportBase) -> TBinaryProtocol:
        prot = TBinaryProtocol(trans, self.strictRead, self.strictWrite,
                               string_length_limit=self.string_length_limit,
                               container_length_limit=self.container_length_limit)
        return prot


class TBinaryProtocolAccelerated(TBinaryProtocol):
    """C-Accelerated version of TBinaryProtocol.

    This class does not override any of TBinaryProtocol's methods,
    but the generated code recognizes it directly and will call into
    our C module to do the encoding, bypassing this object entirely.
    We inherit from TBinaryProtocol so that the normal TBinaryProtocol
    encoding can happen if the fastbinary module doesn't work for some
    reason.  (TODO(dreiss): Make this happen sanely in more cases.)
    To disable this behavior, pass fallback=False constructor argument.

    In order to take advantage of the C module, just use
    TBinaryProtocolAccelerated instead of TBinaryProtocol.

    NOTE:  This code was contributed by an external developer.
           The internal Thrift team has reviewed and tested it,
           but we cannot guarantee that it is production-ready.
           Please feel free to report bugs and/or success stories
           to the public mailing list.
    """

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        fallback = kwargs.pop('fallback', True)
        super(TBinaryProtocolAccelerated, self).__init__(*args, **kwargs)
        try:
            from thrift.protocol import fastbinary  # type: ignore[attr-defined]
        except ImportError:
            if not fallback:
                raise
        else:
            self._fast_decode = fastbinary.decode_binary
            self._fast_encode = fastbinary.encode_binary


class TBinaryProtocolAcceleratedFactory(TProtocolFactory):
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

    def getProtocol(self, trans: TTransportBase) -> TBinaryProtocolAccelerated:
        return TBinaryProtocolAccelerated(
            trans,
            string_length_limit=self.string_length_limit,
            container_length_limit=self.container_length_limit,
            fallback=self._fallback)
