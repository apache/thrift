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

import asyncio
from io import BytesIO
import logging
from struct import unpack

from thrift.Thrift import TType, TApplicationException
from thrift.protocol.TBinaryProtocol import TBinaryProtocol, \
    TBinaryProtocolFactory
from thrift.protocol.TProtocol import TProtocolException
from thrift.transport import TTransport


class TAsyncioTransport(TTransport.TTransportBase):
    """a framed, buffered transport over an asyncio stream"""
    def __init__(self, reader, writer):
        self._reader = reader
        self._writer = writer
        self._wbuf = BytesIO()
        self._logger = logging.getLogger("TAsyncioTransport")

    # not the same number of parameters as TTransportBase.open
    @classmethod
    @asyncio.coroutine
    def connect(cls, host, port, loop=None):
        reader, writer = yield from asyncio.open_connection(host, port, loop=loop)
        return cls(reader, writer)

    @asyncio.coroutine
    def readAll(self, n):
        data = yield from self._reader.readexactly(n)
        return data

    def write(self, buf):
        self._wbuf.write(buf)

    @asyncio.coroutine
    def flush(self, callback=None):
        wbuf = self._wbuf
        size = wbuf.tell()

        if self._logger.isEnabledFor(logging.DEBUG):
            self._logger.debug('writing frame of size %d: %s', size,
                               wbuf.getvalue()[:size])
        if size < 1024:
            data = wbuf.getvalue()[:size]
        else:
            data = memoryview(wbuf.getvalue())[:size]
        self._writer.write(data)
        wbuf.seek(0)
        yield from self._writer.drain()

    def close(self):
        self._reader.close()
        self._writer.close()


class TAsyncioBinaryProtocol(TBinaryProtocol):

    """Binary implementation of the Thrift protocol driver for asyncio."""

    _fast_encode = None
    _fast_decode = None

    @asyncio.coroutine
    def readMessageBegin(self):
        sz = yield from self.readI32()
        if sz < 0:
            version = sz & TBinaryProtocol.VERSION_MASK
            if version != TBinaryProtocol.VERSION_1:
                raise TProtocolException(
                    type=TProtocolException.BAD_VERSION,
                    message='Bad version in readMessageBegin: %d' % (sz))
            type = sz & TBinaryProtocol.TYPE_MASK
            name = yield from self.readString()
            seqid = yield from self.readI32()
        else:
            if self.strictRead:
                raise TProtocolException(type=TProtocolException.BAD_VERSION,
                                         message='No protocol version header')
            name = yield from self.trans.readAll(sz)
            type = yield from self.readByte()
            seqid = yield from self.readI32()
        return (name, type, seqid)

    @asyncio.coroutine
    def readMessageEnd(self):
        pass

    @asyncio.coroutine
    def readStructBegin(self):
        pass

    @asyncio.coroutine
    def readStructEnd(self):
        pass

    @asyncio.coroutine
    def readFieldBegin(self):
        type = yield from self.readByte()
        if type == TType.STOP:
            return (None, type, 0)
        id = yield from self.readI16()
        return (None, type, id)

    @asyncio.coroutine
    def readFieldEnd(self):
        pass

    @asyncio.coroutine
    def readMapBegin(self):
        ktype = yield from self.readByte()
        vtype = yield from self.readByte()
        size = yield from self.readI32()
        self._check_container_length(size)
        return (ktype, vtype, size)

    @asyncio.coroutine
    def readMapEnd(self):
        pass

    @asyncio.coroutine
    def readListBegin(self):
        etype = yield from self.readByte()
        size = yield from self.readI32()
        self._check_container_length(size)
        return (etype, size)

    @asyncio.coroutine
    def readListEnd(self):
        pass

    @asyncio.coroutine
    def readSetBegin(self):
        etype = yield from self.readByte()
        size = yield from self.readI32()
        self._check_container_length(size)
        return (etype, size)

    @asyncio.coroutine
    def readSetEnd(self):
        pass

    @asyncio.coroutine
    def readBool(self):
        byte = yield from self.readByte()
        if byte == 0:
            return False
        return True

    @asyncio.coroutine
    def readByte(self):
        buff = yield from self.trans.readAll(1)
        val, = unpack('!b', buff)
        return val

    @asyncio.coroutine
    def readI16(self):
        buff = yield from self.trans.readAll(2)
        val, = unpack('!h', buff)
        return val

    @asyncio.coroutine
    def readI32(self):
        buff = yield from self.trans.readAll(4)
        val, = unpack('!i', buff)
        return val

    @asyncio.coroutine
    def readI64(self):
        buff = yield from self.trans.readAll(8)
        val, = unpack('!q', buff)
        return val

    @asyncio.coroutine
    def readDouble(self):
        buff = yield from self.trans.readAll(8)
        val, = unpack('!d', buff)
        return val

    @asyncio.coroutine
    def readBinary(self):
        size = yield from self.readI32()
        self._check_string_length(size)
        s = yield from self.trans.readAll(size)
        return s

    @asyncio.coroutine
    def readString(self):
        return (yield from self.readBinary()).decode("utf-8")

    @asyncio.coroutine
    def skip(self, type):
        if type == TType.STOP:
            return
        elif type == TType.BOOL:
            yield from self.readBool()
        elif type == TType.BYTE:
            yield from self.readByte()
        elif type == TType.I16:
            yield from self.readI16()
        elif type == TType.I32:
            yield from self.readI32()
        elif type == TType.I64:
            yield from self.readI64()
        elif type == TType.DOUBLE:
            yield from self.readDouble()
        elif type == TType.FLOAT:
            yield from self.readFloat()
        elif type == TType.STRING:
            yield from self.readString()
        elif type == TType.STRUCT:
            name = yield from self.readStructBegin()
            while True:
                (name, type, id) = yield from self.readFieldBegin()
                if type == TType.STOP:
                    break
                yield from self.skip(type)
                yield from self.readFieldEnd()
            yield from self.readStructEnd()
        elif type == TType.MAP:
            (ktype, vtype, size) = yield from self.readMapBegin()
            for i in range(size):
                yield from self.skip(ktype)
                yield from self.skip(vtype)
            yield from self.readMapEnd()
        elif type == TType.SET:
            (etype, size) = yield from self.readSetBegin()
            for i in range(size):
                yield from self.skip(etype)
            yield from self.readSetEnd()
        elif type == TType.LIST:
            (etype, size) = yield from self.readListBegin()
            for i in range(size):
                yield from self.skip(etype)
            yield from self.readListEnd()


class TAsyncioBinaryProtocolFactory(TBinaryProtocolFactory):
    def getProtocol(self, trans):
        return TAsyncioBinaryProtocol(trans, self.strictRead, self.strictWrite)


class TAsyncioApplicationException(TApplicationException):
    @asyncio.coroutine
    def read(self, iprot):
        yield from iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = yield from iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.STRING:
                    message = yield from iprot.readString()
                    if sys.version_info.major >= 3 and isinstance(
                            message, bytes):
                        try:
                            message = message.decode('utf-8')
                        except UnicodeDecodeError:
                            pass
                    self.message = message
                else:
                    yield from iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.type = yield from iprot.readI32()
                else:
                    yield from iprot.skip(ftype)
            else:
                yield from iprot.skip(ftype)
            yield from iprot.readFieldEnd()
        yield from iprot.readStructEnd()
