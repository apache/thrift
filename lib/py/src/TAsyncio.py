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
from struct import pack, unpack

from thrift.Thrift import TType, TApplicationException
from thrift.protocol.TBinaryProtocol import TBinaryProtocol, \
    TBinaryProtocolFactory
from thrift.protocol.TCompactProtocol import TCompactProtocol, \
    TCompactProtocolFactory, fromZigZag, reader, CLEAR, FIELD_READ, \
    CONTAINER_READ, VALUE_READ, BOOL_READ, CompactType
from thrift.protocol.TProtocol import TProtocolException
from thrift.transport import TTransport, TZlibTransport


class TAsyncioBaseTransport(TTransport.TTransportBase):
    @asyncio.coroutine
    def readAll(self, sz):
        buff = b''
        while sz:
            chunk = yield from self.read(sz)
            sz -= len(chunk)
            buff += chunk

            if len(chunk) == 0:
                raise EOFError()

        return buff


class TAsyncioTransport(TAsyncioBaseTransport):
    """An abstract transport over asyncio streams"""
    def __init__(self, reader, writer):
        self._reader = reader
        self._writer = writer
        self._wbuf = BytesIO()
        self._logger = logging.getLogger("TAsyncioTransport")

    @classmethod
    @asyncio.coroutine
    def connect(cls, host, port, loop=None, ssl=False):
        reader, writer = yield from asyncio.open_connection(
            host, port, loop=loop, ssl=ssl)
        return cls(reader, writer)

    def write(self, buf):
        try:
            self._wbuf.write(buf)
        except Exception as e:
            # reset wbuf so it doesn't contain a partial function call
            self._wbuf.seek(0)
            raise e from None

    @asyncio.coroutine
    def flush(self):
        wbuf = self._wbuf
        size = wbuf.tell()
        if self._logger.isEnabledFor(logging.DEBUG):
            self._logger.debug('writing %s of size %d: %s',
                               size, 'frame' if self.framed else "data",
                               wbuf.getvalue()[:size])
        data = self._get_flushed_data()
        self._writer.write(data)
        wbuf.seek(0)
        yield from self._writer.drain()

    def close(self):
        self._reader.feed_eof()
        self._writer.close()


class TAsyncioBufferedTransport(TAsyncioTransport):
    """A buffered transport over asyncio streams"""

    @asyncio.coroutine
    def read(self, sz):
        return (yield from self._reader.read(sz))

    def _get_flushed_data(self):
        wbuf = self._wbuf
        size = wbuf.tell()
        if size < 1024:
            return wbuf.getvalue()[:size]
        return memoryview(wbuf.getvalue())[:size]


class TAsyncioFramedTransport(TAsyncioTransport):
    """A buffered transport over an asyncio stream"""
    def __init__(self, reader, writer):
        super(TAsyncioFramedTransport, self).__init__(reader, writer)
        self._rbuf = BytesIO()

    @asyncio.coroutine
    def read(self, sz):
        ret = self._rbuf.read(sz)
        if ret:
            return ret
        yield from self.readFrame()
        return self._rbuf.read(sz)

    @asyncio.coroutine
    def readFrame(self):
        buff = yield from self._reader.readexactly(4)
        sz, = unpack('!i', buff)
        self._rbuf = BytesIO((yield from self._reader.readexactly(sz)))

    def _get_flushed_data(self):
        wbuf = self._wbuf
        size = wbuf.tell()
        return pack("!i", size) + wbuf.getvalue()[:size]


class TAsyncioZlibTransport(TZlibTransport.TZlibTransport,
                            TAsyncioBaseTransport):
    """Class that wraps an asyncio-friendly transport with zlib, compressing
    writes and decompresses reads, using the python standard
    library zlib module.
    """

    @asyncio.coroutine
    def read(self, sz):
        """Read up to sz bytes from the decompressed bytes buffer, and
        read from the underlying transport if the decompression
        buffer is empty.
        """
        ret = self._rbuf.read(sz)
        if len(ret) > 0:
            return ret
        # keep reading from transport until something comes back
        while True:
            if (yield from self.readComp(sz)):
                break
        ret = self._rbuf.read(sz)
        return ret

    @asyncio.coroutine
    def readComp(self, sz):
        """Read compressed data from the underlying transport, then
        decompress it and append it to the internal StringIO read buffer
        """
        zbuf = yield from self._trans.read(sz)
        return self._readComp(zbuf)

    @asyncio.coroutine
    def flush(self):
        """Flush any queued up data in the write buffer and ensure the
        compression buffer is flushed out to the underlying transport
        """
        super(TAsyncioZlibTransport, self).flush()
        # flush() in the base class is effectively a no-op
        yield from self._trans.flush()

    @asyncio.coroutine
    def cstringio_refill(self, partialread, reqlen):
        """Implement the CReadableTransport interface for refill"""
        retstring = partialread
        if reqlen < self.DEFAULT_BUFFSIZE:
            retstring += yield from self.read(self.DEFAULT_BUFFSIZE)
        while len(retstring) < reqlen:
            retstring += yield from self.read(reqlen - len(retstring))
        self._rbuf = BytesIO(retstring)
        return self._rbuf


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


class TAsyncioCompactProtocol(TCompactProtocol):
    """Compact implementation of the Thrift protocol driver with asyncio."""

    @asyncio.coroutine
    def readString(self):
        return (yield from self.readBinary()).decode("utf-8")

    @asyncio.coroutine
    def readFieldBegin(self):
        assert self.state == FIELD_READ, self.state
        type = yield from self.__readUByte()
        if type & 0x0f == TType.STOP:
            return (None, 0, 0)
        delta = type >> 4
        if delta == 0:
            fid = yield from self.__readI16()
        else:
            fid = self._last_fid + delta
        self._last_fid = fid
        type = type & 0x0f
        if type == CompactType.TRUE:
            self.state = BOOL_READ
            self._bool_value = True
        elif type == CompactType.FALSE:
            self.state = BOOL_READ
            self._bool_value = False
        else:
            self.state = VALUE_READ
        return (None, self._getTType(type), fid)

    @asyncio.coroutine
    def readFieldEnd(self):
        super(TAsyncioCompactProtocol, self).readFieldEnd()

    @asyncio.coroutine
    def __readUByte(self):
        result, = unpack('!B', (yield from self.trans.readAll(1)))
        return result

    @asyncio.coroutine
    def __readByte(self):
        result, = unpack('!b', (yield from self.trans.readAll(1)))
        return result

    @asyncio.coroutine
    def __readVarint(self):
        result = 0
        shift = 0
        while True:
            x = yield from self.trans.readAll(1)
            byte = ord(x)
            result |= (byte & 0x7f) << shift
            if byte >> 7 == 0:
                return result
            shift += 7

    @asyncio.coroutine
    def __readZigZag(self):
        return fromZigZag((yield from self.__readVarint()))

    @asyncio.coroutine
    def __readSize(self):
        result = yield from self.__readVarint()
        if result < 0:
            raise TProtocolException("Length < 0")
        return result

    @asyncio.coroutine
    def readMessageBegin(self):
        assert self.state == CLEAR
        proto_id = yield from self.__readUByte()
        if proto_id != self.PROTOCOL_ID:
            raise TProtocolException(TProtocolException.BAD_VERSION,
                                     'Bad protocol id in the message: %d' % proto_id)
        ver_type = yield from self.__readUByte()
        type = (ver_type >> self.TYPE_SHIFT_AMOUNT) & self.TYPE_BITS
        version = ver_type & self.VERSION_MASK
        if version != self.VERSION:
            raise TProtocolException(TProtocolException.BAD_VERSION,
                                     'Bad version: %d (expect %d)' % (version, self.VERSION))
        seqid = yield from self.__readVarint()
        name = (yield from self.__readBinary()).decode("utf-8")
        return (name, type, seqid)

    @asyncio.coroutine
    def readMessageEnd(self):
        super(TAsyncioCompactProtocol, self).readMessageEnd()

    @asyncio.coroutine
    def readStructBegin(self):
        super(TAsyncioCompactProtocol, self).readStructBegin()

    @asyncio.coroutine
    def readStructEnd(self):
        super(TAsyncioCompactProtocol, self).readStructEnd()

    @asyncio.coroutine
    def readCollectionBegin(self):
        assert self.state in (VALUE_READ, CONTAINER_READ), self.state
        size_type = yield from self.__readUByte()
        size = size_type >> 4
        type = self._getTType(size_type)
        if size == 15:
            size = yield from self.__readSize()
        self._check_container_length(size)
        self._containers.append(self.state)
        self.state = CONTAINER_READ
        return type, size
    readSetBegin = readCollectionBegin
    readListBegin = readCollectionBegin

    @asyncio.coroutine
    def readMapBegin(self):
        assert self.state in (VALUE_READ, CONTAINER_READ), self.state
        size = yield from self.__readSize()
        self._check_container_length(size)
        types = 0
        if size > 0:
            types = yield from self.__readUByte()
        vtype = self._getTType(types)
        ktype = self._getTType(types >> 4)
        self._containers.append(self.state)
        self.state = CONTAINER_READ
        return (ktype, vtype, size)

    @asyncio.coroutine
    def readCollectionEnd(self):
        super(TAsyncioCompactProtocol, self).readCollectionEnd()
    readSetEnd = readCollectionEnd
    readListEnd = readCollectionEnd
    readMapEnd = readCollectionEnd

    @asyncio.coroutine
    def readBool(self):
        return super(TAsyncioCompactProtocol, self).readBool()

    readByte = reader(__readByte)
    __readI16 = __readZigZag
    readI16 = reader(__readZigZag)
    readI32 = reader(__readZigZag)
    readI64 = reader(__readZigZag)

    @asyncio.coroutine
    @reader
    def readDouble(self):
        buff = yield from self.trans.readAll(8)
        val, = unpack('<d', buff)
        return val

    @asyncio.coroutine
    def __readBinary(self):
        size = yield from self.__readSize()
        self._check_string_length(size)
        return (yield from self.trans.readAll(size))
    readBinary = reader(__readBinary)


class TAsyncioCompactProtocolFactory(TCompactProtocolFactory):
    def getProtocol(self, trans):
        return TAsyncioCompactProtocol(trans,
                                       self.string_length_limit,
                                       self.container_length_limit)


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
                    if isinstance(message, bytes):
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
