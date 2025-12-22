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

from abc import ABC, abstractmethod
from io import BytesIO
from struct import pack, unpack
from typing import TYPE_CHECKING, Any, BinaryIO

from thrift.Thrift import TException

if TYPE_CHECKING:
    from puresasl.client import SASLClient  # type: ignore[import-not-found]


class TTransportException(TException):
    """Custom Transport Exception class"""

    UNKNOWN: int = 0
    NOT_OPEN: int = 1
    ALREADY_OPEN: int = 2
    TIMED_OUT: int = 3
    END_OF_FILE: int = 4
    NEGATIVE_SIZE: int = 5
    SIZE_LIMIT: int = 6
    INVALID_CLIENT_TYPE: int = 7

    type: int
    inner: Exception | None

    def __init__(
        self,
        type: int = UNKNOWN,
        message: str | None = None,
        inner: Exception | None = None,
    ) -> None:
        TException.__init__(self, message)
        self.type = type
        self.inner = inner


class TTransportBase(ABC):
    """Base class for Thrift transport layer."""

    @abstractmethod
    def isOpen(self) -> bool:
        pass

    def open(self) -> None:
        pass

    def close(self) -> None:
        pass

    @abstractmethod
    def read(self, sz: int) -> bytes:
        pass

    def readAll(self, sz: int) -> bytes:
        buff = b''
        have = 0
        while have < sz:
            chunk = self.read(sz - have)
            chunkLen = len(chunk)
            have += chunkLen
            buff += chunk

            if chunkLen == 0:
                raise EOFError()

        return buff

    def write(self, buf: bytes) -> None:
        pass

    def flush(self) -> None:
        pass


class CReadableTransport(ABC):
    """Base class for transports that are readable from C"""

    @property
    @abstractmethod
    def cstringio_buf(self) -> BytesIO:
        """A cStringIO buffer that contains the current chunk we are reading."""
        pass

    @abstractmethod
    def cstringio_refill(self, partialread: bytes, reqlen: int) -> BytesIO:
        """Refills cstringio_buf.

        Returns the currently used buffer (which can but need not be the same as
        the old cstringio_buf). partialread is what the C code has read from the
        buffer, and should be inserted into the buffer before any more reads.  The
        return value must be a new, not borrowed reference.  Something along the
        lines of self._buf should be fine.

        If reqlen bytes can't be read, throw EOFError.
        """
        pass


class TServerTransportBase:
    """Base class for Thrift server transports."""

    def listen(self) -> None:
        pass

    def accept(self) -> TTransportBase | None:
        pass

    def close(self) -> None:
        pass


class TTransportFactoryBase:
    """Base class for a Transport Factory"""

    def getTransport(self, trans: TTransportBase) -> TTransportBase:
        return trans


class TBufferedTransportFactory:
    """Factory transport that builds buffered transports"""

    def getTransport(self, trans: TTransportBase) -> TBufferedTransport:
        buffered = TBufferedTransport(trans)
        return buffered


class TBufferedTransport(TTransportBase, CReadableTransport):
    """Class that wraps another transport and buffers its I/O.

    The implementation uses a (configurable) fixed-size read buffer
    but buffers all writes until a flush is performed.
    """

    DEFAULT_BUFFER: int = 4096

    __trans: TTransportBase
    __wbuf: BytesIO
    __rbuf: BytesIO
    __rbuf_size: int

    def __init__(self, trans: TTransportBase, rbuf_size: int = DEFAULT_BUFFER) -> None:
        self.__trans = trans
        self.__wbuf = BytesIO()
        # Pass string argument to initialize read buffer as cStringIO.InputType
        self.__rbuf = BytesIO(b'')
        self.__rbuf_size = rbuf_size

    def isOpen(self) -> bool:
        return self.__trans.isOpen()

    def open(self) -> None:
        return self.__trans.open()

    def close(self) -> None:
        return self.__trans.close()

    def read(self, sz: int) -> bytes:
        ret = self.__rbuf.read(sz)
        if len(ret) != 0:
            return ret
        self.__rbuf = BytesIO(self.__trans.read(max(sz, self.__rbuf_size)))
        return self.__rbuf.read(sz)

    def write(self, buf: bytes) -> None:
        try:
            self.__wbuf.write(buf)
        except Exception as e:
            # on exception reset wbuf so it doesn't contain a partial function call
            self.__wbuf = BytesIO()
            raise e

    def flush(self) -> None:
        out = self.__wbuf.getvalue()
        # reset wbuf before write/flush to preserve state on underlying failure
        self.__wbuf = BytesIO()
        self.__trans.write(out)
        self.__trans.flush()

    # Implement the CReadableTransport interface.
    @property
    def cstringio_buf(self) -> BytesIO:
        return self.__rbuf

    def cstringio_refill(self, partialread: bytes, reqlen: int) -> BytesIO:
        retstring = partialread
        if reqlen < self.__rbuf_size:
            # try to make a read of as much as we can.
            retstring += self.__trans.read(self.__rbuf_size)

        # but make sure we do read reqlen bytes.
        if len(retstring) < reqlen:
            retstring += self.__trans.readAll(reqlen - len(retstring))

        self.__rbuf = BytesIO(retstring)
        return self.__rbuf


class TMemoryBuffer(TTransportBase, CReadableTransport):
    """Wraps a cBytesIO object as a TTransport.

    NOTE: Unlike the C++ version of this class, you cannot write to it
          then immediately read from it.  If you want to read from a
          TMemoryBuffer, you must either pass a string to the constructor.
    TODO(dreiss): Make this work like the C++ version.
    """

    _buffer: BytesIO

    def __init__(self, value: bytes | None = None, offset: int = 0) -> None:
        """value -- a value to read from for stringio

        If value is set, this will be a transport for reading,
        otherwise, it is for writing"""
        if value is not None:
            self._buffer = BytesIO(value)
        else:
            self._buffer = BytesIO()
        if offset:
            self._buffer.seek(offset)

    def isOpen(self) -> bool:
        return not self._buffer.closed

    def open(self) -> None:
        pass

    def close(self) -> None:
        self._buffer.close()

    def read(self, sz: int) -> bytes:
        return self._buffer.read(sz)

    def write(self, buf: bytes) -> None:
        self._buffer.write(buf)

    def flush(self) -> None:
        pass

    def getvalue(self) -> bytes:
        return self._buffer.getvalue()

    # Implement the CReadableTransport interface.
    @property
    def cstringio_buf(self) -> BytesIO:
        return self._buffer

    def cstringio_refill(self, partialread: bytes, reqlen: int) -> BytesIO:
        # only one shot at reading...
        raise EOFError()


class TFramedTransportFactory:
    """Factory transport that builds framed transports"""

    def getTransport(self, trans: TTransportBase) -> TFramedTransport:
        framed = TFramedTransport(trans)
        return framed


class TFramedTransport(TTransportBase, CReadableTransport):
    """Class that wraps another transport and frames its I/O when writing."""

    __trans: TTransportBase
    __rbuf: BytesIO
    __wbuf: BytesIO

    def __init__(self, trans: TTransportBase) -> None:
        self.__trans = trans
        self.__rbuf = BytesIO(b'')
        self.__wbuf = BytesIO()

    def isOpen(self) -> bool:
        return self.__trans.isOpen()

    def open(self) -> None:
        return self.__trans.open()

    def close(self) -> None:
        return self.__trans.close()

    def read(self, sz: int) -> bytes:
        ret = self.__rbuf.read(sz)
        if len(ret) != 0:
            return ret

        self.readFrame()
        return self.__rbuf.read(sz)

    def readFrame(self) -> None:
        buff = self.__trans.readAll(4)
        (sz,) = unpack('!i', buff)
        self.__rbuf = BytesIO(self.__trans.readAll(sz))

    def write(self, buf: bytes) -> None:
        self.__wbuf.write(buf)

    def flush(self) -> None:
        wout = self.__wbuf.getvalue()
        wsz = len(wout)
        # reset wbuf before write/flush to preserve state on underlying failure
        self.__wbuf = BytesIO()
        # N.B.: Doing this string concatenation is WAY cheaper than making
        # two separate calls to the underlying socket object. Socket writes in
        # Python turn out to be REALLY expensive, but it seems to do a pretty
        # good job of managing string buffer operations without excessive copies
        buf = pack("!i", wsz) + wout
        self.__trans.write(buf)
        self.__trans.flush()

    # Implement the CReadableTransport interface.
    @property
    def cstringio_buf(self) -> BytesIO:
        return self.__rbuf

    def cstringio_refill(self, partialread: bytes, reqlen: int) -> BytesIO:
        # self.__rbuf will already be empty here because fastbinary doesn't
        # ask for a refill until the previous buffer is empty.  Therefore,
        # we can start reading new frames immediately.
        while len(partialread) < reqlen:
            self.readFrame()
            partialread += self.__rbuf.getvalue()
        self.__rbuf = BytesIO(partialread)
        return self.__rbuf


class TFileObjectTransport(TTransportBase):
    """Wraps a file-like object to make it work as a Thrift transport."""

    fileobj: BinaryIO

    def __init__(self, fileobj: BinaryIO) -> None:
        self.fileobj = fileobj

    def isOpen(self) -> bool:
        return True

    def close(self) -> None:
        self.fileobj.close()

    def read(self, sz: int) -> bytes:
        return self.fileobj.read(sz)

    def write(self, buf: bytes) -> None:
        self.fileobj.write(buf)

    def flush(self) -> None:
        self.fileobj.flush()


class TSaslClientTransport(TTransportBase, CReadableTransport):
    """
    SASL transport
    """

    START: int = 1
    OK: int = 2
    BAD: int = 3
    ERROR: int = 4
    COMPLETE: int = 5

    transport: TTransportBase
    sasl: SASLClient
    __wbuf: BytesIO
    __rbuf: BytesIO

    def __init__(
        self,
        transport: TTransportBase,
        host: str,
        service: str,
        mechanism: str = 'GSSAPI',
        **sasl_kwargs: Any,
    ) -> None:
        """
        transport: an underlying transport to use, typically just a TSocket
        host: the name of the server, from a SASL perspective
        service: the name of the server's service, from a SASL perspective
        mechanism: the name of the preferred mechanism to use

        All other kwargs will be passed to the puresasl.client.SASLClient
        constructor.
        """

        from puresasl.client import SASLClient  # type: ignore[import-not-found]

        self.transport = transport
        self.sasl = SASLClient(host, service, mechanism, **sasl_kwargs)

        self.__wbuf = BytesIO()
        self.__rbuf = BytesIO(b'')

    def open(self) -> None:
        if not self.transport.isOpen():
            self.transport.open()

        mechanism = self.sasl.mechanism
        assert mechanism is not None, "SASL mechanism must be set"
        self.send_sasl_msg(self.START, bytes(mechanism, 'ascii'))
        self.send_sasl_msg(self.OK, self.sasl.process())

        while True:
            status, challenge = self.recv_sasl_msg()
            if status == self.OK:
                self.send_sasl_msg(self.OK, self.sasl.process(challenge))
            elif status == self.COMPLETE:
                if not self.sasl.complete:
                    raise TTransportException(
                        TTransportException.NOT_OPEN,
                        "The server erroneously indicated "
                        "that SASL negotiation was complete",
                    )
                else:
                    break
            else:
                raise TTransportException(
                    TTransportException.NOT_OPEN,
                    "Bad SASL negotiation status: %d (%s)" % (status, challenge),
                )

    def isOpen(self) -> bool:
        return self.transport.isOpen()

    def send_sasl_msg(self, status: int, body: bytes) -> None:
        header = pack(">BI", status, len(body))
        self.transport.write(header + body)
        self.transport.flush()

    def recv_sasl_msg(self) -> tuple[int, bytes | str]:
        header = self.transport.readAll(5)
        status, length = unpack(">BI", header)
        if length > 0:
            payload: bytes | str = self.transport.readAll(length)
        else:
            payload = ""
        return status, payload

    def write(self, buf: bytes) -> None:
        self.__wbuf.write(buf)

    def flush(self) -> None:
        data = self.__wbuf.getvalue()
        encoded = self.sasl.wrap(data)
        self.transport.write(pack("!i", len(encoded)) + encoded)
        self.transport.flush()
        self.__wbuf = BytesIO()

    def read(self, sz: int) -> bytes:
        ret = self.__rbuf.read(sz)
        if len(ret) != 0:
            return ret

        self._read_frame()
        return self.__rbuf.read(sz)

    def _read_frame(self) -> None:
        header = self.transport.readAll(4)
        (length,) = unpack('!i', header)
        encoded = self.transport.readAll(length)
        self.__rbuf = BytesIO(self.sasl.unwrap(encoded))

    def close(self) -> None:
        self.sasl.dispose()
        self.transport.close()

    # based on TFramedTransport
    @property
    def cstringio_buf(self) -> BytesIO:
        return self.__rbuf

    def cstringio_refill(self, partialread: bytes, reqlen: int) -> BytesIO:
        # self.__rbuf will already be empty here because fastbinary doesn't
        # ask for a refill until the previous buffer is empty.  Therefore,
        # we can start reading new frames immediately.
        while len(partialread) < reqlen:
            self._read_frame()
            partialread += self.__rbuf.getvalue()
        self.__rbuf = BytesIO(partialread)
        return self.__rbuf
