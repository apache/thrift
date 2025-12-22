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

import logging
import socket
import struct
import warnings
from collections import deque
from contextlib import contextmanager
from io import BytesIO
from typing import Any, Callable, Generator, TYPE_CHECKING

from tornado import gen, iostream, ioloop, tcpserver, concurrent  # type: ignore[import-untyped]

from .transport.TTransport import TTransportException, TTransportBase, TMemoryBuffer

if TYPE_CHECKING:
    from thrift.protocol.TProtocol import TProtocolFactory
    from thrift.Thrift import TProcessor

__all__ = ['TTornadoServer', 'TTornadoStreamTransport']

logger: logging.Logger = logging.getLogger(__name__)


class _Lock:
    """Simple async lock for Tornado coroutines."""

    _waiters: deque[concurrent.Future[None]]

    def __init__(self) -> None:
        self._waiters = deque()

    def acquired(self) -> bool:
        return len(self._waiters) > 0

    @gen.coroutine  # type: ignore[misc]
    def acquire(self) -> Generator[Any, Any, Any]:
        blocker = self._waiters[-1] if self.acquired() else None
        future: concurrent.Future[None] = concurrent.Future()
        self._waiters.append(future)
        if blocker:
            yield blocker

        raise gen.Return(self._lock_context())

    def release(self) -> None:
        assert self.acquired(), 'Lock not aquired'
        future = self._waiters.popleft()
        future.set_result(None)

    @contextmanager
    def _lock_context(self) -> Generator[None, None, None]:
        try:
            yield
        finally:
            self.release()


class TTornadoStreamTransport(TTransportBase):
    """a framed, buffered transport over a Tornado stream"""

    host: str
    port: int
    io_loop: ioloop.IOLoop
    __wbuf: BytesIO
    _read_lock: _Lock
    stream: iostream.IOStream | None

    def __init__(
        self,
        host: str,
        port: int,
        stream: iostream.IOStream | None = None,
        io_loop: ioloop.IOLoop | None = None,
    ) -> None:
        if io_loop is not None:
            warnings.warn(
                "The `io_loop` parameter is deprecated and unused. Passing "
                "`io_loop` is unnecessary because Tornado now automatically "
                "provides the current I/O loop via `IOLoop.current()`. "
                "Remove the `io_loop` parameter to ensure compatibility - it "
                "will be removed in a future release.",
                DeprecationWarning,
                stacklevel=2,
            )
        self.host = host
        self.port = port
        self.io_loop = ioloop.IOLoop.current()
        self.__wbuf = BytesIO()
        self._read_lock = _Lock()

        # servers provide a ready-to-go stream
        self.stream = stream

    def with_timeout(self, timeout: float, future: Any) -> Any:
        return gen.with_timeout(timeout, future)

    def isOpen(self) -> bool:
       if self.stream is None:
           return False
       return not self.stream.closed()

    @gen.coroutine  # type: ignore[misc]
    def open(self, timeout: float | None = None) -> Generator[Any, Any, TTornadoStreamTransport]:
        logger.debug('socket connecting')
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
        self.stream = iostream.IOStream(sock)

        try:
            connect = self.stream.connect((self.host, self.port))
            if timeout is not None:
                yield self.with_timeout(timeout, connect)
            else:
                yield connect
        except (socket.error, IOError, ioloop.TimeoutError) as e:
            message = 'could not connect to {}:{} ({})'.format(self.host, self.port, e)
            raise TTransportException(
                type=TTransportException.NOT_OPEN,
                message=message)

        raise gen.Return(self)

    def set_close_callback(self, callback: Callable[[], None] | None) -> None:
        """
        Should be called only after open() returns
        """
        self.stream.set_close_callback(callback)  # type: ignore[union-attr]

    def close(self) -> None:
        # don't raise if we intend to close
        self.stream.set_close_callback(None)  # type: ignore[union-attr]
        self.stream.close()  # type: ignore[union-attr]

    def read(self, sz: int) -> bytes:
        # The generated code for Tornado shouldn't do individual reads -- only
        # frames at a time
        raise NotImplementedError("Use readFrame() for Tornado transport")

    @contextmanager
    def io_exception_context(self) -> Generator[None, None, None]:
        try:
            yield
        except (socket.error, IOError) as e:
            raise TTransportException(
                type=TTransportException.END_OF_FILE,
                message=str(e))
        except iostream.StreamBufferFullError as e:
            raise TTransportException(
                type=TTransportException.UNKNOWN,
                message=str(e))

    @gen.coroutine  # type: ignore[misc]
    def readFrame(self) -> Generator[Any, Any, bytes]:
        # IOStream processes reads one at a time
        with (yield self._read_lock.acquire()):
            with self.io_exception_context():
                frame_header = yield self.stream.read_bytes(4)
                if len(frame_header) == 0:
                    raise iostream.StreamClosedError('Read zero bytes from stream')
                frame_length, = struct.unpack('!i', frame_header)
                frame = yield self.stream.read_bytes(frame_length)
                raise gen.Return(frame)

    def write(self, buf: bytes) -> None:
        self.__wbuf.write(buf)

    def flush(self) -> Any:
        frame = self.__wbuf.getvalue()
        # reset wbuf before write/flush to preserve state on underlying failure
        frame_length = struct.pack('!i', len(frame))
        self.__wbuf = BytesIO()
        with self.io_exception_context():
            return self.stream.write(frame_length + frame)  # type: ignore[union-attr]


class TTornadoServer(tcpserver.TCPServer):  # type: ignore[misc]
    """Tornado-based Thrift server."""

    _processor: TProcessor
    _iprot_factory: TProtocolFactory
    _oprot_factory: TProtocolFactory

    def __init__(
        self,
        processor: TProcessor,
        iprot_factory: TProtocolFactory,
        oprot_factory: TProtocolFactory | None = None,
        *args: Any,
        **kwargs: Any,
    ) -> None:
        super(TTornadoServer, self).__init__(*args, **kwargs)

        self._processor = processor
        self._iprot_factory = iprot_factory
        self._oprot_factory = (oprot_factory if oprot_factory is not None
                               else iprot_factory)

    @gen.coroutine  # type: ignore[misc]
    def handle_stream(self, stream: iostream.IOStream, address: tuple[str, int]) -> Generator[Any, Any, None]:
        host, port = address[:2]
        trans = TTornadoStreamTransport(host=host, port=port, stream=stream)
        oprot = self._oprot_factory.getProtocol(trans)

        try:
            while not trans.stream.closed():  # type: ignore[union-attr]
                try:
                    frame = yield trans.readFrame()
                except TTransportException as e:
                    if e.type == TTransportException.END_OF_FILE:
                        break
                    else:
                        raise
                tr = TMemoryBuffer(frame)
                iprot = self._iprot_factory.getProtocol(tr)
                yield self._processor.process(iprot, oprot)
        except Exception:
            logger.exception('thrift exception in handle_stream')
            trans.close()

        logger.info('client disconnected %s:%d', host, port)
