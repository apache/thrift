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
"""Implementation of non-blocking server.

The main idea of the server is to receive and send requests
only from the main thread.

The thread pool should be sized for concurrent tasks, not
maximum connections
"""

from __future__ import annotations

import logging
import queue
import select
import socket
import struct
import threading
from collections import deque
from typing import Any, Callable, TYPE_CHECKING

from thrift.transport import TTransport
from thrift.protocol.TBinaryProtocol import TBinaryProtocolFactory

if TYPE_CHECKING:
    from thrift.protocol.TProtocol import TProtocolFactory
    from thrift.Thrift import TProcessor
    from thrift.transport.TSocket import TServerSocket

__all__ = ['TNonblockingServer']

logger: logging.Logger = logging.getLogger(__name__)


class Worker(threading.Thread):
    """Worker is a small helper to process incoming connection."""

    queue: queue.Queue[list[Any]]

    def __init__(self, queue: queue.Queue[list[Any]]) -> None:
        threading.Thread.__init__(self)
        self.queue = queue

    def run(self) -> None:
        """Process queries from task queue, stop if processor is None."""
        while True:
            processor, iprot, oprot, otrans, callback = self.queue.get()
            if processor is None:
                break
            try:
                processor.process(iprot, oprot)
                callback(True, otrans.getvalue())
            except Exception:
                logger.exception("Exception while processing request", exc_info=True)
                callback(False, b'')


WAIT_LEN: int = 0
WAIT_MESSAGE: int = 1
WAIT_PROCESS: int = 2
SEND_ANSWER: int = 3
CLOSED: int = 4


def locked(func: Callable[..., Any]) -> Callable[..., Any]:
    """Decorator which locks self.lock."""
    def nested(self: Any, *args: Any, **kwargs: Any) -> Any:
        self.lock.acquire()
        try:
            return func(self, *args, **kwargs)
        finally:
            self.lock.release()
    return nested


def socket_exception(func: Callable[..., Any]) -> Callable[..., Any]:
    """Decorator close object on socket.error."""
    def read(self: Any, *args: Any, **kwargs: Any) -> Any:
        try:
            return func(self, *args, **kwargs)
        except socket.error:
            logger.debug('ignoring socket exception', exc_info=True)
            self.close()
    return read


class Message:
    """Represents a message being read from or written to a connection."""

    offset: int
    len: int
    buffer: bytes | None
    is_header: bool

    def __init__(self, offset: int, len_: int, header: bool) -> None:
        self.offset = offset
        self.len = len_
        self.buffer = None
        self.is_header = header

    @property
    def end(self) -> int:
        return self.offset + self.len


class Connection:
    """Basic class is represented connection.

    It can be in state:
        WAIT_LEN --- connection is reading request len.
        WAIT_MESSAGE --- connection is reading request.
        WAIT_PROCESS --- connection has just read whole request and
                         waits for call ready routine.
        SEND_ANSWER --- connection is sending answer string (including length
                        of answer).
        CLOSED --- socket was closed and connection should be deleted.
    """

    socket: socket.socket
    status: int
    len: int
    received: deque[Message]
    _reading: Message
    _rbuf: bytes
    _wbuf: bytes
    lock: threading.Lock
    wake_up: Callable[[], None]
    remaining: bool

    def __init__(self, new_socket: socket.socket, wake_up: Callable[[], None]) -> None:
        self.socket = new_socket
        self.socket.setblocking(False)
        self.status = WAIT_LEN
        self.len = 0
        self.received = deque()
        self._reading = Message(0, 4, True)
        self._rbuf = b''
        self._wbuf = b''
        self.lock = threading.Lock()
        self.wake_up = wake_up
        self.remaining = False

    @socket_exception
    def read(self) -> None:
        """Reads data from stream and switch state."""
        assert self.status in (WAIT_LEN, WAIT_MESSAGE)
        assert not self.received
        buf_size = 8192
        first = True
        done = False
        while not done:
            read = self.socket.recv(buf_size)
            rlen = len(read)
            done = rlen < buf_size
            self._rbuf += read
            if first and rlen == 0:
                if self.status != WAIT_LEN or self._rbuf:
                    logger.error('could not read frame from socket')
                else:
                    logger.debug('read zero length. client might have disconnected')
                self.close()
            while len(self._rbuf) >= self._reading.end:
                if self._reading.is_header:
                    mlen, = struct.unpack('!i', self._rbuf[:4])
                    if mlen < 0:
                        logger.error('could not read the head from frame')
                        self.close()
                        break
                    self._reading = Message(self._reading.end, mlen, False)
                    self.status = WAIT_MESSAGE
                else:
                    self._reading.buffer = self._rbuf
                    self.received.append(self._reading)
                    self._rbuf = self._rbuf[self._reading.end:]
                    self._reading = Message(0, 4, True)
            first = False
            if self.received:
                self.status = WAIT_PROCESS
                break
        self.remaining = not done

    @socket_exception
    def write(self) -> None:
        """Writes data from socket and switch state."""
        assert self.status == SEND_ANSWER
        sent = self.socket.send(self._wbuf)
        if sent == len(self._wbuf):
            self.status = WAIT_LEN
            self._wbuf = b''
            self.len = 0
        else:
            self._wbuf = self._wbuf[sent:]

    @locked
    def ready(self, all_ok: bool, message: bytes) -> None:
        """Callback function for switching state and waking up main thread.

        This function is the only function witch can be called asynchronous.

        The ready can switch Connection to three states:
            WAIT_LEN if request was oneway.
            SEND_ANSWER if request was processed in normal way.
            CLOSED if request throws unexpected exception.

        The one wakes up main thread.
        """
        assert self.status == WAIT_PROCESS
        if not all_ok:
            self.close()
            self.wake_up()
            return
        self.len = 0
        if len(message) == 0:
            # it was a oneway request, do not write answer
            self._wbuf = b''
            self.status = WAIT_LEN
        else:
            self._wbuf = struct.pack('!i', len(message)) + message
            self.status = SEND_ANSWER
        self.wake_up()

    @locked
    def is_writeable(self) -> bool:
        """Return True if connection should be added to write list of select"""
        return self.status == SEND_ANSWER

    # it's not necessary, but...
    @locked
    def is_readable(self) -> bool:
        """Return True if connection should be added to read list of select"""
        return self.status in (WAIT_LEN, WAIT_MESSAGE)

    @locked
    def is_closed(self) -> bool:
        """Returns True if connection is closed."""
        return self.status == CLOSED

    def fileno(self) -> int:
        """Returns the file descriptor of the associated socket."""
        return self.socket.fileno()

    def close(self) -> None:
        """Closes connection"""
        self.status = CLOSED
        self.socket.close()


class TNonblockingServer:
    """Non-blocking server."""

    processor: TProcessor
    socket: TServerSocket
    in_protocol: TProtocolFactory
    out_protocol: TProtocolFactory
    threads: int
    clients: dict[int, Connection]
    tasks: queue.Queue[list[Any]]
    _read: socket.socket
    _write: socket.socket
    prepared: bool
    _stop: bool
    poll: select.poll | None

    def __init__(
        self,
        processor: TProcessor,
        lsocket: TServerSocket,
        inputProtocolFactory: TProtocolFactory | None = None,
        outputProtocolFactory: TProtocolFactory | None = None,
        threads: int = 10,
    ) -> None:
        self.processor = processor
        self.socket = lsocket
        self.in_protocol = inputProtocolFactory or TBinaryProtocolFactory()
        self.out_protocol = outputProtocolFactory or self.in_protocol
        self.threads = int(threads)
        self.clients = {}
        self.tasks = queue.Queue()
        self._read, self._write = socket.socketpair()
        self.prepared = False
        self._stop = False
        self.poll = select.poll() if hasattr(select, 'poll') else None

    def setNumThreads(self, num: int) -> None:
        """Set the number of worker threads that should be created."""
        # implement ThreadPool interface
        assert not self.prepared, "Can't change number of threads after start"
        self.threads = num

    def prepare(self) -> None:
        """Prepares server for serve requests."""
        if self.prepared:
            return
        self.socket.listen()
        for _ in range(self.threads):
            thread = Worker(self.tasks)
            thread.daemon = True
            thread.start()
        self.prepared = True

    def wake_up(self) -> None:
        """Wake up main thread.

        The server usually waits in select call in we should terminate one.
        The simplest way is using socketpair.

        Select always wait to read from the first socket of socketpair.

        In this case, we can just write anything to the second socket from
        socketpair.
        """
        self._write.send(b'1')

    def stop(self) -> None:
        """Stop the server.

        This method causes the serve() method to return.  stop() may be invoked
        from within your handler, or from another thread.

        After stop() is called, serve() will return but the server will still
        be listening on the socket.  serve() may then be called again to resume
        processing requests.  Alternatively, close() may be called after
        serve() returns to close the server socket and shutdown all worker
        threads.
        """
        self._stop = True
        self.wake_up()

    def _select(self) -> tuple[list[int], list[int], list[int], bool]:
        """Does select on open connections."""
        readable: list[int] = [self.socket.handle.fileno(), self._read.fileno()]  # type: ignore[union-attr]
        writable: list[int] = []
        remaining: list[int] = []
        for i, connection in list(self.clients.items()):
            if connection.is_readable():
                readable.append(connection.fileno())
                if connection.remaining or connection.received:
                    remaining.append(connection.fileno())
            if connection.is_writeable():
                writable.append(connection.fileno())
            if connection.is_closed():
                del self.clients[i]
        if remaining:
            return remaining, [], [], False
        else:
            return select.select(readable, writable, readable) + (True,)  # type: ignore[return-value]

    def _poll_select(self) -> tuple[list[int], list[int], list[int], bool]:
        """Does poll on open connections, if available."""
        remaining: list[int] = []

        self.poll.register(self.socket.handle.fileno(), select.POLLIN | select.POLLRDNORM)  # type: ignore[union-attr]
        self.poll.register(self._read.fileno(), select.POLLIN | select.POLLRDNORM)  # type: ignore[union-attr]

        for i, connection in list(self.clients.items()):
            if connection.is_readable():
                self.poll.register(connection.fileno(), select.POLLIN | select.POLLRDNORM | select.POLLERR | select.POLLHUP | select.POLLNVAL)  # type: ignore[union-attr]
                if connection.remaining or connection.received:
                    remaining.append(connection.fileno())
            if connection.is_writeable():
                self.poll.register(connection.fileno(), select.POLLOUT | select.POLLWRNORM)  # type: ignore[union-attr]
            if connection.is_closed():
                try:
                    self.poll.unregister(i)  # type: ignore[union-attr]
                except KeyError:
                    logger.debug("KeyError in unregistering connections...")
                del self.clients[i]
        if remaining:
            return remaining, [], [], False

        rlist: list[int] = []
        wlist: list[int] = []
        xlist: list[int] = []
        pollres = self.poll.poll()  # type: ignore[union-attr]
        for fd, event in pollres:
            if event & (select.POLLERR | select.POLLHUP | select.POLLNVAL):
                xlist.append(fd)
            elif event & (select.POLLOUT | select.POLLWRNORM):
                wlist.append(fd)
            elif event & (select.POLLIN | select.POLLRDNORM):
                rlist.append(fd)
            else:  # should be impossible
                logger.debug("reached an impossible state in _poll_select")
                xlist.append(fd)

        return rlist, wlist, xlist, True

    def handle(self) -> None:
        """Handle requests.

        WARNING! You must call prepare() BEFORE calling handle()
        """
        assert self.prepared, "You have to call prepare before handle"
        rset, wset, xset, selected = self._select() if not self.poll else self._poll_select()
        for readable in rset:
            if readable == self._read.fileno():
                # don't care i just need to clean readable flag
                self._read.recv(1024)
            elif readable == self.socket.handle.fileno():
                try:
                    client = self.socket.accept()
                    if client and client.handle:
                        self.clients[client.handle.fileno()] = Connection(client.handle,
                                                                          self.wake_up)
                except socket.error:
                    logger.debug('error while accepting', exc_info=True)
            else:
                connection = self.clients[readable]
                if selected:
                    connection.read()
                if connection.received:
                    connection.status = WAIT_PROCESS
                    if self.poll:
                        self.poll.unregister(connection.fileno())
                    msg = connection.received.popleft()
                    itransport = TTransport.TMemoryBuffer(msg.buffer, msg.offset)
                    otransport = TTransport.TMemoryBuffer()
                    iprot = self.in_protocol.getProtocol(itransport)
                    oprot = self.out_protocol.getProtocol(otransport)
                    self.tasks.put([self.processor, iprot, oprot,
                                    otransport, connection.ready])
        for writeable in wset:
            self.clients[writeable].write()
        for oob in xset:
            self.clients[oob].close()

    def close(self) -> None:
        """Closes the server."""
        for _ in range(self.threads):
            self.tasks.put([None, None, None, None, None])
        self.socket.close()
        self.prepared = False

    def serve(self) -> None:
        """Serve requests.

        Serve requests forever, or until stop() is called.
        """
        self._stop = False
        self.prepare()
        while not self._stop:
            self.handle()
