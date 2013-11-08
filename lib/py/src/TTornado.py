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

from cStringIO import StringIO
import logging
import socket
import struct

from thrift.transport import TTransport
from thrift.transport.TTransport import TTransportException

from tornado import gen
from tornado import iostream
from tornado import tcpserver


class TTornadoStreamTransport(TTransport.TTransportBase):
    """a framed, buffered transport over a Tornado stream"""
    def __init__(self, host, port, stream=None):
        self.host = host
        self.port = port
        self.is_queuing_reads = False
        self.read_queue = []
        self.__wbuf = StringIO()

        # servers provide a ready-to-go stream
        self.stream = stream
        if self.stream is not None:
            self._set_close_callback()

    # not the same number of parameters as TTransportBase.open
    def open(self, callback):
        logging.debug('socket connecting')
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
        self.stream = iostream.IOStream(sock)

        def on_close_in_connect(*_):
            message = 'could not connect to {}:{}'.format(self.host, self.port)
            raise TTransportException(
                type=TTransportException.NOT_OPEN,
                message=message)
        self.stream.set_close_callback(on_close_in_connect)

        def finish(*_):
            self._set_close_callback()
            callback()

        self.stream.connect((self.host, self.port), callback=finish)

    def _set_close_callback(self):
        def on_close():
            raise TTransportException(
                type=TTransportException.END_OF_FILE,
                message='socket closed')
        self.stream.set_close_callback(self.close)

    def close(self):
        # don't raise if we intend to close
        self.stream.set_close_callback(None)
        self.stream.close()

    def read(self, _):
        # The generated code for Tornado shouldn't do individual reads -- only
        # frames at a time
        assert "you're doing it wrong" is True

    @gen.engine
    def readFrame(self, callback):
        self.read_queue.append(callback)
        logging.debug('read queue: %s', self.read_queue)

        if self.is_queuing_reads:
            # If a read is already in flight, then the while loop below should
            # pull it from self.read_queue
            return

        self.is_queuing_reads = True
        while self.read_queue:
            next_callback = self.read_queue.pop()
            result = yield gen.Task(self._readFrameFromStream)
            next_callback(result)
        self.is_queuing_reads = False

    @gen.engine
    def _readFrameFromStream(self, callback):
        logging.debug('_readFrameFromStream')
        frame_header = yield gen.Task(self.stream.read_bytes, 4)
        frame_length, = struct.unpack('!i', frame_header)
        logging.debug('received frame header, frame length = %i', frame_length)
        frame = yield gen.Task(self.stream.read_bytes, frame_length)
        logging.debug('received frame payload')
        callback(frame)

    def write(self, buf):
        self.__wbuf.write(buf)

    def flush(self, callback=None):
        wout = self.__wbuf.getvalue()
        wsz = len(wout)
        # reset wbuf before write/flush to preserve state on underlying failure
        self.__wbuf = StringIO()
        # N.B.: Doing this string concatenation is WAY cheaper than making
        # two separate calls to the underlying socket object. Socket writes in
        # Python turn out to be REALLY expensive, but it seems to do a pretty
        # good job of managing string buffer operations without excessive copies
        buf = struct.pack("!i", wsz) + wout

        logging.debug('writing frame length = %i', wsz)
        self.stream.write(buf, callback)


class TTornadoServer(tcpserver.TCPServer):
    def __init__(self, processor, iprot_factory, oprot_factory=None,
                 *args, **kwargs):
        super(TTornadoServer, self).__init__(*args, **kwargs)

        self._processor = processor
        self._iprot_factory = iprot_factory
        self._oprot_factory = (oprot_factory if oprot_factory is not None
                               else iprot_factory)

    def handle_stream(self, stream, address):
        try:
            host, port = address
            trans = TTornadoStreamTransport(host=host, port=port, stream=stream)
            oprot = self._oprot_factory.getProtocol(trans)

            def next_pass():
                if not trans.stream.closed():
                    self._processor.process(trans, self._iprot_factory, oprot,
                                            callback=next_pass)

            next_pass()

        except Exception:
            logging.exception('thrift exception in handle_stream')
            trans.close()
