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
# TTornadoStreamTransport reads a four-byte signed length and then asks the
# Tornado stream for exactly that many bytes. TFramedTransport and
# THeaderTransport in this binding refuse a negative frame size, and one over
# DEFAULT_MAX_FRAME_SIZE, before they read the body. These tests hold the
# Tornado transport, and the server built on it, to the same rules.
#

import os
import socket
import struct
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.transport.TTransport import (  # noqa
    DEFAULT_MAX_FRAME_SIZE,
    HARD_MAX_FRAME_SIZE,
    TTransportException,
)

try:
    from tornado import iostream, tcpclient  # noqa
    from tornado.testing import (  # noqa
        AsyncTestCase,
        ExpectLog,
        bind_unused_port,
        gen_test,
    )
    from thrift import TTornado  # noqa
    from thrift.protocol import TBinaryProtocol  # noqa
except ImportError:  # Tornado is an optional dependency of this binding
    TTornado = None
    AsyncTestCase = unittest.TestCase

    def gen_test(*args, **kwargs):
        return lambda test: test


def count_reads(stream):
    """Record every size the transport asks the stream for.

    This is what separates "refused the declaration" from "asked for the whole
    frame and ran out": a refused frame must never be requested.
    """
    requested = []
    read_bytes = stream.read_bytes

    def counted(num_bytes, partial=False):
        requested.append(num_bytes)
        return read_bytes(num_bytes, partial)

    stream.read_bytes = counted
    return requested


@unittest.skipIf(TTornado is None, 'tornado is not installed')
class TornadoTransportFrameSizeTest(AsyncTestCase):

    def transport(self, **kwargs):
        ours, peer = socket.socketpair()
        self.addCleanup(peer.close)
        stream = iostream.IOStream(ours)
        self.addCleanup(stream.close)
        requested = count_reads(stream)
        transport = TTornado.TTornadoStreamTransport(
            'localhost', 0, stream=stream, **kwargs)
        return transport, peer, requested

    @gen_test(timeout=5)
    def test_frame_larger_than_the_maximum_is_refused_before_reading_it(self):
        transport, peer, requested = self.transport(max_frame_size=100)
        peer.sendall(struct.pack('!i', 101) + b'x' * 101)
        with self.assertRaises(TTransportException) as cm:
            yield transport.readFrame()
        self.assertEqual(cm.exception.type, TTransportException.SIZE_LIMIT)
        self.assertEqual(requested, [4])

    @gen_test(timeout=5)
    def test_a_frame_over_the_default_maximum_is_refused(self):
        transport, peer, requested = self.transport()
        peer.sendall(struct.pack('!i', DEFAULT_MAX_FRAME_SIZE + 1))
        peer.close()
        with self.assertRaises(TTransportException) as cm:
            yield transport.readFrame()
        self.assertEqual(cm.exception.type, TTransportException.SIZE_LIMIT)
        self.assertEqual(requested, [4])

    @gen_test(timeout=5)
    def test_negative_frame_size_is_refused(self):
        transport, peer, requested = self.transport()
        peer.sendall(struct.pack('!i', -1) + b'x' * 13)
        with self.assertRaises(TTransportException) as cm:
            yield transport.readFrame()
        self.assertEqual(cm.exception.type, TTransportException.NEGATIVE_SIZE)
        self.assertEqual(requested, [4])

    def test_the_maximum_is_validated(self):
        for bad in (0, -1, HARD_MAX_FRAME_SIZE + 1):
            with self.assertRaises(ValueError):
                TTornado.TTornadoStreamTransport(
                    'localhost', 0, max_frame_size=bad)

    @gen_test(timeout=5)
    def test_a_frame_at_the_maximum_is_read_whole(self):
        transport, peer, requested = self.transport(max_frame_size=100)
        peer.sendall(struct.pack('!i', 100) + b'x' * 100)
        frame = yield transport.readFrame()
        self.assertEqual(frame, b'x' * 100)

    @gen_test(timeout=5)
    def test_a_frame_within_the_default_maximum_still_reads(self):
        transport, peer, requested = self.transport()
        peer.sendall(struct.pack('!i', 13) + b'y' * 13)
        frame = yield transport.readFrame()
        self.assertEqual(frame, b'y' * 13)
        self.assertEqual(requested, [4, 13])

    @gen_test(timeout=5)
    def test_an_empty_frame_is_still_accepted(self):
        transport, peer, requested = self.transport()
        peer.sendall(struct.pack('!i', 0))
        frame = yield transport.readFrame()
        self.assertEqual(frame, b'')


class CountingProcessor(object):
    def __init__(self):
        self.calls = 0

    def process(self, iprot, oprot):
        self.calls += 1


@unittest.skipIf(TTornado is None, 'tornado is not installed')
class TornadoServerFrameSizeTest(AsyncTestCase):

    def setUp(self):
        super(TornadoServerFrameSizeTest, self).setUp()
        self.servers = []

    def tearDown(self):
        # Stop listening before AsyncTestCase closes every descriptor the loop
        # still holds: TCPServer.stop() asserts that its sockets are open.
        for server in self.servers:
            server.stop()
        super(TornadoServerFrameSizeTest, self).tearDown()

    def serve(self, **kwargs):
        processor = CountingProcessor()
        server = TTornado.TTornadoServer(
            processor, TBinaryProtocol.TBinaryProtocolFactory(), **kwargs)
        sock, port = bind_unused_port()
        server.add_sockets([sock])
        self.servers.append(server)
        return processor, port

    @gen_test(timeout=5)
    def test_the_server_refuses_a_frame_over_its_maximum(self):
        processor, port = self.serve(max_frame_size=100)
        stream = yield tcpclient.TCPClient().connect('127.0.0.1', port)
        self.addCleanup(stream.close)
        with ExpectLog('thrift.TTornado', 'thrift exception in handle_stream'):
            yield stream.write(struct.pack('!i', 101) + b'x' * 101)
            rest = yield stream.read_until_close()
        self.assertEqual(rest, b'')
        self.assertEqual(processor.calls, 0)

    @gen_test(timeout=5)
    def test_the_server_refuses_a_frame_over_the_default_maximum(self):
        processor, port = self.serve()
        stream = yield tcpclient.TCPClient().connect('127.0.0.1', port)
        self.addCleanup(stream.close)
        with ExpectLog('thrift.TTornado', 'thrift exception in handle_stream'):
            yield stream.write(struct.pack('!i', DEFAULT_MAX_FRAME_SIZE + 1))
            rest = yield stream.read_until_close()
        self.assertEqual(rest, b'')
        self.assertEqual(processor.calls, 0)


if __name__ == '__main__':
    unittest.main()
