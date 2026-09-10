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
# TNonblockingServer reads a frame's four-byte length itself and then collects
# that many bytes before it hands the frame to a worker. It refused a negative
# length but had no maximum, so a connection went on buffering whatever arrived
# for as long as the declared length allowed. These tests hold it to the frame
# size limit that TFramedTransport and THeaderTransport apply.
#

import os
import socket
import struct
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.server import TNonblockingServer  # noqa
from thrift.server.TNonblockingServer import CLOSED, WAIT_PROCESS, Connection  # noqa
from thrift.transport import TSocket  # noqa
from thrift.transport.TTransport import DEFAULT_MAX_FRAME_SIZE, HARD_MAX_FRAME_SIZE  # noqa


def frame(payload):
    return struct.pack('!i', len(payload)) + payload


class ConnectionFrameSizeTest(unittest.TestCase):

    def connection(self, *args):
        ours, peer = socket.socketpair()
        self.addCleanup(peer.close)
        self.addCleanup(ours.close)
        return Connection(ours, lambda: None, *args), peer

    def test_a_frame_over_the_default_maximum_closes_the_connection(self):
        conn, peer = self.connection()
        peer.sendall(struct.pack('!i', DEFAULT_MAX_FRAME_SIZE + 1) + b'x' * 100)
        conn.read()
        self.assertEqual(conn.status, CLOSED)
        self.assertFalse(conn.received)

    def test_a_frame_over_a_given_maximum_closes_the_connection(self):
        conn, peer = self.connection(100)
        peer.sendall(struct.pack('!i', 101) + b'x' * 101)
        conn.read()
        self.assertEqual(conn.status, CLOSED)
        self.assertFalse(conn.received)

    def test_a_frame_at_the_maximum_is_received(self):
        conn, peer = self.connection(100)
        peer.sendall(frame(b'y' * 100))
        conn.read()
        self.assertEqual(conn.status, WAIT_PROCESS)
        self.assertEqual([message.len for message in conn.received], [100])

    def test_a_frame_within_the_default_maximum_is_received(self):
        conn, peer = self.connection()
        peer.sendall(frame(b'z' * 13))
        conn.read()
        self.assertEqual(conn.status, WAIT_PROCESS)
        self.assertEqual([message.len for message in conn.received], [13])

    def test_a_negative_frame_size_still_closes_the_connection(self):
        conn, peer = self.connection()
        peer.sendall(struct.pack('!i', -1))
        conn.read()
        self.assertEqual(conn.status, CLOSED)


class ServerFrameSizeTest(unittest.TestCase):

    def test_the_maximum_is_validated(self):
        for bad in (0, -1, HARD_MAX_FRAME_SIZE + 1):
            with self.assertRaises(ValueError):
                TNonblockingServer.TNonblockingServer(
                    None, TSocket.TServerSocket('127.0.0.1', 0), max_frame_size=bad)

    def test_the_server_applies_its_maximum_to_accepted_connections(self):
        lsocket = TSocket.TServerSocket('127.0.0.1', 0)
        server = TNonblockingServer.TNonblockingServer(
            object(), lsocket, threads=1, max_frame_size=100)
        server.prepare()
        self.addCleanup(server.close)
        client = socket.create_connection(('127.0.0.1', lsocket.handle.getsockname()[1]))
        self.addCleanup(client.close)
        server.handle()  # accepts
        client.sendall(struct.pack('!i', 101) + b'x' * 101)
        server.handle()  # reads the length and refuses it
        self.assertEqual([c.is_closed() for c in server.clients.values()], [True])


if __name__ == '__main__':
    unittest.main()
