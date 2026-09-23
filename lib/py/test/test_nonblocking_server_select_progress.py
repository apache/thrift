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
# TNonblockingServer drives its own select/poll loop. Once it has read and
# dispatched a request it has to wait on its sockets again before the next
# turn, so the listening socket and every other connection keep being
# serviced. These tests hold it to that: after a request has been answered the
# server stays idle until there is something to do, a further client is still
# accepted and served, and a second frame that arrived in the same read is
# still processed straight from the buffer.
#

import os
import socket
import struct
import sys
import threading
import time
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.server import TNonblockingServer  # noqa
from thrift.transport import TSocket  # noqa


def frame(payload):
    return struct.pack('!i', len(payload)) + payload


class FixedResponseProcessor(object):
    """Answers every request with the same small frame, ignoring its body."""

    def process(self, iprot, oprot):
        oprot.trans.write(b'ok')


class SelectProgressTest(unittest.TestCase):

    IDLE_WINDOW = 1.0
    SERVE_TIMEOUT = 2.0

    def start_server(self):
        """Start a server on an ephemeral port in its own thread, return port."""
        lsocket = TSocket.TServerSocket('127.0.0.1', 0)
        server = TNonblockingServer.TNonblockingServer(
            FixedResponseProcessor(), lsocket, threads=1)
        server.prepare()
        port = lsocket.handle.getsockname()[1]
        thread = threading.Thread(target=server.serve)
        thread.daemon = True
        thread.start()

        def stop():
            server.stop()
            thread.join(self.IDLE_WINDOW + self.SERVE_TIMEOUT + 10)
            server.close()
            # A server left spinning would keep a thread alive and taint the
            # idle measurement of the next test.
            self.assertFalse(thread.is_alive(), 'server thread did not stop')
        self.addCleanup(stop)
        return port

    def connect(self, port):
        client = socket.create_connection(('127.0.0.1', port), self.SERVE_TIMEOUT)
        client.settimeout(self.SERVE_TIMEOUT)
        self.addCleanup(client.close)
        return client

    def _recv_all(self, client, n):
        data = b''
        while len(data) < n:
            chunk = client.recv(n - len(data))
            if not chunk:
                return None
            data += chunk
        return data

    def read_frame(self, client):
        """Read one framed response, or None if it does not arrive in time."""
        try:
            header = self._recv_all(client, 4)
            if header is None:
                return None
            length, = struct.unpack('!i', header)
            return self._recv_all(client, length)
        except socket.timeout:
            return None

    def exchange(self, port, payload):
        """Send one request and return its response payload (None on timeout)."""
        client = self.connect(port)
        client.sendall(frame(payload))
        return self.read_frame(client)

    def idle_cpu_over_window(self):
        """CPU seconds the process burns while it is meant to be waiting."""
        before = os.times()
        time.sleep(self.IDLE_WINDOW)
        after = os.times()
        return (after.user - before.user) + (after.system - before.system)

    def assert_idle_then_serves(self, payload):
        port = self.start_server()

        # A first request is answered ...
        self.assertEqual(self.exchange(port, payload), b'ok')

        # ... after which the server has nothing to do and must not keep a CPU
        # busy while it waits for the next event.
        idle_cpu = self.idle_cpu_over_window()

        # A further client is still accepted and served.
        served = self.exchange(port, b'ping')

        # Primary, portable signal: the second client got its answer.
        self.assertEqual(served, b'ok')
        # Secondary signal, with generous slack so a loaded CI host cannot flake.
        self.assertLess(idle_cpu, 0.5 * self.IDLE_WINDOW)

    def test_a_frame_on_the_read_buffer_boundary_leaves_the_server_idle(self):
        # Connection.read() pulls from the socket in 8192-byte chunks; a 4-byte
        # length prefix plus an 8188-byte payload makes a recv land exactly on
        # that chunk boundary. This literal tracks read()'s chunk size: change
        # that size and the payload has to follow, or this stops being a
        # boundary case and merely repeats the small-frame test.
        self.assert_idle_then_serves(b'x' * 8188)

    def test_a_small_frame_leaves_the_server_idle(self):
        self.assert_idle_then_serves(b'x' * 100)

    def test_two_frames_delivered_together_are_both_processed(self):
        port = self.start_server()
        client = self.connect(port)
        client.sendall(frame(b'one') + frame(b'two'))
        first = self.read_frame(client)
        second = self.read_frame(client)
        self.assertEqual((first, second), (b'ok', b'ok'))


if __name__ == '__main__':
    unittest.main()
