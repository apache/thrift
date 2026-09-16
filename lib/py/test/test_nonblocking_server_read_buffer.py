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
# How TNonblockingServer's Connection collects what arrives in pieces: the
# work has to stay proportional to the frame, whatever the pieces, and each
# message has to hold its own frame and nothing after it.
#

import os
import struct
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.server.TNonblockingServer import WAIT_LEN, WAIT_MESSAGE, WAIT_PROCESS, Connection  # noqa
from thrift.transport import TTransport  # noqa


def frame(payload):
    return struct.pack('!i', len(payload)) + payload


class CountingPiece(bytes):
    """A received piece that counts what appending it to a bytes object copies.

    A bytes object cannot grow, so buffer + piece builds a new object out of
    both. Appending to a buffer that can grow is not counted.
    """
    copied = 0

    def __radd__(self, other):
        if type(other) is not bytes:
            return NotImplemented
        CountingPiece.copied += len(other) + len(self)
        return other + bytes(self)


class PieceSocket(object):
    """Hands out one of the given pieces per recv().

    The pieces are shorter than what Connection.read() asks for, so each call
    of read() takes exactly one of them, as it would after one select().
    """

    def __init__(self, pieces):
        self.pieces = list(pieces)

    def setblocking(self, flag):
        pass

    def recv(self, bufsize):
        assert self.pieces, 'read() asked for more than the test sent'
        piece = self.pieces.pop(0)
        assert len(piece) < bufsize
        return piece

    def close(self):
        pass


def pieces_of(data, size):
    return [CountingPiece(data[i:i + size]) for i in range(0, len(data), size)]


def read_message(message):
    """What the server's processing thread reads for a message: the message
    itself, then whatever follows it in the transport it is given."""
    trans = TTransport.TMemoryBuffer(message.buffer, message.offset)
    return trans.read(message.len), trans.read(1 << 20)


class ConnectionReadBufferTest(unittest.TestCase):

    def test_a_frame_in_many_pieces_is_collected_without_copying_it_again_per_piece(self):
        payload = bytes(range(256)) * 4096  # 1 MiB
        data = frame(payload)
        pieces = pieces_of(data, 4096)
        conn = Connection(PieceSocket(pieces), lambda: None)

        CountingPiece.copied = 0
        for _ in pieces:
            self.assertIn(conn.status, (WAIT_LEN, WAIT_MESSAGE))
            conn.read()

        self.assertEqual(conn.status, WAIT_PROCESS)
        # Rebuilding the buffer on every piece copies about
        # len(pieces) * len(data) / 2 bytes, 130 MiB here.
        self.assertLessEqual(CountingPiece.copied, 2 * len(data))
        self.assertEqual(read_message(conn.received.popleft()), (payload, b''))

    def test_each_message_holds_its_own_frame_and_nothing_after_it(self):
        first, second = b'first message', b'the second one'
        conn = Connection(PieceSocket([frame(first) + frame(second)]), lambda: None)
        conn.read()

        self.assertEqual(conn.status, WAIT_PROCESS)
        self.assertEqual([read_message(m) for m in conn.received],
                         [(first, b''), (second, b'')])

    def test_a_length_split_across_reads_is_collected(self):
        payload = b'split length'
        data = frame(payload)
        conn = Connection(PieceSocket([data[:2], data[2:3], data[3:]]), lambda: None)

        conn.read()
        conn.read()
        self.assertEqual(conn.status, WAIT_LEN)
        conn.read()

        self.assertEqual(conn.status, WAIT_PROCESS)
        self.assertEqual(read_message(conn.received.popleft()), (payload, b''))

    def test_what_follows_a_frame_is_kept_for_the_next_one(self):
        first, second = b'first message', b'the second one'
        data = frame(first) + frame(second)
        cut = len(frame(first)) + 3
        conn = Connection(PieceSocket([data[:cut], data[cut:]]), lambda: None)

        conn.read()
        self.assertEqual(conn.status, WAIT_PROCESS)
        self.assertEqual(read_message(conn.received.popleft()), (first, b''))

        conn.ready(True, b'')  # a oneway request: nothing to send back
        conn.read()
        self.assertEqual(conn.status, WAIT_PROCESS)
        self.assertEqual(read_message(conn.received.popleft()), (second, b''))


if __name__ == '__main__':
    unittest.main()
