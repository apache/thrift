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
# The protocols in TTwisted frame their messages with Twisted's
# Int32StringReceiver, which closes a connection whose length prefix is over
# MAX_LENGTH. TTwisted set that to 2**31 - 1 on all three protocols, so a frame
# was collected for as long as its declared length allowed. These tests hold
# them to the frame size limit TFramedTransport applies.
#

import os
import struct
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.transport.TTransport import DEFAULT_MAX_FRAME_SIZE  # noqa

try:
    from twisted.internet.testing import StringTransport
    from thrift.transport import TTwisted
except ImportError:  # Twisted is an optional extra
    TTwisted = None


def header(length):
    return struct.pack('!i', length)


class Client(object):
    def __init__(self, transport, oprot_factory):
        self._reqs = {}


@unittest.skipIf(TTwisted is None, 'Twisted is not installed')
class TwistedFrameSizeTest(unittest.TestCase):

    def connect(self, protocol):
        transport = StringTransport()
        protocol.makeConnection(transport)
        return transport

    def server(self):
        frames = []

        class RecordingServerProtocol(TTwisted.ThriftServerProtocol):
            def stringReceived(self, frame):
                frames.append(frame)

        protocol = RecordingServerProtocol()
        return protocol, self.connect(protocol), frames

    def test_the_server_closes_a_connection_whose_frame_is_over_the_maximum(self):
        protocol, transport, frames = self.server()
        protocol.dataReceived(header(DEFAULT_MAX_FRAME_SIZE + 1) + b'x' * 65536)
        self.assertTrue(transport.disconnecting)
        self.assertEqual(frames, [])

    def test_the_server_waits_for_a_frame_at_the_maximum(self):
        protocol, transport, frames = self.server()
        protocol.dataReceived(header(DEFAULT_MAX_FRAME_SIZE) + b'x' * 65536)
        self.assertFalse(transport.disconnecting)
        self.assertEqual(frames, [])

    def test_the_server_still_receives_an_ordinary_frame(self):
        protocol, transport, frames = self.server()
        protocol.dataReceived(header(5) + b'hello')
        self.assertFalse(transport.disconnecting)
        self.assertEqual(frames, [b'hello'])

    def test_the_client_closes_a_connection_whose_frame_is_over_the_maximum(self):
        protocol = TTwisted.ThriftClientProtocol(Client, None)
        transport = self.connect(protocol)
        protocol.dataReceived(header(DEFAULT_MAX_FRAME_SIZE + 1) + b'x' * 65536)
        self.assertTrue(transport.disconnecting)

    def test_every_protocol_is_held_to_the_maximum(self):
        for protocol in (TTwisted.ThriftServerProtocol,
                         TTwisted.ThriftClientProtocol,
                         TTwisted.ThriftSASLClientProtocol):
            self.assertEqual(protocol.MAX_LENGTH, DEFAULT_MAX_FRAME_SIZE, protocol.__name__)


if __name__ == '__main__':
    unittest.main()
