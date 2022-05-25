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

import unittest
import os
import sys

gen_path = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "gen-py"
)
sys.path.append(gen_path)

import _import_local_thrift  # noqa
from thrift.protocol.TBinaryProtocol import TBinaryProtocolFactory
from thrift.protocol.TBinaryProtocol import TBinaryProtocolAcceleratedFactory
from thrift.protocol.TCompactProtocol import TCompactProtocolFactory
from thrift.protocol.TCompactProtocol import TCompactProtocolAcceleratedFactory
from thrift.transport import TTransport
from thrift.TSerialization import serialize, deserialize
from TestServer.ttypes import Message


class TestSerializer(unittest.TestCase):
    def setUp(self):
        self.message = Message("hello thrift", 42)
        self.binary_serialized = b"\x0b\x00\x01\x00\x00\x00\x0chello thrift\n\x00\x02\x00\x00\x00\x00\x00\x00\x00*\x00"
        self.compact_serialized = b'\x18\x0chello thrift\x16T\x00'

    def verify(self, serialized, factory):
        self.assertEqual(serialized, serialize(self.message, factory))

        self.assertEqual(
            "hello thrift",
            deserialize(Message(), serialized, factory).body,
        )
        self.assertEqual(
            42, deserialize(Message(), serialized, factory).num
        )

        self.assertRaises(EOFError, deserialize, Message(), b'', factory)

    def test_TBinaryProtocol(self):
        buf = TTransport.TMemoryBuffer()
        transport = TTransport.TBufferedTransportFactory().getTransport(buf)
        factory = TBinaryProtocolFactory(transport)
        self.verify(self.binary_serialized, factory)

    def test_TBinaryProtocolAccelerated(self):
        buf = TTransport.TMemoryBuffer()
        transport = TTransport.TBufferedTransportFactory().getTransport(buf)
        factory = TBinaryProtocolAcceleratedFactory(transport)
        self.verify(self.binary_serialized, factory)

    def test_TCompactProtocol(self):
        factory = TCompactProtocolFactory()
        self.verify(self.compact_serialized, factory)

    def test_TCompactProtocolAccelerated(self):
        factory = TCompactProtocolAcceleratedFactory()
        self.verify(self.compact_serialized, factory)


if __name__ == "__main__":
    unittest.main()
