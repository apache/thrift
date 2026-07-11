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
import uuid

import _import_local_thrift  # noqa
from thrift.Thrift import TMessageType, TType
from thrift.protocol.THeaderProtocol import THeaderProtocol
from thrift.transport.THeaderTransport import (
    THeaderClientType,
    THeaderSubprotocolID,
)
from thrift.transport.TTransport import TMemoryBuffer


class TestTHeaderProtocol(unittest.TestCase):
    def test_uuid_roundtrip(self):
        expected = uuid.UUID('00010203-0405-0607-0809-0a0b0c0d0e0f')

        subprotocols = (THeaderSubprotocolID.BINARY, THeaderSubprotocolID.COMPACT)
        for subprotocol in subprotocols:
            with self.subTest(subprotocol=subprotocol):
                output = TMemoryBuffer()
                writer = THeaderProtocol(output, (THeaderClientType.HEADERS,), subprotocol)
                writer.writeMessageBegin('uuid', TMessageType.CALL, 1)
                writer.writeStructBegin('uuid_args')
                writer.writeFieldBegin('value', TType.UUID, 1)
                writer.writeUuid(expected)
                writer.writeFieldEnd()
                writer.writeFieldStop()
                writer.writeStructEnd()
                writer.writeMessageEnd()
                writer.trans.flush()

                reader = THeaderProtocol(
                    TMemoryBuffer(output.getvalue()),
                    (THeaderClientType.HEADERS,),
                )
                self.assertEqual(
                    reader.readMessageBegin(),
                    ('uuid', TMessageType.CALL, 1),
                )
                reader.readStructBegin()
                self.assertEqual(reader.readFieldBegin(), (None, TType.UUID, 1))
                self.assertEqual(reader.readUuid(), expected)
                reader.readFieldEnd()
                self.assertEqual(reader.readFieldBegin(), (None, TType.STOP, 0))
                reader.readStructEnd()
                reader.readMessageEnd()


if __name__ == '__main__':
    unittest.main()
