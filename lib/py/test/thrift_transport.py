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

import struct
import unittest
import zlib
import os

import _import_local_thrift  # noqa
from thrift.transport import TTransport
from thrift.transport.THeaderTransport import (
    THeaderTransport, THeaderClientType, THeaderTransformID, HEADER_MAGIC,
)
from thrift.transport.TTransport import TTransportException


class TestTFileObjectTransport(unittest.TestCase):

    def test_TFileObjectTransport(self):
        test_dir = os.path.dirname(os.path.abspath(__file__))
        datatxt_path = os.path.join(test_dir, 'data.txt')
        buffer = '{"soft":"thrift","version":0.13,"1":true}'
        with open(datatxt_path, "w+") as f:
            buf = TTransport.TFileObjectTransport(f)
            buf.write(buffer)
            buf.flush()
            buf.close()

        with open(datatxt_path, "rb") as f:
            buf = TTransport.TFileObjectTransport(f)
            value = buf.read(len(buffer)).decode('utf-8')
            self.assertEqual(buffer, value)
            buf.close()
        os.remove(datatxt_path)


class TestMemoryBuffer(unittest.TestCase):

    def test_memorybuffer_write(self):
        data = '{"1":[1,"hello"],"a":{"A":"abc"},"bool":true,"num":12345}'

        buffer_w = TTransport.TMemoryBuffer()
        buffer_w.write(data.encode('utf-8'))
        value = buffer_w.getvalue()
        self.assertEqual(value.decode('utf-8'), data)
        buffer_w.close()

    def test_memorybuffer_read(self):
        data = '{"1":[1, "hello"],"a":{"A":"abc"},"bool":true,"num":12345}'

        buffer_r = TTransport.TMemoryBuffer(data.encode('utf-8'))
        value_r = buffer_r.read(len(data))
        value = buffer_r.getvalue()
        self.assertEqual(value.decode('utf-8'), data)
        self.assertEqual(value_r.decode('utf-8'), data)
        buffer_r.close()


def _craft_zlib_header_frame(plain_payload):
    """Return a complete THeaderTransport frame with a ZLIB-compressed payload."""
    compressed = zlib.compress(plain_payload)
    # proto_id=0, 1 transform; values <128 encode as single varint bytes
    hdr_content = bytes([0, 1, THeaderTransformID.ZLIB])
    padding = (4 - len(hdr_content) % 4) % 4
    hdr_bytes = hdr_content + b'\x00' * padding
    header_len_u16 = len(hdr_bytes) // 4
    # 10 fixed bytes: 2(magic)+2(flags)+4(seqid)+2(hdr_len_u16)
    frame_size = 10 + len(hdr_bytes) + len(compressed)
    frame = struct.pack('!i', frame_size)
    frame += struct.pack('!H', HEADER_MAGIC)
    frame += struct.pack('!H', 0)   # flags
    frame += struct.pack('!i', 0)   # sequence_id
    frame += struct.pack('!H', header_len_u16)
    frame += hdr_bytes
    frame += compressed
    return frame


class TestTHeaderTransportDecompressionLimit(unittest.TestCase):

    def _make_transport(self, payload, max_decompressed_size=None):
        frame = _craft_zlib_header_frame(payload)
        inner = TTransport.TMemoryBuffer(frame)
        transport = THeaderTransport(inner, [THeaderClientType.HEADERS])
        if max_decompressed_size is not None:
            transport.set_max_decompressed_size(max_decompressed_size)
        return transport

    def test_zlib_roundtrip(self):
        payload = b'Hello Thrift! ' * 100
        t = self._make_transport(payload)
        t.readFrame(1)
        self.assertEqual(t._read_buffer.read(), payload)

    def test_decompression_bomb_blocked(self):
        # 10,000 bytes decompressed; limit set to 1,000
        payload = b'A' * 10000
        t = self._make_transport(payload, max_decompressed_size=1000)
        with self.assertRaises(TTransportException) as ctx:
            t.readFrame(1)
        self.assertEqual(ctx.exception.type, TTransportException.SIZE_LIMIT)

    def test_payload_at_limit_passes(self):
        payload = b'B' * 5000
        t = self._make_transport(payload, max_decompressed_size=5000)
        t.readFrame(1)
        self.assertEqual(t._read_buffer.read(), payload)

    def test_set_max_decompressed_size_rejects_invalid(self):
        inner = TTransport.TMemoryBuffer(b'')
        t = THeaderTransport(inner, [THeaderClientType.HEADERS])
        with self.assertRaises(ValueError):
            t.set_max_decompressed_size(0)
        with self.assertRaises(ValueError):
            t.set_max_decompressed_size(-1)


if __name__ == '__main__':
    unittest.main()
