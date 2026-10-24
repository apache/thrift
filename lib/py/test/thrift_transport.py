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
from unittest.mock import Mock

import _import_local_thrift  # noqa
from thrift.transport import TTransport
from thrift.transport.THeaderTransport import (
    THeaderTransport, THeaderClientType, THeaderTransformID, HEADER_MAGIC,
)
from thrift.transport.TTransport import TTransportException


class FragmentedTransport(TTransport.TTransportBase):

    def __init__(self, chunks):
        self.chunks = iter(chunks)
        self.requests = []

    def read(self, sz):
        self.requests.append(sz)
        chunk = next(self.chunks, b'')
        if len(chunk) > sz:
            raise AssertionError('Test chunk exceeds the requested size')
        return chunk


class TestReadAll(unittest.TestCase):

    def test_fragmented_read(self):
        chunks = [b'\x00\xff', b'a', b'bcd']
        transport = FragmentedTransport(chunks)
        self.assertEqual(transport.readAll(6), b''.join(chunks))
        self.assertEqual(transport.requests, [6, 4, 3])

    def test_single_read(self):
        chunk = b'abc'
        transport = FragmentedTransport([chunk])
        result = transport.readAll(3)
        self.assertEqual(result, b'abc')
        self.assertIsInstance(result, bytes)
        self.assertEqual(transport.requests, [3])

    def test_single_read_other_chunk_types(self):
        class ReflectedChunk(bytes):
            def __radd__(self, prefix):
                return prefix + b'xyz'

        for chunk in (bytearray(b'abc'), memoryview(b'abc'), ReflectedChunk(b'abc')):
            with self.subTest(chunk_type=type(chunk)):
                result = FragmentedTransport([chunk]).readAll(3)
                self.assertEqual(result, b'' + chunk)
                self.assertIs(type(result), bytes)

    def test_fragmented_large_reads(self):
        for size in (32 * 1024, 128 * 1024, 512 * 1024):
            payload = bytes(range(256)) * (size // 256)
            for count in (2, 3, 4, 16):
                with self.subTest(size=size, chunks=count):
                    offsets = [size * i // count for i in range(count + 1)]
                    chunks = [payload[start:end]
                              for start, end in zip(offsets, offsets[1:])]
                    transport = FragmentedTransport(chunks)
                    result = transport.readAll(size)
                    self.assertEqual(result, payload)
                    self.assertIs(type(result), bytes)
                    self.assertEqual(transport.requests,
                                     [size - offset for offset in offsets[:-1]])

    def test_fragmented_read_avoids_repeated_prefix_copies(self):
        copied = []

        # A subclass's reflected addition takes priority over the bytes prefix.
        class CountingChunk(bytes):
            def __radd__(self, prefix):
                if type(prefix) is not bytes:
                    return NotImplemented
                copied.append(len(prefix) + len(self))
                return prefix + bytes(self)

        payload = bytes(range(256)) * 4096
        chunks = [CountingChunk(payload[i:i + 4096])
                  for i in range(0, len(payload), 4096)]
        transport = FragmentedTransport(chunks)
        self.assertEqual(transport.readAll(len(payload)), payload)
        # Repeated immutable concatenation copies about 128 MiB for this 1 MiB read.
        self.assertLessEqual(sum(copied), 2 * len(payload))

    def test_nonpositive_size(self):
        for size in (0, -1):
            with self.subTest(size=size):
                transport = FragmentedTransport([])
                self.assertEqual(transport.readAll(size), b'')
                self.assertEqual(transport.requests, [])

    def test_eof(self):
        cases = (([], [4]), ([b'ab'], [4, 2]), ([b'ab', b'', b'cd'], [4, 2]))
        for chunks, requests in cases:
            with self.subTest(chunks=chunks):
                transport = FragmentedTransport(chunks)
                with self.assertRaises(EOFError):
                    transport.readAll(4)
                self.assertEqual(transport.requests, requests)

    def test_read_exception(self):
        error = TTransportException(TTransportException.TIMED_OUT, 'read timeout')
        transport = TTransport.TTransportBase()
        transport.read = Mock(side_effect=[b'ab', error])
        with self.assertRaises(TTransportException) as caught:
            transport.readAll(4)
        self.assertIs(caught.exception, error)
        self.assertEqual(transport.read.call_count, 2)

    def test_leaves_following_data_unread(self):
        transport = FragmentedTransport([b'ab', b'cd'])
        self.assertEqual(transport.readAll(2), b'ab')
        self.assertEqual(transport.readAll(2), b'cd')

    def test_fragmented_frame(self):
        payload = bytes(range(256)) * 256
        chunks = [struct.pack('!i', len(payload))]
        chunks.extend(payload[i:i + 1024] for i in range(0, len(payload), 1024))
        transport = TTransport.TFramedTransport(FragmentedTransport(chunks))
        self.assertEqual(transport.readAll(len(payload)), payload)


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


def _varint(n):
    out = bytearray()
    while True:
        if n & ~0x7f == 0:
            out.append(n)
            return bytes(out)
        out.append((n & 0x7f) | 0x80)
        n >>= 7


def _craft_stacked_zlib_header_frame(plain_payload, n_layers):
    """Return a THeaderTransport frame that lists n_layers ZLIB transforms and
    carries plain_payload deflated once per layer."""
    payload = plain_payload
    for _ in range(n_layers):
        payload = zlib.compress(payload)
    # proto_id=0, then the transform count, then that many ZLIB transform ids.
    hdr_content = _varint(0) + _varint(n_layers) + _varint(THeaderTransformID.ZLIB) * n_layers
    padding = (4 - len(hdr_content) % 4) % 4
    hdr_bytes = hdr_content + b'\x00' * padding
    header_len_u16 = len(hdr_bytes) // 4
    frame_size = 10 + len(hdr_bytes) + len(payload)
    frame = struct.pack('!i', frame_size)
    frame += struct.pack('!H', HEADER_MAGIC)
    frame += struct.pack('!H', 0)   # flags
    frame += struct.pack('!i', 0)   # sequence_id
    frame += struct.pack('!H', header_len_u16)
    frame += hdr_bytes
    frame += payload
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


class TestTHeaderTransportTransformCountLimit(unittest.TestCase):

    def _counting_transport(self, frame):
        inner = TTransport.TMemoryBuffer(frame)
        transport = THeaderTransport(inner, [THeaderClientType.HEADERS])
        layers = []
        original = transport._apply_read_transform

        def counting(transform_id, payload):
            layers.append(transform_id)
            return original(transform_id, payload)

        transport._apply_read_transform = counting
        return transport, layers

    def test_single_layer_round_trips(self):
        payload = b'payload-' * 128
        frame = _craft_stacked_zlib_header_frame(payload, 1)
        transport, layers = self._counting_transport(frame)
        transport.readFrame(1)
        self.assertEqual(transport._read_buffer.read(), payload)
        self.assertEqual(len(layers), 1)

    def test_stacked_transforms_refused(self):
        payload = b'payload-' * 128
        frame = _craft_stacked_zlib_header_frame(payload, 1500)
        transport, layers = self._counting_transport(frame)
        with self.assertRaises(TTransportException) as ctx:
            transport.readFrame(1)
        self.assertEqual(ctx.exception.type, TTransportException.SIZE_LIMIT)
        self.assertIn('transform', ctx.exception.message)
        self.assertEqual(len(layers), 0)


if __name__ == '__main__':
    unittest.main()
