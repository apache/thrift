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
from thrift.protocol.TMultiplexedProtocol import TMultiplexedProtocol
from thrift.transport.THeaderTransport import (
    THeaderClientType,
    THeaderSubprotocolID,
)
from thrift.transport.TTransport import (
    CReadableTransport,
    TMemoryBuffer,
    TTransportException,
)

try:
    from thrift.protocol import fastbinary  # noqa
except ImportError:
    fastbinary = None

LIMIT = 16384000
# THeaderTransport refuses a frame larger than DEFAULT_MAX_FRAME_SIZE (16384000),
# so a frame carrying a string at or over that size needs a raised cap on both ends.
BIG_FRAME = 0x3FFFFFFE


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


class _S(object):
    """A one-field struct {1: binary}, read by the accelerated decoder."""
    thrift_spec = (None, (1, TType.STRING, 'name', 'BINARY', None, ),)

    def __init__(self, name=None):
        self.name = name


_wire_cache = {}


def _encode_header_string(strlen, subproto=THeaderSubprotocolID.BINARY):
    """A THeaderProtocol CALL framing a struct whose field 1 is strlen 'A' bytes."""
    key = (strlen, subproto)
    if key not in _wire_cache:
        out = TMemoryBuffer()
        w = THeaderProtocol(out, (THeaderClientType.HEADERS,), subproto)
        w.trans.set_max_frame_size(BIG_FRAME)
        w.writeMessageBegin('m', TMessageType.CALL, 1)
        w.writeStructBegin('S')
        w.writeFieldBegin('name', TType.STRING, 1)
        w.writeBinary(b'A' * strlen)
        w.writeFieldEnd()
        w.writeFieldStop()
        w.writeStructEnd()
        w.writeMessageEnd()
        w.trans.flush()
        _wire_cache[key] = out.getvalue()
    return _wire_cache[key]


def _reader(wire, subproto=THeaderSubprotocolID.BINARY):
    r = THeaderProtocol(TMemoryBuffer(wire), (THeaderClientType.HEADERS,), subproto)
    r.trans.set_max_frame_size(BIG_FRAME)
    r.readMessageBegin()  # runs _set_protocol, copies _fast_decode from the inner protocol
    return r


@unittest.skipIf(fastbinary is None, 'the C extension is not built')
class THeaderProtocolStringLengthLimitTest(unittest.TestCase):
    # THeaderProtocol frames an inner accelerated protocol and copies its
    # _fast_decode, so a struct body is decoded in C. The accelerated decoder
    # reads string_length_limit / container_length_limit off the protocol object,
    # so the inner protocol's limits must be visible on the THeaderProtocol.

    def test_header_exposes_the_inner_limits(self):
        for subproto in (THeaderSubprotocolID.BINARY, THeaderSubprotocolID.COMPACT):
            with self.subTest(subprotocol=subproto):
                r = _reader(_encode_header_string(5, subproto), subproto)
                self.assertTrue(hasattr(r, 'string_length_limit'))
                self.assertEqual(r.string_length_limit, LIMIT)
                self.assertEqual(r.string_length_limit, r._protocol.string_length_limit)
                self.assertEqual(r.container_length_limit, r._protocol.container_length_limit)

    def test_header_fast_decode_reads_a_string_at_the_limit(self):
        # Control: a string of exactly the default limit is accepted on the fast path.
        r = _reader(_encode_header_string(LIMIT))
        self.assertIsNotNone(r._fast_decode)
        self.assertIsInstance(r.trans, CReadableTransport)
        s = _S()
        r._fast_decode(s, r, [_S, _S.thrift_spec])
        self.assertEqual(len(s.name), LIMIT)

    def test_header_fast_decode_refuses_a_string_over_the_limit(self):
        # One byte over the default limit: the fast path refuses before the
        # string is materialised. Unfixed, the decoder reads all LIMIT + 1 bytes.
        r = _reader(_encode_header_string(LIMIT + 1))
        self.assertIsNotNone(r._fast_decode)
        self.assertIsInstance(r.trans, CReadableTransport)
        s = _S()
        with self.assertRaises(Exception) as cm:
            r._fast_decode(s, r, [_S, _S.thrift_spec])
        self.assertIn('exceeded', str(cm.exception).lower())
        self.assertIsNone(s.name)

    def test_header_pure_path_also_refuses_over_the_limit(self):
        # Parity control: the delegated (pure-Python) read refuses the same bytes,
        # so the two read paths now agree.
        r = _reader(_encode_header_string(LIMIT + 1))
        r.readStructBegin()
        r.readFieldBegin()
        with self.assertRaises(TTransportException) as cm:
            r.readBinary()
        self.assertEqual(cm.exception.type, TTransportException.SIZE_LIMIT)

    def test_header_through_a_protocol_decorator_still_bounds_the_decoder(self):
        # A multiplexed header server decodes through TProtocolDecorator, whose
        # class is built from a snapshot of the wrapped protocol's __dict__. The
        # limit must still reach the decoder through that snapshot.
        r = _reader(_encode_header_string(LIMIT + 1))
        dec = TMultiplexedProtocol(r, 'svc')
        self.assertIsNotNone(dec._fast_decode)
        self.assertIsInstance(dec.trans, CReadableTransport)
        self.assertEqual(dec.string_length_limit, LIMIT)
        s = _S()
        with self.assertRaises(Exception) as cm:
            dec._fast_decode(s, dec, [_S, _S.thrift_spec])
        self.assertIn('exceeded', str(cm.exception).lower())
        self.assertIsNone(s.name)


if __name__ == '__main__':
    unittest.main()
