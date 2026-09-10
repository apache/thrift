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
# The binary and compact protocols read a string's length off the wire and hand
# it to the transport as a read size. A framed transport bounds that by its frame
# size; an unframed one (TSocket, TBufferedTransport) has nothing to bound it with
# unless the caller sets string_length_limit. These tests hold both protocols,
# their accelerated variants and their factories to a default limit, and check
# that a refused length is never asked of the transport.
#

import os
import struct
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.Thrift import TType  # noqa
from thrift.protocol import TBinaryProtocol, TCompactProtocol  # noqa
from thrift.transport import TTransport  # noqa
from thrift.transport.TTransport import TTransportException  # noqa

try:
    from thrift.protocol import fastbinary  # noqa
except ImportError:
    fastbinary = None

LIMIT = 16384000
HUGE = 0x7fffffff


class CountingTransport(TTransport.TTransportBase):
    """Serves a fixed buffer and records every read size it is asked for."""

    def __init__(self, data):
        self._data = data
        self._pos = 0
        self.requested = []

    def isOpen(self):
        return True

    def read(self, sz):
        self.requested.append(sz)
        chunk = self._data[self._pos:self._pos + sz]
        self._pos += len(chunk)
        return chunk


def binary_length(length):
    return struct.pack('!i', length)


def compact_binary_field(length):
    """A compact field header for a binary field with id 1, then its length."""
    out = bytearray(b'\x18')
    while True:
        byte, length = length & 0x7f, length >> 7
        out.append(byte | 0x80 if length else byte)
        if not length:
            return bytes(out)


class StringLengthLimitTest(unittest.TestCase):

    def assert_refused_unread(self, trans, read, declared):
        with self.assertRaises(TTransportException) as cm:
            read()
        self.assertEqual(cm.exception.type, TTransportException.SIZE_LIMIT)
        self.assertTrue(all(sz < declared for sz in trans.requested), trans.requested)

    def test_the_default_is_exported(self):
        from thrift.protocol.TProtocol import DEFAULT_STRING_LENGTH_LIMIT
        self.assertEqual(DEFAULT_STRING_LENGTH_LIMIT, LIMIT)

    def test_protocols_and_factories_default_to_the_limit(self):
        trans = CountingTransport(b'')
        for prot in (TBinaryProtocol.TBinaryProtocol(trans),
                     TBinaryProtocol.TBinaryProtocolFactory().getProtocol(trans),
                     TBinaryProtocol.TBinaryProtocolAcceleratedFactory().getProtocol(trans),
                     TCompactProtocol.TCompactProtocol(trans),
                     TCompactProtocol.TCompactProtocolFactory().getProtocol(trans),
                     TCompactProtocol.TCompactProtocolAcceleratedFactory().getProtocol(trans)):
            self.assertEqual(prot.string_length_limit, LIMIT, type(prot).__name__)

    def test_binary_refuses_a_string_over_the_default_limit(self):
        trans = CountingTransport(binary_length(HUGE))
        prot = TBinaryProtocol.TBinaryProtocol(trans)
        self.assert_refused_unread(trans, prot.readString, HUGE)

    def test_compact_refuses_a_string_over_the_default_limit(self):
        trans = CountingTransport(compact_binary_field(HUGE))
        prot = TCompactProtocol.TCompactProtocol(trans)
        prot.readStructBegin()
        prot.readFieldBegin()
        self.assert_refused_unread(trans, prot.readString, HUGE)

    def test_the_default_limit_is_the_boundary(self):
        trans = CountingTransport(binary_length(LIMIT + 1))
        self.assert_refused_unread(trans, TBinaryProtocol.TBinaryProtocol(trans).readString,
                                   LIMIT + 1)
        trans = CountingTransport(binary_length(LIMIT))
        with self.assertRaises(EOFError):
            TBinaryProtocol.TBinaryProtocol(trans).readString()
        self.assertIn(LIMIT, trans.requested)

    def test_an_old_style_message_name_is_bounded(self):
        # Without strictRead, a non-negative first word is the method name's length.
        trans = CountingTransport(binary_length(HUGE))
        prot = TBinaryProtocol.TBinaryProtocol(trans)
        self.assert_refused_unread(trans, prot.readMessageBegin, HUGE)

    def test_an_old_style_message_name_honours_an_explicit_limit(self):
        trans = CountingTransport(binary_length(101) + b'x' * 101)
        prot = TBinaryProtocol.TBinaryProtocol(trans, string_length_limit=100)
        self.assert_refused_unread(trans, prot.readMessageBegin, 101)

    def test_an_explicit_limit_still_applies(self):
        trans = CountingTransport(binary_length(101) + b'x' * 101)
        prot = TBinaryProtocol.TBinaryProtocol(trans, string_length_limit=100)
        self.assert_refused_unread(trans, prot.readString, 101)

    def test_a_string_within_the_limit_still_reads(self):
        trans = CountingTransport(binary_length(5) + b'hello')
        self.assertEqual(TBinaryProtocol.TBinaryProtocol(trans).readString(), 'hello')

    def test_none_still_means_no_limit(self):
        trans = CountingTransport(binary_length(LIMIT + 1))
        prot = TBinaryProtocol.TBinaryProtocol(trans, string_length_limit=None)
        with self.assertRaises(EOFError):
            prot.readString()
        self.assertIn(LIMIT + 1, trans.requested)


class _S(object):
    thrift_spec = (None, (1, TType.STRING, 'name', 'BINARY', None, ),)

    def __init__(self, name=None):
        self.name = name


@unittest.skipIf(fastbinary is None, 'the C extension is not built')
class AcceleratedStringLengthLimitTest(unittest.TestCase):

    def requested_by(self, protocol_class, payload):
        inner = CountingTransport(payload)
        prot = protocol_class(TTransport.TBufferedTransport(inner))
        with self.assertRaises(Exception):
            prot._fast_decode(_S(), prot, [_S, _S.thrift_spec])
        return inner.requested

    def test_binary_refuses_a_string_over_the_default_limit(self):
        requested = self.requested_by(TBinaryProtocol.TBinaryProtocolAccelerated,
                                      struct.pack('!bhi', TType.STRING, 1, HUGE))
        self.assertLess(max(requested), HUGE // 2, requested)

    def test_compact_refuses_a_string_over_the_default_limit(self):
        requested = self.requested_by(TCompactProtocol.TCompactProtocolAccelerated,
                                      compact_binary_field(HUGE))
        self.assertLess(max(requested), HUGE // 2, requested)


if __name__ == '__main__':
    unittest.main()
