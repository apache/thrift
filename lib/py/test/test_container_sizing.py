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
# The C extension sizes a list, tuple or set from the element count the peer
# declares. These tests hold that allocation to what the message can actually
# supply, and check that containers which really are large still decode.
#
# The pure-Python decoder appends in a loop and never preallocates, so it has
# nothing to answer for here.
#

import os
import struct
import sys
import tracemalloc
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.protocol.TBinaryProtocol import TBinaryProtocolAccelerated
from thrift.protocol.TCompactProtocol import TCompactProtocolAccelerated
from thrift.protocol.TProtocol import TType
from thrift.transport import TTransport

try:
    from thrift.protocol import fastbinary
    HAVE_FASTBINARY = True
except ImportError:
    HAVE_FASTBINARY = False


class Holder(object):
    """Stands in for a generated struct with a single container field."""

    __slots__ = ("xs",)


def spec(container_type, immutable):
    """thrift_spec for `struct Holder { 1: <container_type><i64> xs }`."""
    return [
        Holder,
        (
            None,
            (1, container_type, "xs", (TType.I64, None, immutable), None),
        ),
    ]


# A count of one element per byte of body is the most any protocol can encode,
# so a body this short cannot hold anywhere near the count it declares.
DECLARED = 2000000
BODY = tuple(range(3))

# With the count taken at face value the extension reserves 8 bytes per
# element, so DECLARED alone costs about 15 MiB. Anything under a mebibyte
# means the allocation followed the payload instead.
ALLOCATION_CEILING = 1 << 20


def binary_payload(container_type, count, values=()):
    out = struct.pack("!bh", container_type, 1)
    out += struct.pack("!bi", TType.I64, count)
    out += b"".join(struct.pack("!q", v) for v in values)
    return out + b"\x00"


def _varint(n):
    out = b""
    while True:
        if n & ~0x7F == 0:
            return out + bytes(bytearray([n]))
        out += bytes(bytearray([(n & 0x7F) | 0x80]))
        n >>= 7


def _zigzag(n):
    return (n << 1) ^ (n >> 63)


# Compact encodes types with its own tags, unrelated to TType.
COMPACT_I64 = 0x06
COMPACT_LIST = 0x09
COMPACT_SET = 0x0A


def compact_payload(container_type, count, values=()):
    tag = COMPACT_LIST if container_type == TType.LIST else COMPACT_SET
    out = bytes(bytearray([(1 << 4) | tag]))  # field 1, delta encoded
    if count < 15:
        out += bytes(bytearray([(count << 4) | COMPACT_I64]))
    else:
        out += bytes(bytearray([0xF0 | COMPACT_I64])) + _varint(count)
    out += b"".join(_varint(_zigzag(v)) for v in values)
    return out + b"\x00"


PROTOCOLS = (
    ("binary", TBinaryProtocolAccelerated, fastbinary.decode_binary if HAVE_FASTBINARY else None,
     binary_payload),
    ("compact", TCompactProtocolAccelerated, fastbinary.decode_compact if HAVE_FASTBINARY else None,
     compact_payload),
)


def decode(protocol_cls, decoder, payload, thrift_spec, transport=None):
    trans = TTransport.TMemoryBuffer(payload)
    if transport is not None:
        trans = transport(trans)
    obj = Holder()
    decoder(obj, protocol_cls(trans), thrift_spec)
    return obj


def peak_allocation(fn):
    """Bytes allocated at the high-water mark while fn() runs."""
    tracemalloc.start()
    try:
        tracemalloc.reset_peak()
        try:
            fn()
        except Exception:
            pass
        return tracemalloc.get_traced_memory()[1]
    finally:
        tracemalloc.stop()


@unittest.skipUnless(HAVE_FASTBINARY, "fastbinary not built")
class TestContainerPrealloc(unittest.TestCase):
    def test_declared_count_does_not_size_the_allocation(self):
        for name, protocol_cls, decoder, payload in PROTOCOLS:
            for kind, container_type in (("list", TType.LIST), ("set", TType.SET)):
                for immutable in (False, True):
                    with self.subTest(protocol=name, container=kind, immutable=immutable):
                        wire = payload(container_type, DECLARED, BODY)
                        peak = peak_allocation(
                            lambda: decode(protocol_cls, decoder, wire,
                                           spec(container_type, immutable)))
                        self.assertLess(
                            peak, ALLOCATION_CEILING,
                            "%d bytes reserved for a %d byte body declaring %d elements"
                            % (peak, len(wire), DECLARED))

    def test_short_payload_still_reports_the_truncation(self):
        for name, protocol_cls, decoder, payload in PROTOCOLS:
            with self.subTest(protocol=name):
                wire = payload(TType.LIST, DECLARED, BODY)
                with self.assertRaises(EOFError):
                    decode(protocol_cls, decoder, wire, spec(TType.LIST, False))

    def test_small_container_still_decodes(self):
        values = list(range(5))
        for name, protocol_cls, decoder, payload in PROTOCOLS:
            with self.subTest(protocol=name):
                wire = payload(TType.LIST, len(values), values)
                obj = decode(protocol_cls, decoder, wire, spec(TType.LIST, False))
                self.assertEqual(obj.xs, values)

                obj = decode(protocol_cls, decoder, wire, spec(TType.LIST, True))
                self.assertIsInstance(obj.xs, tuple)
                self.assertEqual(obj.xs, tuple(values))


@unittest.skipUnless(HAVE_FASTBINARY, "fastbinary not built")
class TestLargeContainerOverABufferedTransport(unittest.TestCase):
    """A buffered transport holds a few kilobytes at a time, so a container
    larger than that cannot be sized up front from what is in hand. These
    check that it still decodes, whole and in order."""

    COUNT = 200000

    def _check(self, container_type, immutable, expected_type, expected):
        values = list(range(self.COUNT))
        for name, protocol_cls, decoder, payload in PROTOCOLS:
            with self.subTest(protocol=name):
                wire = payload(container_type, len(values), values)
                obj = decode(protocol_cls, decoder, wire,
                             spec(container_type, immutable),
                             transport=TTransport.TBufferedTransport)
                self.assertIsInstance(obj.xs, expected_type)
                self.assertEqual(obj.xs, expected(values))

    def test_list(self):
        self._check(TType.LIST, False, list, lambda v: v)

    def test_immutable_list_is_a_tuple(self):
        self._check(TType.LIST, True, tuple, tuple)

    def test_set(self):
        self._check(TType.SET, False, set, set)

    def test_immutable_set_is_a_frozenset(self):
        self._check(TType.SET, True, frozenset, frozenset)


if __name__ == "__main__":
    unittest.main()
