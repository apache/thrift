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
# TFramedTransport reads a four-byte length and then asks the transport
# underneath for exactly that many bytes. readAll() accumulates them, so the
# four bytes decide how much the process will hold before it can notice
# anything is wrong.
#
# THeaderTransport in this same binding has enforced a maximum since it was
# written (DEFAULT_MAX_FRAME_SIZE, 16 MB, the value every other binding uses).
# These tests hold TFramedTransport to the same bound.
#

import os
import struct
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.transport import TTransport  # noqa
from thrift.transport.TTransport import (  # noqa
    TFramedTransport,
    TMemoryBuffer,
    TTransportException,
)


class DripFeedTransport(object):
    """Answers reads a byte at a time out of a fixed buffer.

    A real peer declaring a huge frame does not have to send it: it can trickle,
    or send nothing at all. This stands in for that without a socket, and it
    counts how much was actually asked for, so a test can tell "refused the
    declaration" apart from "tried to read it and ran out".
    """

    def __init__(self, data):
        self._data = data
        self._pos = 0
        self.bytes_requested = 0

    def isOpen(self):
        return True

    def open(self):
        pass

    def close(self):
        pass

    def read(self, sz):
        self.bytes_requested += sz
        chunk = self._data[self._pos:self._pos + 1]
        self._pos += len(chunk)
        return chunk

    def readAll(self, sz):
        self.bytes_requested += sz
        chunk = self._data[self._pos:self._pos + sz]
        self._pos += len(chunk)
        if len(chunk) != sz:
            raise EOFError()
        return chunk

    def write(self, buf):
        pass

    def flush(self):
        pass


def framed_over(declared_size, payload=b''):
    """A framed transport reading one frame with the given declared size."""
    return TFramedTransport(
        DripFeedTransport(struct.pack('!i', declared_size) + payload))


class TestFramedFrameSize(unittest.TestCase):

    def test_frame_larger_than_the_maximum_is_refused(self):
        transport = framed_over(TTransport.DEFAULT_MAX_FRAME_SIZE + 1)

        with self.assertRaises(TTransportException) as caught:
            transport.read(1)

        self.assertEqual(caught.exception.type, TTransportException.SIZE_LIMIT)

    def test_a_declared_two_gigabytes_is_refused_without_reading_it(self):
        """The point of the bound: refuse on the declaration, not after the fact."""
        inner = DripFeedTransport(struct.pack('!i', 0x7FFFFFFF))
        transport = TFramedTransport(inner)

        with self.assertRaises(TTransportException):
            transport.read(1)

        # Four bytes for the header, and nothing asked for on account of the
        # size the header declared.
        self.assertEqual(inner.bytes_requested, 4)

    def test_negative_frame_size_is_refused(self):
        transport = framed_over(-1)

        with self.assertRaises(TTransportException) as caught:
            transport.read(1)

        self.assertEqual(caught.exception.type,
                         TTransportException.NEGATIVE_SIZE)

    def test_the_maximum_is_configurable(self):
        payload = b'x' * 64
        transport = TFramedTransport(
            DripFeedTransport(struct.pack('!i', len(payload)) + payload),
            max_frame_size=32)

        with self.assertRaises(TTransportException) as caught:
            transport.read(1)
        self.assertEqual(caught.exception.type, TTransportException.SIZE_LIMIT)

        transport = TFramedTransport(
            DripFeedTransport(struct.pack('!i', len(payload)) + payload),
            max_frame_size=128)
        self.assertEqual(transport.read(64), payload)

    def test_a_frame_within_the_maximum_still_reads(self):
        payload = b'hello, frame'
        transport = framed_over(len(payload), payload)

        self.assertEqual(transport.read(len(payload)), payload)

    def test_a_round_trip_through_a_memory_buffer_is_unaffected(self):
        """Whatever this binding writes, it must still be able to read."""
        payload = b'a round trip' * 100

        buffer = TMemoryBuffer()
        writer = TFramedTransport(buffer)
        writer.write(payload)
        writer.flush()

        reader = TFramedTransport(TMemoryBuffer(buffer.getvalue()))
        self.assertEqual(reader.read(len(payload)), payload)

    def test_an_empty_frame_is_still_accepted(self):
        """flush() with nothing buffered writes a zero-length frame, and Java's
        reader accepts one, so refusing it here would break interoperability.
        It is harmless: the read below simply yields nothing."""
        transport = framed_over(0)

        self.assertEqual(transport.read(4), b'')

    def test_cstringio_refill_is_bounded_too(self):
        """The C extension refills through this path rather than through read()."""
        inner = DripFeedTransport(struct.pack('!i', 0x7FFFFFFF))
        transport = TFramedTransport(inner)

        with self.assertRaises(TTransportException):
            transport.cstringio_refill(b'', 1)

        self.assertEqual(inner.bytes_requested, 4)


if __name__ == '__main__':
    unittest.main()
