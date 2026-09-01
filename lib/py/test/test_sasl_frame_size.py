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
# TSaslClientTransport sizes two reads from a length the peer supplies: the
# negotiation message, exchanged before either side has authenticated the
# other, and the data frame carrying a wrapped message afterwards.
#
# The data frame is held to the same maximum the framed transport uses. The
# negotiation message gets a tighter one of its own -- a SASL challenge is
# small, and the peer sending it has not yet proved anything.
#

import os
import struct
import sys
import types
import unittest
from io import BytesIO

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa

# Stub puresasl so TSaslClientTransport can be imported without it installed,
# the same way test_sasl_transport.py does.
sys.modules.setdefault('puresasl', types.ModuleType('puresasl'))
sys.modules.setdefault('puresasl.client', types.ModuleType('puresasl.client'))

from thrift.transport import TTransport  # noqa
from thrift.transport.TTransport import (  # noqa
    TSaslClientTransport,
    TTransportException,
)


class CountingTransport(object):
    """Serves a fixed buffer and records how much was asked of it.

    The count is what separates "refused the declared size" from "tried to read
    it and ran out of data".
    """

    def __init__(self, data=b''):
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
        chunk = self._data[self._pos:self._pos + sz]
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


class PassThroughSasl(object):
    def unwrap(self, encoded):
        return encoded


def sasl_over(inner):
    """A TSaslClientTransport wired to `inner`, without running the handshake.

    __init__ builds a puresasl SASLClient, which is neither available nor needed
    to exercise the two reads under test. Built the way test_sasl_transport.py
    builds one, with the attributes those two methods touch set by hand.
    """
    transport = object.__new__(TSaslClientTransport)
    transport.transport = inner
    transport.sasl = PassThroughSasl()
    transport._TSaslClientTransport__wbuf = BytesIO()
    transport._TSaslClientTransport__rbuf = BytesIO(b'')
    # Set by __init__ in the fixed library; set here too, since __init__ was
    # bypassed. getattr defaults keep this file importable either way.
    transport._max_negotiation_size = getattr(
        TTransport, 'DEFAULT_MAX_SASL_NEGOTIATION_SIZE', None)
    transport._max_frame_size = getattr(
        TTransport, 'DEFAULT_MAX_FRAME_SIZE', None)
    return transport


def negotiation_header(length):
    return struct.pack('>BI', TSaslClientTransport.OK, length)


class TestSaslNegotiationFrameSize(unittest.TestCase):
    """Path A -- recv_sasl_msg(), reached before authentication."""

    def test_an_oversized_negotiation_payload_is_refused(self):
        inner = CountingTransport(negotiation_header(0xFFFFFFFF))

        with self.assertRaises(TTransportException) as caught:
            sasl_over(inner).recv_sasl_msg()

        self.assertEqual(caught.exception.type, TTransportException.SIZE_LIMIT)
        # Five bytes of header, and nothing on account of what it declared.
        self.assertEqual(inner.bytes_requested, 5)

    def test_a_negotiation_payload_within_the_maximum_still_reads(self):
        payload = b'a plausible challenge'
        inner = CountingTransport(negotiation_header(len(payload)) + payload)

        status, got = sasl_over(inner).recv_sasl_msg()

        self.assertEqual(status, TSaslClientTransport.OK)
        self.assertEqual(got, payload)

    def test_an_empty_negotiation_payload_still_reads(self):
        inner = CountingTransport(negotiation_header(0))

        status, got = sasl_over(inner).recv_sasl_msg()

        self.assertEqual(status, TSaslClientTransport.OK)
        self.assertEqual(got, b'')


class TestSaslDataFrameSize(unittest.TestCase):
    """Path B -- _read_frame(), after the handshake."""

    def test_an_oversized_data_frame_is_refused(self):
        inner = CountingTransport(struct.pack('!i', 0x7FFFFFFF))

        with self.assertRaises(TTransportException) as caught:
            sasl_over(inner)._read_frame()

        self.assertEqual(caught.exception.type, TTransportException.SIZE_LIMIT)
        self.assertEqual(inner.bytes_requested, 4)

    def test_a_negative_data_frame_length_is_refused(self):
        inner = CountingTransport(struct.pack('!i', -1))

        with self.assertRaises(TTransportException) as caught:
            sasl_over(inner)._read_frame()

        self.assertEqual(caught.exception.type,
                         TTransportException.NEGATIVE_SIZE)

    def test_a_data_frame_within_the_maximum_still_reads(self):
        payload = b'wrapped bytes'
        inner = CountingTransport(struct.pack('!i', len(payload)) + payload)
        transport = sasl_over(inner)

        transport._read_frame()

        self.assertEqual(transport.read(len(payload)), payload)

    def test_an_empty_data_frame_is_still_accepted(self):
        """Asserted on the buffer rather than through read(): read() fetches
        another frame once the buffer is empty, and there is not one here."""
        inner = CountingTransport(struct.pack('!i', 0))
        transport = sasl_over(inner)

        transport._read_frame()

        self.assertEqual(
            transport._TSaslClientTransport__rbuf.getvalue(), b'')


if __name__ == '__main__':
    unittest.main()
