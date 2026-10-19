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
# The JSON number reader reads one byte at a time and keeps the bytes that are
# numeric. At the end of the input the transport returns the empty byte string
# b'', which is not a numeric character, so the reader must treat it as the end
# of the number and stop. These tests drive a number that runs to the end of the
# input and assert the read stops after a bounded number of reads, independent of
# string_length_limit, and that a number followed by a delimiter still reads.
#

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.Thrift import TType, TMessageType  # noqa
from thrift.protocol import TJSONProtocol  # noqa
from thrift.protocol.TProtocol import TProtocolException  # noqa
from thrift.transport import TTransport  # noqa


class _ReadPastEndGuard(Exception):
    """Raised when a reader asks the transport for data too many times past the
    end of the input, so a reader that fails to stop cannot run without end
    inside the test itself."""


class GuardedMemoryBuffer(TTransport.TMemoryBuffer):
    """A TMemoryBuffer that serves a fixed payload and then counts the reads it
    is asked for past the end of the input. Past the end read() returns b'' like
    any exhausted transport, but after `cap` such empty reads it raises
    _ReadPastEndGuard."""

    def __init__(self, payload, cap=1000):
        TTransport.TMemoryBuffer.__init__(self, payload)
        self._cap = cap
        self.empty_reads = 0
        self.total_reads = 0

    def read(self, sz):
        self.total_reads += 1
        chunk = TTransport.TMemoryBuffer.read(self, sz)
        if not chunk:
            self.empty_reads += 1
            if self.empty_reads > self._cap:
                raise _ReadPastEndGuard(self.empty_reads)
        return chunk


class JSONNumericEofTest(unittest.TestCase):

    def protocol(self, payload, string_length_limit='default', cap=1000):
        buf = GuardedMemoryBuffer(payload, cap=cap)
        if string_length_limit == 'default':
            prot = TJSONProtocol.TJSONProtocol(buf)
        else:
            prot = TJSONProtocol.TJSONProtocol(buf, string_length_limit=string_length_limit)
        return prot, buf

    def assert_bounded(self, buf, payload):
        # A reader that stops at the end of the input peeks b'' at most once and
        # reads no more than the payload plus that one lookahead.
        self.assertLessEqual(buf.empty_reads, 2, buf.empty_reads)
        self.assertLessEqual(buf.total_reads, len(payload) + 2, buf.total_reads)

    # --- root cause -------------------------------------------------------

    def test_empty_read_is_not_numeric(self):
        prot, _ = self.protocol(b'')
        self.assertIs(prot.isJSONNumeric(b''), False)
        # a real numeric byte is still numeric
        self.assertIs(prot.isJSONNumeric(b'5'), True)
        self.assertIs(prot.isJSONNumeric(b'-'), True)
        self.assertIs(prot.isJSONNumeric(b'x'), False)

    # --- a number that runs to the end of the input -----------------------

    def test_bare_number_at_end_of_input_stops(self):
        for limit in ('default', None):
            prot, buf = self.protocol(b'123', string_length_limit=limit)
            self.assertEqual(prot.readJSONNumericChars(), '123')
            self.assert_bounded(buf, b'123')

    def test_version_token_at_end_of_input_is_refused(self):
        # readMessageBegin reads the protocol version with the number reader; a
        # message that ends right after the version must be refused, not read
        # without end.
        for limit in ('default', None):
            prot, buf = self.protocol(b'[1', string_length_limit=limit)
            with self.assertRaises(TProtocolException) as cm:
                prot.readMessageBegin()
            self.assertEqual(cm.exception.type, TProtocolException.INVALID_DATA)
            self.assert_bounded(buf, b'[1')

    def test_skipped_field_value_at_end_of_input_is_refused(self):
        # A field value whose digits run to the end of the input, reached
        # through skip() when a field is skipped.
        payload = b'[1,"m",1,0,{"1":{"i32":5'
        for limit in ('default', None):
            prot, buf = self.protocol(payload, string_length_limit=limit)
            prot.readMessageBegin()
            with self.assertRaises(TProtocolException) as cm:
                prot.skip(TType.STRUCT)
            self.assertEqual(cm.exception.type, TProtocolException.INVALID_DATA)
            self.assert_bounded(buf, payload)

    def test_missing_number_at_end_of_input_is_refused(self):
        # The number is expected but absent (the input ends where it should
        # start): the existing parse error is surfaced, not an unending read.
        payload = b'[1,"m",1,'
        for limit in ('default', None):
            prot, buf = self.protocol(payload, string_length_limit=limit)
            with self.assertRaises(TProtocolException) as cm:
                prot.readMessageBegin()
            self.assertEqual(cm.exception.type, TProtocolException.INVALID_DATA)
            self.assert_bounded(buf, payload)

    # --- controls: valid numbers still parse unchanged --------------------

    def test_number_followed_by_delimiter_still_reads(self):
        prot, buf = self.protocol(b'123,')
        self.assertEqual(prot.readJSONNumericChars(), '123')
        # the delimiter stopped the read; it was not consumed as a number
        self.assertEqual(buf.empty_reads, 0)

    def test_valid_message_round_trips(self):
        trans = TTransport.TMemoryBuffer()
        w = TJSONProtocol.TJSONProtocol(trans)
        w.writeMessageBegin('ping', TMessageType.CALL, 7)
        w.writeStructBegin('S')
        w.writeFieldBegin('a', TType.I32, 1)
        w.writeI32(123456)
        w.writeFieldEnd()
        w.writeFieldBegin('b', TType.I64, 2)
        w.writeI64(5000000000)
        w.writeFieldEnd()
        w.writeFieldBegin('c', TType.DOUBLE, 3)
        w.writeDouble(3.5)
        w.writeFieldEnd()
        w.writeFieldStop()
        w.writeStructEnd()
        w.writeMessageEnd()

        r = TJSONProtocol.TJSONProtocol(TTransport.TMemoryBuffer(trans.getvalue()))
        name, mtype, seqid = r.readMessageBegin()
        self.assertEqual((name, mtype, seqid), ('ping', TMessageType.CALL, 7))
        r.readStructBegin()
        _, ft, fid = r.readFieldBegin()
        self.assertEqual((ft, fid), (TType.I32, 1))
        self.assertEqual(r.readI32(), 123456)
        r.readFieldEnd()
        _, ft, fid = r.readFieldBegin()
        self.assertEqual((ft, fid), (TType.I64, 2))
        self.assertEqual(r.readI64(), 5000000000)
        r.readFieldEnd()
        _, ft, fid = r.readFieldBegin()
        self.assertEqual((ft, fid), (TType.DOUBLE, 3))
        self.assertEqual(r.readDouble(), 3.5)
        r.readFieldEnd()
        _, ft, _ = r.readFieldBegin()
        self.assertEqual(ft, TType.STOP)
        r.readStructEnd()
        r.readMessageEnd()


if __name__ == '__main__':
    unittest.main()
