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

import _import_local_thrift  # noqa
from thrift.protocol.TJSONProtocol import TJSONProtocol, TJSONProtocolFactory
from thrift.transport import TTransport

#
# In order to run the test under Windows. We need to create symbolic link
# name 'thrift' to '../src' folder by using:
#
# mklink /D thrift ..\src
#


class TestJSONString(unittest.TestCase):

    def test_escaped_unicode_string(self):
        unicode_json = b'"hello \\u0e01\\u0e02\\u0e03\\ud835\\udcab\\udb40\\udc70 unicode"'
        unicode_text = u'hello \u0e01\u0e02\u0e03\U0001D4AB\U000E0070 unicode'

        buf = TTransport.TMemoryBuffer(unicode_json)
        transport = TTransport.TBufferedTransportFactory().getTransport(buf)
        protocol = TJSONProtocol(transport)

        self.assertEqual(protocol.readString(), unicode_text)

    def test_TJSONProtocol_write(self):
        write_data = '{"software":"thrift","1":[23,1.2010000000000001,32767,2147483647,9223372036854775807],"base64":"aGVsbG8gdGhyaWZ0","bool":0}'

        buff = TTransport.TMemoryBuffer()
        transport = TTransport.TBufferedTransportFactory().getTransport(buff)
        protocol = TJSONProtocol(transport)
        protocol.writeJSONObjectStart()
        protocol.writeJSONString("software")
        protocol.writeJSONString("thrift")
        protocol.writeJSONString("1")
        protocol.writeJSONArrayStart()
        protocol.writeJSONNumber(23)
        protocol.writeDouble(1.201)
        protocol.writeI16(32767)
        protocol.writeI32(2147483647)
        protocol.writeI64(9223372036854775807)
        protocol.writeJSONArrayEnd()
        protocol.writeJSONString("base64")
        protocol.writeJSONBase64("hello thrift".encode('utf-8'))
        protocol.writeJSONString("bool")
        protocol.writeBool(0)
        protocol.writeJSONObjectEnd()

        transport.flush()
        value = buff.getvalue()

        self.assertEqual(write_data, value.decode('utf-8'))

    def test_TJSONProtol_read(self):
        expected = "{'software':'thrift','1':[23,1.2010000000000001,32767,2147483647,9223372036854775807],'base64':'hello thrift','bool':False}"
        read_data = '{"software":"thrift","1":[23,1.2010000000000001,32767,2147483647,9223372036854775807],"base64":"aGVsbG8gdGhyaWZ0","bool":0}'

        buff = TTransport.TMemoryBuffer(read_data.encode('utf-8'))
        transport = TTransport.TBufferedTransportFactory().getTransport(buff)
        protocol = TJSONProtocol(transport)
        protocol.readJSONObjectStart()
        u_1 = protocol.readString()
        u_2 = protocol.readString()
        u_3 = protocol.readString()
        protocol.readJSONArrayStart()
        u_4 = protocol.readNumber()
        u_5 = protocol.readDouble()
        u_6 = protocol.readI16()
        u_7 = protocol.readI32()
        u_8 = protocol.readI64()
        protocol.readJSONArrayEnd()
        u_9 = protocol.readString()
        u_10 = protocol.readJSONBase64()
        u_11 = protocol.readString()
        u_12 = protocol.readBool()
        protocol.writeJSONObjectEnd()

        result_read = {}
        result_read[u_1] = u_2
        result_read[u_3] = []
        result_read[u_3].append(u_4)
        result_read[u_3].append(u_5)
        result_read[u_3].append(u_6)
        result_read[u_3].append(u_7)
        result_read[u_3].append(u_8)
        result_read[u_9] = u_10.decode('utf-8')
        result_read[u_11] = u_12

        self.assertEqual(eval(expected), result_read)


class TestJSONStringSizeLimit(unittest.TestCase):
    """A JSON string/number is quote- or character-delimited, so unlike a
    length-prefixed binary field there is no declared size to reject up front.
    TJSONProtocol must therefore honor the same string_length_limit that
    TBinaryProtocol/TCompactProtocol already enforce, bounding a single decoded
    field as it is read."""

    @staticmethod
    def _protocol(data, **kwargs):
        buf = TTransport.TMemoryBuffer(data)
        transport = TTransport.TBufferedTransportFactory().getTransport(buf)
        return TJSONProtocol(transport, **kwargs)

    def test_string_within_limit_is_read(self):
        # A string comfortably under the limit is returned unchanged.
        proto = self._protocol(b'"' + b'A' * 512 + b'"', string_length_limit=1024)
        self.assertEqual(proto.readString(), 'A' * 512)

    def test_string_exceeding_limit_is_rejected(self):
        # A single string larger than the configured limit is rejected as it grows.
        proto = self._protocol(b'"' + b'A' * 4096 + b'"', string_length_limit=1024)
        with self.assertRaises(TTransport.TTransportException) as ctx:
            proto.readString()
        self.assertEqual(ctx.exception.type, TTransport.TTransportException.SIZE_LIMIT)

    def test_number_exceeding_limit_is_rejected(self):
        # The same char-by-char, no-declared-length reasoning applies to numeric
        # literals via readJSONNumericChars(). Terminate the run with a
        # non-numeric byte so that, absent the bound, the read would stop cleanly
        # rather than on buffer exhaustion -- isolating the limit check.
        proto = self._protocol(b'1' * 4096 + b' ', string_length_limit=1024)
        with self.assertRaises(TTransport.TTransportException) as ctx:
            proto.readI64()
        self.assertEqual(ctx.exception.type, TTransport.TTransportException.SIZE_LIMIT)

    def test_default_has_no_string_limit(self):
        # Parity with binary/compact: the limit is opt-in. With no limit set the
        # same large string is read successfully, confirming the rejections above
        # are the configured limit firing, not malformed input.
        proto = self._protocol(b'"' + b'A' * 4096 + b'"')
        self.assertEqual(proto.readString(), 'A' * 4096)

    def test_limit_enforced_via_factory(self):
        # The limit must be threaded through TJSONProtocolFactory, not only the
        # direct constructor -- servers build protocols through the factory.
        buf = TTransport.TMemoryBuffer(b'"' + b'A' * 4096 + b'"')
        proto = TJSONProtocolFactory(string_length_limit=1024).getProtocol(buf)
        with self.assertRaises(TTransport.TTransportException) as ctx:
            proto.readString()
        self.assertEqual(ctx.exception.type, TTransport.TTransportException.SIZE_LIMIT)

    def test_limit_counts_bytes_not_characters(self):
        # 100 three-byte UTF-8 characters = 300 wire bytes but only 100 decoded
        # characters. A limit of 200 must reject it, which only a byte count (not
        # a character count) does -- keeping parity with the byte-length limit
        # TBinaryProtocol enforces.
        data = b'"' + '\u0e01'.encode('utf-8') * 100 + b'"'
        proto = self._protocol(data, string_length_limit=200)
        with self.assertRaises(TTransport.TTransportException) as ctx:
            proto.readString()
        self.assertEqual(ctx.exception.type, TTransport.TTransportException.SIZE_LIMIT)

    def test_multibyte_run_read_is_bounded(self):
        # A single unbroken run of multibyte (>= 0x80) bytes is consumed in one
        # inner decode loop; it must be rejected as it grows, not buffered in
        # full first, so the bytes actually read stay bounded by the limit.
        limit = 1024
        trans = _ByteCountingTransport(b'"' + b'\xc2\xa0' * 50000 + b'"')
        proto = TJSONProtocol(trans, string_length_limit=limit)
        with self.assertRaises(TTransport.TTransportException) as ctx:
            proto.readString()
        self.assertEqual(ctx.exception.type, TTransport.TTransportException.SIZE_LIMIT)
        self.assertLess(trans.bytes_read, limit + 100)


class _ByteCountingTransport(TTransport.TTransportBase):
    """Reads from an in-memory buffer while recording how many bytes were
    actually consumed, so a test can assert an oversized field is rejected as it
    grows rather than buffered in full."""

    def __init__(self, data):
        self._buf = TTransport.TMemoryBuffer(data)
        self.bytes_read = 0

    def isOpen(self):
        return True

    def read(self, sz):
        chunk = self._buf.read(sz)
        self.bytes_read += len(chunk)
        return chunk


if __name__ == '__main__':
    unittest.main()
