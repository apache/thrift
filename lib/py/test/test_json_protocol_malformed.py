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
# A truncated or malformed JSON string body used to surface as a raw Python
# TypeError/ValueError (ord() on the empty EOF read, int(x, 16) on a short \uXXXX
# escape, decode() on an invalid UTF-8 run, b64decode() on a bad binary body)
# instead of a TProtocolException. Application code catching the documented
# TException base therefore missed a malformed peer message. These cases must now
# all raise TProtocolException, and a well-formed value must still read.
#

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import _import_local_thrift  # noqa
from thrift.transport.TTransport import TMemoryBuffer  # noqa
from thrift.protocol.TJSONProtocol import TJSONProtocol  # noqa
from thrift.protocol.TProtocol import TProtocolException  # noqa


class MalformedJSONStringTest(unittest.TestCase):

    def _read_string(self, data):
        return TJSONProtocol(TMemoryBuffer(data)).readString()

    def _read_binary(self, data):
        return TJSONProtocol(TMemoryBuffer(data)).readBinary()

    def test_truncated_after_opening_quote(self):
        with self.assertRaises(TProtocolException):
            self._read_string(b'"')

    def test_truncated_after_backslash(self):
        with self.assertRaises(TProtocolException):
            self._read_string(b'"\\')

    def test_truncated_unicode_escape(self):
        with self.assertRaises(TProtocolException):
            self._read_string(b'"\\u')

    def test_non_hex_unicode_escape(self):
        with self.assertRaises(TProtocolException):
            self._read_string(b'"\\uZZZZ')

    def test_truncated_utf8_sequence(self):
        with self.assertRaises(TProtocolException):
            self._read_string(b'"\xc3')

    def test_invalid_base64_binary(self):
        with self.assertRaises(TProtocolException):
            self._read_binary(b'"A"')

    def test_non_ascii_base64_binary(self):
        with self.assertRaises(TProtocolException):
            self._read_binary(b'"\xc3\xa9"')

    def test_unknown_field_type_code(self):
        # A struct field whose JSON type code is not a known type ("zz") must be a
        # protocol error, not a raw KeyError from the JTYPES lookup.
        p = TJSONProtocol(TMemoryBuffer(b'{"1":{"zz":5}}'))
        p.readStructBegin()
        with self.assertRaises(TProtocolException):
            p.readFieldBegin()

    def test_a_well_formed_string_still_reads(self):
        self.assertEqual(self._read_string(b'"hello"'), "hello")


if __name__ == '__main__':
    unittest.main()
