#!/usr/bin/env python

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

"""
Test cases for THRIFT-4002: Immutable exception deserialization.

This test verifies that immutable structs (including exceptions, which are immutable
by default since Thrift 0.14.0) can be properly deserialized without triggering
the __setattr__ TypeError.

The bug manifests when:
1. A struct class is marked immutable (has __setattr__ that raises TypeError)
2. Thrift's deserialization tries to set attributes via setattr instead of
   using the kwargs constructor

This test ensures that all deserialization paths (C extension, pure Python,
all protocols) correctly handle immutable structs.
"""

import unittest
from collections.abc import Hashable

import glob
import os
import sys

SCRIPT_DIR = os.path.realpath(os.path.dirname(__file__))
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.dirname(SCRIPT_DIR)))

for libpath in glob.glob(os.path.join(ROOT_DIR, 'lib', 'py', 'build', 'lib.*')):
    for pattern in ('-%d.%d', '-%d%d'):
        postfix = pattern % (sys.version_info[0], sys.version_info[1])
        if libpath.endswith(postfix):
            sys.path.insert(0, libpath)
            break
else:
    src_path = os.path.join(ROOT_DIR, 'lib', 'py', 'src')
    if os.path.exists(src_path):
        sys.path.insert(0, src_path)
from thrift.Thrift import TException
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol, TCompactProtocol


class ImmutableException(TException):
    """Test exception that mimics generated immutable exception behavior."""

    thrift_spec = (
        None,  # 0
        (1, 11, 'message', 'UTF8', None, ),  # 1: string
    )

    def __init__(self, message=None):
        super(ImmutableException, self).__init__(message)

    def __setattr__(self, *args):
        raise TypeError("can't modify immutable instance")

    def __delattr__(self, *args):
        raise TypeError("can't modify immutable instance")

    def __hash__(self):
        return hash(self.__class__) ^ hash((self.message,))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.message == other.message

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('ImmutableException')
        if self.message is not None:
            oprot.writeFieldBegin('message', 11, 1)
            oprot.writeString(self.message)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    @classmethod
    def read(cls, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and cls.thrift_spec is not None:
            return iprot._fast_decode(None, iprot, [cls, cls.thrift_spec])
        return iprot.readStruct(cls, cls.thrift_spec, True)


class MutableException(TException):
    """Test exception that mimics generated mutable exception behavior."""

    thrift_spec = (
        None,  # 0
        (1, 11, 'message', 'UTF8', None, ),  # 1: string
    )

    def __init__(self, message=None):
        super(MutableException, self).__init__(message)

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, [self.__class__, self.thrift_spec]))
            return
        oprot.writeStructBegin('MutableException')
        if self.message is not None:
            oprot.writeFieldBegin('message', 11, 1)
            oprot.writeString(self.message)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    @classmethod
    def read(cls, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and cls.thrift_spec is not None:
            return iprot._fast_decode(None, iprot, [cls, cls.thrift_spec])
        return iprot.readStruct(cls, cls.thrift_spec, False)


class TestImmutableExceptionDeserialization(unittest.TestCase):
    """Test that immutable exceptions can be properly deserialized."""

    def _roundtrip(self, exc, protocol_class):
        """Serialize and deserialize an exception."""
        otrans = TTransport.TMemoryBuffer()
        oproto = protocol_class.getProtocol(otrans)
        exc.write(oproto)
        itrans = TTransport.TMemoryBuffer(otrans.getvalue())
        iproto = protocol_class.getProtocol(itrans)
        return exc.__class__.read(iproto)

    def test_immutable_exception_is_hashable(self):
        """Verify that immutable exceptions are hashable (required for caching/logging)."""
        exc = ImmutableException(message="test")
        self.assertTrue(isinstance(exc, Hashable))
        self.assertEqual(hash(exc), hash(ImmutableException(message="test")))

    def test_immutable_exception_blocks_modification(self):
        """Verify that immutable exceptions raise TypeError on attribute modification."""
        exc = ImmutableException(message="test")
        with self.assertRaises(TypeError) as cm:
            exc.message = "modified"
        self.assertIn("immutable", str(cm.exception))

    def test_immutable_exception_blocks_deletion(self):
        """Verify that immutable exceptions raise TypeError on attribute deletion."""
        exc = ImmutableException(message="test")
        with self.assertRaises(TypeError) as cm:
            del exc.message
        self.assertIn("immutable", str(cm.exception))

    def test_immutable_exception_binary_protocol(self):
        """Test immutable exception deserialization with TBinaryProtocol."""
        exc = ImmutableException(message="test error")
        deserialized = self._roundtrip(exc, TBinaryProtocol.TBinaryProtocolFactory())
        self.assertEqual(exc.message, deserialized.message)
        self.assertEqual(exc, deserialized)

    def test_immutable_exception_compact_protocol(self):
        """Test immutable exception deserialization with TCompactProtocol."""
        exc = ImmutableException(message="test error")
        deserialized = self._roundtrip(exc, TCompactProtocol.TCompactProtocolFactory())
        self.assertEqual(exc.message, deserialized.message)
        self.assertEqual(exc, deserialized)

    def test_mutable_exception_can_be_modified(self):
        """Verify that mutable exceptions can be modified (control test)."""
        exc = MutableException(message="original")
        exc.message = "modified"
        self.assertEqual(exc.message, "modified")


class TestImmutableExceptionAccelerated(unittest.TestCase):
    """Test immutable exception deserialization with accelerated protocols (C extension)."""

    def setUp(self):
        try:
            # The import is intentionally unused - it only checks if the C extension
            # is available by catching ImportError. The noqa comment documents this.
            from thrift.protocol import fastbinary  # noqa: F401
            self._has_c_extension = True
        except ImportError:
            self._has_c_extension = False

    def _roundtrip(self, exc, protocol_class):
        """Serialize and deserialize an exception."""
        otrans = TTransport.TMemoryBuffer()
        oproto = protocol_class.getProtocol(otrans)
        exc.write(oproto)
        itrans = TTransport.TMemoryBuffer(otrans.getvalue())
        iproto = protocol_class.getProtocol(itrans)
        return exc.__class__.read(iproto)

    def test_immutable_exception_binary_accelerated(self):
        """Test immutable exception with TBinaryProtocolAccelerated."""
        if not self._has_c_extension:
            self.skipTest("C extension not available")
        exc = ImmutableException(message="test error")
        deserialized = self._roundtrip(
            exc,
            TBinaryProtocol.TBinaryProtocolAcceleratedFactory(fallback=False)
        )
        self.assertEqual(exc.message, deserialized.message)
        self.assertEqual(exc, deserialized)

    def test_immutable_exception_compact_accelerated(self):
        """Test immutable exception with TCompactProtocolAccelerated."""
        if not self._has_c_extension:
            self.skipTest("C extension not available")
        exc = ImmutableException(message="test error")
        deserialized = self._roundtrip(
            exc,
            TCompactProtocol.TCompactProtocolAcceleratedFactory(fallback=False)
        )
        self.assertEqual(exc.message, deserialized.message)
        self.assertEqual(exc, deserialized)


def suite():
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    suite.addTest(loader.loadTestsFromTestCase(TestImmutableExceptionDeserialization))
    suite.addTest(loader.loadTestsFromTestCase(TestImmutableExceptionAccelerated))
    return suite


if __name__ == "__main__":
    unittest.main(defaultTest="suite", testRunner=unittest.TextTestRunner(verbosity=2))
