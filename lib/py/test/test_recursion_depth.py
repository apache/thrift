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
# Round-trip tests for the struct/exception read/write recursion depth limit.
# Covers the pure-Python path and, when fastbinary is available, the C extension,
# for classes generated with and without the py:dynamic option.
#

import io
import os
import sys
import unittest

gen_path = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "gen-py"
)
sys.path.insert(0, gen_path)

import _import_local_thrift  # noqa
from thrift.protocol.TBinaryProtocol import TBinaryProtocol
from thrift.protocol.TBinaryProtocol import TBinaryProtocolAccelerated
from thrift.protocol.TCompactProtocol import TCompactProtocol
from thrift.protocol.TCompactProtocol import TCompactProtocolAccelerated
from thrift.protocol.TJSONProtocol import TJSONProtocol
from thrift.protocol.TProtocol import TProtocolBase, TProtocolException
from thrift.transport import TTransport

from Recursive.ttypes import CoError, CoError2, RecTree
from RecursiveDynamic.ttypes import DynNode, DynTree, FrozenDynNode

LIMIT = TProtocolBase.DEFAULT_RECURSION_DEPTH


def make_chain(depth):
    """RecTree chain where writing increments the counter 'depth' times."""
    node = RecTree()
    for _ in range(depth - 1):
        node = RecTree(children=[node])
    return node


def make_error_chain(depth):
    """CoError/CoError2 alternating chain of total 'depth' struct levels."""
    leaf = CoError()
    node = leaf
    for _ in range(depth - 1):
        if isinstance(node, CoError):
            node = CoError2(other=node)
        else:
            node = CoError(other=node)
    return node


def make_binary_payload(depth):
    """Raw TBinaryProtocol payload for a chain of 'depth' nested RecTree nodes."""
    payload = b"\x00"  # leaf: STOP
    for _ in range(depth - 1):
        # field id=1 type=LIST, list elem=STRUCT count=1, then STOP for outer struct
        payload = b"\x0f\x00\x01\x0c\x00\x00\x00\x01" + payload + b"\x00"
    return payload


def make_skip_payload(depth):
    """Raw TBinaryProtocol payload nesting 'depth' structs under a field id no
    spec declares, so every level is read through the protocol's skip path."""
    payload = b"\x00"  # innermost struct: STOP
    for _ in range(depth):
        # field id=7 type=STRUCT, the nested struct, then STOP for this level
        payload = b"\x0c\x00\x07" + payload + b"\x00"
    return payload


def make_compact_payload(depth):
    """Compact-protocol payload for a chain of 'depth' nested RecTree nodes."""
    buf = TTransport.TMemoryBuffer()
    proto = TCompactProtocol(buf)
    proto.DEFAULT_RECURSION_DEPTH = depth
    make_chain(depth).write(proto)
    return buf.getvalue()


def make_json_payload(depth):
    """JSON-protocol payload for a chain of 'depth' nested RecTree nodes."""
    buf = TTransport.TMemoryBuffer()
    proto = TJSONProtocol(buf)
    proto.DEFAULT_RECURSION_DEPTH = depth
    make_chain(depth).write(proto)
    return buf.getvalue()


def roundtrip_binary(struct):
    buf = TTransport.TMemoryBuffer()
    struct.write(TBinaryProtocol(buf))
    result = RecTree()
    result.read(TBinaryProtocol(TTransport.TMemoryBuffer(buf.getvalue())))
    return result


def roundtrip_accel(struct):
    buf = TTransport.TMemoryBuffer()
    struct.write(TBinaryProtocolAccelerated(buf))
    result = RecTree()
    result.read(TBinaryProtocolAccelerated(TTransport.TMemoryBuffer(buf.getvalue())))
    return result


class RecursionDepthBinaryTest(unittest.TestCase):

    def test_roundtrip_at_limit(self):
        self.assertIsNotNone(roundtrip_binary(make_chain(LIMIT)))

    def test_write_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            make_chain(LIMIT + 1).write(TBinaryProtocol(TTransport.TMemoryBuffer()))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_read_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            result = RecTree()
            result.read(TBinaryProtocol(TTransport.TMemoryBuffer(make_binary_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_exception_type_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            make_error_chain(LIMIT + 1).write(TBinaryProtocol(TTransport.TMemoryBuffer()))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_skip_under_limit(self):
        result = RecTree()
        result.read(TBinaryProtocol(TTransport.TMemoryBuffer(make_skip_payload(LIMIT - 1))))
        self.assertIsNotNone(result)

    def test_skip_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            result = RecTree()
            result.read(TBinaryProtocol(TTransport.TMemoryBuffer(make_skip_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_depth_restored_after_exception(self):
        """Depth counter must return to 0 after a caught overflow so the protocol is reusable."""
        proto = TBinaryProtocol(TTransport.TMemoryBuffer())
        with self.assertRaises(TProtocolException):
            make_chain(LIMIT + 1).write(proto)
        self.assertEqual(proto._recursion_depth, 0)


class RecursionDepthAcceleratedTest(unittest.TestCase):

    def setUp(self):
        try:
            import thrift.protocol.fastbinary  # noqa
            self.has_fastbinary = True
        except ImportError:
            self.has_fastbinary = False

    def test_roundtrip_at_limit(self):
        if not self.has_fastbinary:
            self.skipTest("fastbinary not built")
        self.assertIsNotNone(roundtrip_accel(make_chain(LIMIT)))

    def test_write_over_limit(self):
        if not self.has_fastbinary:
            self.skipTest("fastbinary not built")
        with self.assertRaises(TProtocolException) as ctx:
            make_chain(LIMIT + 1).write(TBinaryProtocolAccelerated(TTransport.TMemoryBuffer()))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_read_over_limit(self):
        if not self.has_fastbinary:
            self.skipTest("fastbinary not built")
        with self.assertRaises(TProtocolException) as ctx:
            result = RecTree()
            result.read(
                TBinaryProtocolAccelerated(
                    TTransport.TMemoryBuffer(make_binary_payload(LIMIT + 1))
                )
            )
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_skip_under_limit(self):
        """Fields the spec does not declare are read through skip, which the
        accelerated decoder must bound the same way it bounds a struct read."""
        if not self.has_fastbinary:
            self.skipTest("fastbinary not built")
        result = RecTree()
        result.read(
            TBinaryProtocolAccelerated(
                TTransport.TMemoryBuffer(make_skip_payload(LIMIT - 1))
            )
        )
        self.assertIsNotNone(result)

    def test_skip_over_limit(self):
        if not self.has_fastbinary:
            self.skipTest("fastbinary not built")
        with self.assertRaises(TProtocolException) as ctx:
            result = RecTree()
            result.read(
                TBinaryProtocolAccelerated(
                    TTransport.TMemoryBuffer(make_skip_payload(LIMIT + 1))
                )
            )
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)


class RecursionDepthCompactTest(unittest.TestCase):

    def test_roundtrip_at_limit(self):
        buf = TTransport.TMemoryBuffer()
        make_chain(LIMIT).write(TCompactProtocol(buf))
        result = RecTree()
        result.read(TCompactProtocol(TTransport.TMemoryBuffer(buf.getvalue())))
        self.assertIsNotNone(result)

    def test_write_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            make_chain(LIMIT + 1).write(TCompactProtocol(TTransport.TMemoryBuffer()))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_read_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            result = RecTree()
            result.read(TCompactProtocol(TTransport.TMemoryBuffer(make_compact_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_write_over_limit_accelerated(self):
        with self.assertRaises(TProtocolException) as ctx:
            make_chain(LIMIT + 1).write(TCompactProtocolAccelerated(TTransport.TMemoryBuffer()))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)


class RecursionDepthJSONTest(unittest.TestCase):

    def test_roundtrip_at_limit(self):
        buf = TTransport.TMemoryBuffer()
        make_chain(LIMIT).write(TJSONProtocol(buf))
        result = RecTree()
        result.read(TJSONProtocol(TTransport.TMemoryBuffer(buf.getvalue())))
        self.assertIsNotNone(result)

    def test_read_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            result = RecTree()
            result.read(TJSONProtocol(TTransport.TMemoryBuffer(make_json_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_write_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            make_chain(LIMIT + 1).write(TJSONProtocol(TTransport.TMemoryBuffer()))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)


def make_binary_node_payload(depth):
    """Raw TBinaryProtocol payload for 'depth' nested DynNode (or FrozenDynNode)
    levels: each level but the innermost opens field id=1 type=STRUCT, and every
    level ends with STOP."""
    return b"\x0c\x00\x01" * (depth - 1) + b"\x00" * depth


def make_compact_node_payload(depth):
    """The same nesting in TCompactProtocol: field id delta 1, type STRUCT."""
    return b"\x1c" * (depth - 1) + b"\x00" * depth


def make_json_node_payload(depth):
    """The same nesting in TJSONProtocol."""
    return b'{"1":{"rec":' * (depth - 1) + b"{}" + b"}}" * (depth - 1)


def make_dyn_chain(depth):
    """DynNode chain of 'depth' nested levels."""
    node = DynNode()
    for _ in range(depth - 1):
        node = DynNode(child=node)
    return node


def chain_depth(node):
    """Number of nested levels in a DynNode, FrozenDynNode or DynTree chain."""
    depth = 0
    while node is not None:
        depth += 1
        if isinstance(node, DynTree):
            node = node.kids[0] if node.kids else None
        else:
            node = node.child
    return depth


NODE_PAYLOADS = (
    (TBinaryProtocol, make_binary_node_payload),
    (TCompactProtocol, make_compact_node_payload),
    (TJSONProtocol, make_json_node_payload),
)


class RecursionDepthDynamicTest(unittest.TestCase):
    """Classes generated with py:dynamic have no read() or write() of their own.
    TBase hands them to the protocol's readStruct() and writeStruct() whenever
    the C extension does not take over, and that path must stop at the same
    depth as the code generated without the option."""

    def test_read_at_limit(self):
        for protocol, payload in NODE_PAYLOADS:
            with self.subTest(protocol=protocol.__name__):
                node = DynNode()
                node.read(protocol(TTransport.TMemoryBuffer(payload(LIMIT))))
                self.assertEqual(chain_depth(node), LIMIT)

    def test_read_over_limit(self):
        for protocol, payload in NODE_PAYLOADS:
            with self.subTest(protocol=protocol.__name__):
                with self.assertRaises(TProtocolException) as ctx:
                    DynNode().read(protocol(TTransport.TMemoryBuffer(payload(LIMIT + 1))))
                self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_read_list_at_limit(self):
        # DynTree has the wire layout of RecTree
        tree = DynTree()
        tree.read(TBinaryProtocol(TTransport.TMemoryBuffer(make_binary_payload(LIMIT))))
        self.assertEqual(chain_depth(tree), LIMIT)

    def test_read_list_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            DynTree().read(
                TBinaryProtocol(TTransport.TMemoryBuffer(make_binary_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_frozen_read_at_limit(self):
        node = FrozenDynNode.read(
            TBinaryProtocol(TTransport.TMemoryBuffer(make_binary_node_payload(LIMIT))))
        self.assertEqual(chain_depth(node), LIMIT)

    def test_frozen_read_over_limit(self):
        with self.assertRaises(TProtocolException) as ctx:
            FrozenDynNode.read(
                TBinaryProtocol(TTransport.TMemoryBuffer(make_binary_node_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_read_over_limit_accelerated_without_c_transport(self):
        """The accelerated protocol reads through readStruct() as well when the
        C extension cannot read from its transport."""
        proto = TBinaryProtocolAccelerated(
            TTransport.TFileObjectTransport(io.BytesIO(make_binary_node_payload(LIMIT + 1))))
        with self.assertRaises(TProtocolException) as ctx:
            DynNode().read(proto)
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)

    def test_depth_restored_after_refused_read(self):
        proto = TBinaryProtocol(TTransport.TMemoryBuffer(make_binary_node_payload(LIMIT + 1)))
        with self.assertRaises(TProtocolException):
            DynNode().read(proto)
        self.assertEqual(proto._recursion_depth, 0)
        proto.trans = TTransport.TMemoryBuffer(make_binary_node_payload(LIMIT))
        node = DynNode()
        node.read(proto)
        self.assertEqual(chain_depth(node), LIMIT)

    def test_write_at_limit(self):
        buf = TTransport.TMemoryBuffer()
        make_dyn_chain(LIMIT).write(TBinaryProtocol(buf))
        self.assertEqual(buf.getvalue(), make_binary_node_payload(LIMIT))

    def test_write_over_limit(self):
        proto = TBinaryProtocol(TTransport.TMemoryBuffer())
        with self.assertRaises(TProtocolException) as ctx:
            make_dyn_chain(LIMIT + 1).write(proto)
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)
        self.assertEqual(proto._recursion_depth, 0)

    def test_accelerated_read(self):
        try:
            import thrift.protocol.fastbinary  # noqa
        except ImportError:
            self.skipTest("fastbinary not built")
        node = DynNode()
        node.read(TBinaryProtocolAccelerated(
            TTransport.TMemoryBuffer(make_binary_node_payload(LIMIT))))
        self.assertEqual(chain_depth(node), LIMIT)
        with self.assertRaises(TProtocolException) as ctx:
            DynNode().read(TBinaryProtocolAccelerated(
                TTransport.TMemoryBuffer(make_binary_node_payload(LIMIT + 1))))
        self.assertEqual(ctx.exception.type, TProtocolException.DEPTH_LIMIT)


if __name__ == "__main__":
    unittest.main(verbosity=2)
