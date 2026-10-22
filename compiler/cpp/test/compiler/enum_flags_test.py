#!/usr/bin/env python3
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
"""Tests for enum.flags in the Python and C# generators."""

import enum
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import Mock

runtime = Path(__file__).resolve().parents[4] / "lib/py/src/__init__.py"
spec = importlib.util.spec_from_file_location("thrift", runtime)
thrift = importlib.util.module_from_spec(spec)
sys.modules["thrift"] = thrift
spec.loader.exec_module(thrift)
from thrift.protocol import TBinaryProtocol, TCompactProtocol, TJSONProtocol
from thrift.transport import TTransport


class TestEnumFlags(unittest.TestCase):
    THRIFT_COMPILER = None
    FIXTURE = Path(__file__).with_name("EnumFlagsTest.thrift")

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.output = Path(self.directory.name)

    def generate(self, language):
        subprocess.run([self.THRIFT_COMPILER, "--gen", language,
                        "-out", str(self.output), str(self.FIXTURE)], check=True,
                       capture_output=True, text=True)

    def generate_schema(self, schema, options):
        fixture = self.output / "Schema.thrift"
        fixture.write_text(schema)
        subprocess.run([self.THRIFT_COMPILER, "--gen", options,
                        "-out", str(self.output), str(fixture)], check=True,
                       capture_output=True, text=True)
        source = (self.output / "Schema/ttypes.py").read_text()
        namespace = {}
        exec(compile(source, "ttypes.py", "exec"), namespace)
        return namespace

    def test_python_flag_names(self):
        schema = """
            enum Flag { A = 1 } (enum.flags)
            enum IntFlag { A = 1 } (enum.flags)
            enum _thrift_Flag { A = 1 } (enum.flags)
            enum _thrift_Flag_ { A = 1 } (enum.flags)
            enum Other { B = 2 } (enum.flags)
        """
        for options in ("py", "py:enum", "py:enum,type_hints"):
            with self.subTest(options=options):
                namespace = self.generate_schema(schema, options)
                for name in ("Flag", "IntFlag", "_thrift_Flag", "_thrift_Flag_", "Other"):
                    expected = enum.IntFlag if "type_hints" in options else enum.Flag
                    self.assertEqual(namespace[name].__bases__, (expected,))
                self.assertEqual(namespace["Other"].B.value, 2)

    def test_python_schema_evolution(self):
        schema = """
            enum Permissions { Read = 1, Write = 2, %s } (enum.flags)
            struct Message {
                1: Permissions flags,
                2: list<Permissions> values,
                3: map<Permissions, Permissions> mapping,
            }
        """
        for options in ("py", "py:enum", "py:enum,type_hints", "py:dynamic", "py:slots"):
            old = self.generate_schema(schema % "", options)
            new = self.generate_schema(schema % "Execute = 4, High = -2147483648,", options)
            for protocol in (TBinaryProtocol.TBinaryProtocol,
                             TCompactProtocol.TCompactProtocol,
                             TJSONProtocol.TJSONProtocol):
                for bits in (4, 5, -2147483648):
                    with self.subTest(options=options, protocol=protocol.__name__, bits=bits):
                        flag = new["Permissions"](bits)
                        message = new["Message"](flags=flag, values=[flag], mapping={flag: flag})
                        wire = TTransport.TMemoryBuffer()
                        message.write(protocol(wire))
                        older = old["Message"]()
                        older.read(protocol(TTransport.TMemoryBuffer(wire.getvalue())))
                        self.assertIsInstance(older.flags, old["Permissions"])
                        self.assertEqual(older.flags, old["Permissions"](bits))
                        self.assertEqual(older.flags.value, bits)
                        self.assertEqual(older.values[0].value, bits)
                        self.assertEqual(older.mapping[older.flags].value, bits)
                        self.assertEqual((older.flags | old["Permissions"].Read).value, bits | 1)
                        forwarded = TTransport.TMemoryBuffer()
                        older.write(protocol(forwarded))
                        restored = new["Message"]()
                        restored.read(protocol(TTransport.TMemoryBuffer(forwarded.getvalue())))
                        self.assertEqual(restored, message)

    def test_python_unknown_flag_hashes(self):
        schema = """
            enum Permissions { Read = 1, Write = 2 } (enum.flags)
            enum Other { Read = 1 } (enum.flags)
            struct Message {
                1: set<Permissions> values,
                2: map<Permissions, Permissions> mapping,
            }
        """
        for options in ("py", "py:enum", "py:enum,type_hints"):
            with self.subTest(options=options):
                ns = self.generate_schema(schema, options)
                flags = ns["Permissions"]
                values = [flags(bits) for bits in range(16, 528)]
                self.assertEqual(len({hash(value) for value in values}), len(values))
                self.assertEqual(flags(17), flags(17))
                self.assertFalse(flags(17) != flags(17))
                self.assertNotEqual(flags(17), flags(18))
                self.assertEqual(hash(flags.Read), hash(flags(1)))
                if "type_hints" not in options:
                    self.assertNotEqual(flags(17), 17)
                    self.assertNotEqual(flags.Read, ns["Other"].Read)
                message = ns["Message"](values=set(values), mapping={v: v for v in values})
                for protocol in (TBinaryProtocol.TBinaryProtocol,
                                 TCompactProtocol.TCompactProtocol,
                                 TJSONProtocol.TJSONProtocol):
                    wire = TTransport.TMemoryBuffer()
                    message.write(protocol(wire))
                    restored = ns["Message"]()
                    restored.read(protocol(TTransport.TMemoryBuffer(wire.getvalue())))
                    self.assertEqual(restored, message)
                    self.assertEqual(restored.mapping[flags(17)], flags(17))

    def test_python_unknown_flags_are_not_cached(self):
        for options in ("py", "py:enum", "py:enum,type_hints"):
            with self.subTest(options=options):
                ns = self.generate_schema(
                    "enum Permissions { Read = 1, Write = 2 } (enum.flags)", options)
                flags = ns["Permissions"]
                original = dict(flags._value2member_map_)
                for bits in range(16, 528):
                    self.assertEqual(flags(bits).value, bits)
                    self.assertEqual(flags(-bits).value, -bits)
                self.assertEqual(flags._value2member_map_, original)

    def test_python_clearing_flags_preserves_unknown_bits(self):
        schema = """
            enum Permissions { Read = 1, Write = 2, ReadWrite = 3 } (enum.flags)
            struct Message { 1: Permissions flags }
        """
        for options in ("py", "py:enum", "py:enum,type_hints"):
            with self.subTest(options=options):
                ns = self.generate_schema(schema, options)
                flags = ns["Permissions"]
                self.assertEqual((flags(17) & ~flags.Read).value, 16)
                self.assertEqual(~flags.Read & flags.ReadWrite, flags.Write)
                self.assertEqual((~~flags.Read), flags.Read)
                self.assertEqual((flags(17) ^ flags.Read).value, 16)
                self.assertEqual((flags(17) | flags.Write).value, 19)
                for protocol in (TBinaryProtocol.TBinaryProtocol,
                                 TCompactProtocol.TCompactProtocol,
                                 TJSONProtocol.TJSONProtocol):
                    message = ns["Message"](flags=flags(17) & ~flags.Read)
                    wire = TTransport.TMemoryBuffer()
                    message.write(protocol(wire))
                    restored = ns["Message"]()
                    restored.read(protocol(TTransport.TMemoryBuffer(wire.getvalue())))
                    self.assertEqual(restored.flags.value, 16)

    def test_python_immutable_flags_accept_integers(self):
        schema = """
            enum Permissions { Read = 1, Write = 2 } (enum.flags)
            typedef Permissions Alias
            struct Frozen {
                1: optional Permissions flags,
                2: optional Alias alias,
            } (python.immutable)
            exception Failure {
                1: optional Permissions flags,
                2: optional Alias alias,
            }
        """
        for options in ("py:enum", "py:enum,type_hints", "py:enum,slots", "py:enum,dynamic"):
            ns = self.generate_schema(schema, options)
            flags = ns["Permissions"]
            for name in ("Frozen", "Failure"):
                for value in (None, 1, 5, flags.Read, flags(5)):
                    with self.subTest(options=options, structure=name, value=value):
                        message = ns[name](flags=value, alias=value)
                        expected = None if value is None else flags(value)
                        self.assertEqual(message.flags, expected)
                        self.assertEqual(message.alias, expected)
                        for protocol in (TBinaryProtocol.TBinaryProtocol,
                                         TCompactProtocol.TCompactProtocol,
                                         TJSONProtocol.TJSONProtocol):
                            wire = TTransport.TMemoryBuffer()
                            message.write(protocol(wire))
                            restored = ns[name].read(
                                protocol(TTransport.TMemoryBuffer(wire.getvalue())))
                            self.assertEqual(restored.flags, expected)
                            self.assertEqual(restored.alias, expected)

    def test_netstd(self):
        self.generate("netstd")
        flags = (self.output / "EnumFlagsTest/Permissions.cs").read_text()
        ordinary = (self.output / "EnumFlagsTest/Ordinary.cs").read_text()
        self.assertIn("[Flags]", flags)
        self.assertNotIn("[Flags]", ordinary)
        self.assertIn("ReadWrite = 3,", flags)

    def test_python(self):
        for options in ("py", "py:enum", "py:enum,type_hints", "py:dynamic", "py:slots"):
            with self.subTest(options=options):
                self.generate(options)
                namespace = {}
                source = (self.output / "EnumFlagsTest/ttypes.py").read_text()
                exec(compile(source, "ttypes.py", "exec"), namespace)
                flags = namespace["Permissions"]
                expected = enum.IntFlag if "type_hints" in options else enum.Flag
                self.assertEqual(flags.__bases__, (expected,))
                combined = flags.Read | flags.Execute
                self.assertEqual(combined.value, 5)
                self.assertEqual(flags.Read | flags.Write, flags.ReadWrite)
                self.assertEqual(flags(0).value, 0)
                with self.assertRaises(ValueError):
                    flags("invalid")
                ordinary = namespace["Ordinary"]
                if "enum" in options:
                    self.assertEqual(ordinary.__bases__, (enum.IntEnum,))
                else:
                    self.assertEqual(ordinary.First, 1)
                    self.assertEqual(ordinary._VALUES_TO_NAMES[1], "First")
                self.assertIn("DEFAULT_PERMISSIONS = Permissions(1)",
                              (self.output / "EnumFlagsTest/constants.py").read_text())
                container = namespace["FlagsContainer"]
                self.assertEqual(container().permissions, flags.Read)
                for protocol in (TBinaryProtocol.TBinaryProtocol,
                                 TCompactProtocol.TCompactProtocol,
                                 TJSONProtocol.TJSONProtocol):
                    value = container(permissions=combined,
                                      values=[flags(0), flags.ReadWrite, combined],
                                      alias=combined)
                    frozen = namespace["FrozenFlags"](permissions=combined)
                    nested = namespace["NestedFlags"](
                        flags=value, mapping={flags.Read: combined}, values={combined})
                    for original in (value, frozen, nested):
                        with self.subTest(protocol=protocol.__name__,
                                          structure=type(original).__name__):
                            transport = TTransport.TMemoryBuffer()
                            writer = protocol(transport)
                            writer._fast_encode = Mock(side_effect=AssertionError("fast encode"))
                            original.write(writer)
                            writer._fast_encode.assert_not_called()
                            reader = protocol(TTransport.TMemoryBuffer(transport.getvalue()))
                            reader._fast_decode = Mock(side_effect=AssertionError("fast decode"))
                            if original is frozen:
                                restored = type(original).read(reader)
                            else:
                                restored = type(original)()
                                restored.read(reader)
                            reader._fast_decode.assert_not_called()
                            self.assertEqual(restored, original)


if __name__ == "__main__":
    TestEnumFlags.THRIFT_COMPILER = os.path.abspath(sys.argv.pop(1))
    unittest.main()
