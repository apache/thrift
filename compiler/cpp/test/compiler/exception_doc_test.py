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
"""Regression tests for exception documentation generated for service methods."""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest


class TestExceptionDocRendering(unittest.TestCase):

    CURRENT_DIR = os.path.dirname(os.path.realpath(__file__))
    THRIFT_COMPILER = None
    FIXTURE_THRIFT = os.path.join(CURRENT_DIR, "ExceptionDocTest.thrift")

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def _generate(self, language, relative_path, output_dir=None):
        result = subprocess.run(
            [self.THRIFT_COMPILER, "--gen", language, "-o", self.tmp_dir,
             self.FIXTURE_THRIFT],
            capture_output=True,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        generated_dir = "gen-" + language if output_dir is None else output_dir
        path = os.path.join(self.tmp_dir, generated_dir, relative_path)
        with open(path, "r", encoding="utf-8") as generated:
            return generated.read()

    def test_java_uses_escaped_qualified_exception_names(self):
        content = self._generate("java", "com/main/ExceptionDocTest.java")
        self.assertIn("@throws com.main.$native First line of the explanation", content)
        self.assertIn("second line uses a < b && c > d", content)

    def test_kotlin_keeps_kdoc_params_unchanged_and_escapes_names(self):
        content = self._generate("kotlin", "com/main/ExceptionDocTest.kt", output_dir="")
        self.assertIn("@throws com.main.`object` Kotlin reserved identifier.", content)
        self.assertNotIn("@param", content)

    def test_cpp_uses_canonical_qualified_type_name(self):
        content = self._generate("cpp", "ExceptionDocTest.h")
        self.assertIn("@throws  ::main_ns::native First line of the explanation", content)

    def test_haxe_omits_throws_field_name(self):
        content = self._generate("haxe", "com/main/ExceptionDocTest.hx")
        self.assertIn("@throws Native First line of the explanation", content)
        self.assertNotIn("Native native_error", content)

    def test_netstd_escapes_exception_documentation_as_xml(self):
        content = self._generate("netstd", "Main/Ns/ExceptionDocTest.cs")
        self.assertIn("Method documentation uses x &lt; y &amp;&amp; y &gt; z.", content)
        self.assertIn("Parameter documentation uses a &lt; b &amp;&amp; c &gt; d.", content)
        self.assertIn(
            '<exception cref="global::Main.Ns.@native">First line of the explanation',
            content,
        )
        self.assertIn("second line uses a &lt; b &amp;&amp; c &gt; d</exception>", content)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <path-to-thrift-compiler>", file=sys.stderr)
        sys.exit(1)
    TestExceptionDocRendering.THRIFT_COMPILER = sys.argv[1]
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()
