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
"""Runs the compiler on typedefs that cannot be resolved, with more than one
generator, and checks that it fails before any generator writes a file."""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest


class TestUnresolvedTypedef(unittest.TestCase):

    THRIFT_COMPILER = None

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()
        self.out_dir = os.path.join(self.tmp_dir, "out")
        os.mkdir(self.out_dir)

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def compile(self, source):
        thrift_file = os.path.join(self.tmp_dir, "Unresolved.thrift")
        with open(thrift_file, "w") as f:
            f.write(source)
        return subprocess.run(
            [self.THRIFT_COMPILER, "--gen", "cpp", "--gen", "py",
             "-o", self.out_dir, thrift_file],
            capture_output=True,
            text=True,
            timeout=60,
        )

    def assert_fails_without_output(self, source, message):
        result = self.compile(source)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn(message, result.stdout + result.stderr)
        self.assertEqual(os.listdir(self.out_dir), [])

    def test_undefined_target(self):
        self.assert_fails_without_output(
            "typedef Missing T\nstruct S { 1: T t }\n",
            'Type "Missing" not defined')

    def test_typedef_of_itself(self):
        self.assert_fails_without_output(
            "typedef T T\nstruct S { 1: T t }\n",
            'Type "T" refers to itself')


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <path-to-thrift-compiler>", file=sys.stderr)
        sys.exit(1)
    TestUnresolvedTypedef.THRIFT_COMPILER = sys.argv[1]
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()
