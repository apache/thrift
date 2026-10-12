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
"""Regression tests for errors a generator raises while generating code."""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest


class TestGeneratorError(unittest.TestCase):

    CURRENT_DIR = os.path.dirname(os.path.realpath(__file__))
    THRIFT_COMPILER = None
    FIXTURE_THRIFT = os.path.join(CURRENT_DIR, "GeneratorError.thrift")

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def test_std_exception_from_generator_is_reported_as_failure(self):
        result = subprocess.run(
            [self.THRIFT_COMPILER, "--gen", "c_glib", "-o", self.tmp_dir,
             self.FIXTURE_THRIFT],
            capture_output=True,
            text=True,
        )
        # A crash ends in a signal on POSIX (negative return code) and in an
        # NTSTATUS or abort() code on Windows; failure() always exits with 1.
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("[FAILURE:", result.stderr)
        self.assertIn("no C base type name for base type uuid", result.stderr)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <path-to-thrift-compiler>", file=sys.stderr)
        sys.exit(1)
    TestGeneratorError.THRIFT_COMPILER = sys.argv[1]
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()
