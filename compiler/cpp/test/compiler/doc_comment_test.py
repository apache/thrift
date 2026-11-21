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
"""Runs the compiler on files whose first doc comment holds nothing but
blanks or has a NUL byte in it, and checks that it compiles them. The compiler
unit tests call the same clean_up_doctext(); this test runs the built compiler
binary end to end."""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest


STRUCT = b"struct A {\n  1: i32 x\n}\n"


class TestDocComment(unittest.TestCase):

    THRIFT_COMPILER = None

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def assert_compiles(self, source):
        # Written as bytes: the NUL cases are not text a fixture file could hold.
        thrift_file = os.path.join(self.tmp_dir, "DocComment.thrift")
        with open(thrift_file, "wb") as f:
            f.write(source)
        result = subprocess.run(
            [self.THRIFT_COMPILER, "--gen", "cpp", "-o", self.tmp_dir, thrift_file],
            capture_output=True,
            timeout=60,
        )
        # A crash ends in a signal on POSIX (negative return code) and in an
        # NTSTATUS or abort() code on Windows.
        self.assertEqual(result.returncode, 0,
                         result.stdout.decode(errors="replace") +
                         result.stderr.decode(errors="replace"))
        self.assertTrue(os.path.exists(
            os.path.join(self.tmp_dir, "gen-cpp", "DocComment_types.h")))

    def test_blank_doc_comment(self):
        self.assert_compiles(b"/** */\n" + STRUCT)

    def test_nul_at_start_of_doc_comment(self):
        self.assert_compiles(b"/**\0*/\n" + STRUCT)

    def test_nul_as_second_character_of_doc_comment(self):
        self.assert_compiles(b"/**x\0*/\n" + STRUCT)

    def test_nul_leaving_doc_comment_blank(self):
        self.assert_compiles(b"/**   \0 x */\n" + STRUCT)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <path-to-thrift-compiler>", file=sys.stderr)
        sys.exit(1)
    TestDocComment.THRIFT_COMPILER = sys.argv[1]
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()
