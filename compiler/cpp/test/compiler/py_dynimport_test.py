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
"""Tests for the dynimport option of the Python generator."""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest


class TestPyDynimport(unittest.TestCase):

    THRIFT_COMPILER = None
    IMPORT_LINE = "from mybase import MyBase"
    SCHEMA = """
namespace py dynimporttest

struct Item {
  1: i32 id
}

service Svc {
  void ping(1: Item item)
}
"""

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def test_dynimport_is_a_line_of_its_own(self):
        fixture = os.path.join(self.tmp_dir, "DynImport.thrift")
        with open(fixture, "w") as f:
            f.write(self.SCHEMA)
        subprocess.run(
            [self.THRIFT_COMPILER, "--gen",
             "py:dynamic,dynbase=MyBase,dynimport=" + self.IMPORT_LINE,
             "-out", self.tmp_dir, fixture],
            check=True,
            capture_output=True,
            text=True,
        )
        for module in ("ttypes.py", "Svc.py"):
            with self.subTest(module=module):
                path = os.path.join(self.tmp_dir, "dynimporttest", module)
                with open(path) as f:
                    source = f.read()
                lines = [line for line in source.splitlines()
                         if line.startswith(self.IMPORT_LINE)]
                self.assertEqual(lines, [self.IMPORT_LINE])
                compile(source, path, "exec")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <path-to-thrift-compiler>", file=sys.stderr)
        sys.exit(1)
    TestPyDynimport.THRIFT_COMPILER = sys.argv[1]
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()
