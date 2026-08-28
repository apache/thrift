#!/usr/bin/env python
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

import importlib
import os
import subprocess
import sys
import tempfile

TEST_DIR = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
sys.path.insert(0, TEST_DIR)

import _import_local_thrift  # noqa: E402,F401
from test_keyword_escape import find_thrift  # noqa: E402


def test_default_struct_value():
    thrift_file = os.path.join(os.path.dirname(__file__), 'Thrift4623.thrift')
    thrift_bin = find_thrift()
    if not thrift_bin:
        print("WARNING: thrift compiler not found, skipping test")
        return 0

    with tempfile.TemporaryDirectory() as tmpdir:
        result = subprocess.run(
            [thrift_bin, '-gen', 'py', '-out', tmpdir, thrift_file],
            capture_output=True, text=True)
        if result.returncode != 0:
            raise AssertionError("thrift compiler failed: " + result.stderr)

        sys.path.insert(0, tmpdir)
        try:
            types = importlib.import_module('thrift4623.ttypes')
            first = types.B()
            second = types.B()

            assert isinstance(first.itm, types.A)
            assert isinstance(first.itm.nested, types.C)
            assert first.itm.nested.value is None
            assert first.itm is not second.itm
            assert first.itm.nested is not second.itm.nested
            assert types.B(itm=None).itm is None
        finally:
            sys.path.pop(0)
            for name in list(sys.modules):
                if name == 'thrift4623' or name.startswith('thrift4623.'):
                    del sys.modules[name]

    print('OK: default-valued struct fields are usable')


if __name__ == '__main__':
    test_default_struct_value()
