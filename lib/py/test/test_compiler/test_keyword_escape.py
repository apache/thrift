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

import os
import sys
import subprocess
import tempfile
import py_compile
import glob
import shutil


def find_thrift():
    """Find the thrift compiler."""
    # Check THRIFT environment variable (set by build system)
    if 'THRIFT' in os.environ:
        thrift_path = os.environ['THRIFT']
        if os.path.exists(thrift_path):
            return thrift_path
    
    # Check common locations in PATH
    paths = [
        'thrift',
        '/usr/local/bin/thrift',
        '/usr/bin/thrift',
    ]
    
    for path in paths:
        if shutil.which(path):
            return path
    
    # Check if we're in the source tree with built compiler
    possible_build_thrift = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'build', 'compiler', 'cpp', 'bin', 'thrift')
    if os.path.exists(possible_build_thrift):
        return possible_build_thrift
    
    # Check for cpp compiler location
    possible_cpp_thrift = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'compiler', 'cpp', 'thrift')
    if os.path.exists(possible_cpp_thrift):
        return possible_cpp_thrift
    
    return None


def test_keyword_escape_compilation():
    """
    Test that the Python generator produces valid Python code
    when the Thrift schema contains Python keywords.
    
    This verifies THRIFT-5927: Python code generator should escape
    identifiers that are Python reserved keywords.
    """
    thrift_file = os.path.join(os.path.dirname(__file__), 'Thrift5927.thrift')
    
    if not os.path.exists(thrift_file):
        print(f"ERROR: Test file not found: {thrift_file}")
        return 1
    
    thrift_bin = find_thrift()
    if not thrift_bin:
        print("WARNING: thrift compiler not found, skipping test")
        print("(In CI, thrift should be available via THRIFT env var)")
        return 0  # Skip gracefully rather than fail
    
    with tempfile.TemporaryDirectory() as tmpdir:
        result = subprocess.run(
            [thrift_bin, '-r', '-gen', 'py', '-out', tmpdir, thrift_file],
            capture_output=True,
            text=True
        )
        
        if result.returncode != 0:
            print(f"ERROR: thrift compiler failed")
            print(f"stdout: {result.stdout}")
            print(f"stderr: {result.stderr}")
            return 1
        
        py_files = glob.glob(os.path.join(tmpdir, '**', '*.py'), recursive=True)
        
        if not py_files:
            print("ERROR: No Python files generated")
            return 1
        
        failed = []
        for py_file in py_files:
            try:
                py_compile.compile(py_file, doraise=True)
            except py_compile.PyCompileError as e:
                failed.append((py_file, str(e)))
        
        if failed:
            print("ERROR: Generated Python files have syntax errors:")
            for file_path, error in failed:
                print(f"  {file_path}: {error}")
            return 1
        
        print(f"OK: All {len(py_files)} generated Python files compile successfully")
        return 0


if __name__ == '__main__':
    sys.exit(test_keyword_escape_compilation())