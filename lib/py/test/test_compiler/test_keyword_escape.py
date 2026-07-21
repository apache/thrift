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

import ast
import keyword
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
    possible_build_thrift = os.path.join(
        os.path.dirname(__file__), '..', '..', '..', '..',
        'build', 'compiler', 'cpp', 'bin', 'thrift')
    if os.path.exists(possible_build_thrift):
        return possible_build_thrift

    # Check for cpp compiler location
    possible_cpp_thrift = os.path.join(
        os.path.dirname(__file__), '..', '..', '..', '..',
        'compiler', 'cpp', 'thrift')
    if os.path.exists(possible_cpp_thrift):
        return possible_cpp_thrift

    return None


def find_unescaped_all_entries(init_files):
    """
    __all__ = ['ttypes', 'constants', 'continue', ...] is a plain string
    list, so a keyword-colliding entry compiles fine -- py_compile can't
    see the bug. It only surfaces at "from package import *" time, where
    Python tries to import the listed name as a submodule and fails
    because the actual (escaped) filename doesn't match. Check statically
    instead of needing a real import.
    """
    problems = []
    for init_file in init_files:
        with open(init_file) as f:
            try:
                tree = ast.parse(f.read(), filename=init_file)
            except SyntaxError:
                continue  # already reported by the compile check
        for node in ast.walk(tree):
            if not (isinstance(node, ast.Assign) and
                    any(isinstance(t, ast.Name) and t.id == '__all__' for t in node.targets) and
                    isinstance(node.value, ast.List)):
                continue
            for elt in node.value.elts:
                if isinstance(elt, ast.Constant) and isinstance(elt.value, str):
                    name = elt.value
                    if not name.isidentifier() or keyword.iskeyword(name):
                        problems.append((init_file, node.lineno, name))
    return problems


def find_keyword_literal_except_types(py_files):
    """
    "True"/"False"/"None" are valid Python expression atoms (unlike most
    other reserved words), so an unescaped "except True as e:" parses fine
    -- it just silently tries to catch the literal bool/None instead of the
    intended exception class, and would raise a runtime TypeError once hit.
    py_compile can't see this; walk the AST instead.
    """
    problems = []
    for py_file in py_files:
        with open(py_file) as f:
            try:
                tree = ast.parse(f.read(), filename=py_file)
            except SyntaxError:
                continue  # already reported by the compile check
        for node in ast.walk(tree):
            if isinstance(node, ast.ExceptHandler) and isinstance(node.type, ast.Constant):
                problems.append((py_file, node.lineno, repr(node.type.value)))
    return problems


def test_keyword_escape_compilation():
    """
    Test that the Python generator produces valid Python code
    when the Thrift schema contains Python keywords.

    This verifies THRIFT-5927: Python code generator should escape
    identifiers that are Python reserved keywords.
    """
    thrift_file = os.path.join(os.path.dirname(__file__), 'Thrift5927.thrift')

    if not os.path.exists(thrift_file):
        print("ERROR: Test file not found: " + thrift_file)
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
            print("ERROR: thrift compiler failed")
            print("stdout: " + result.stdout)
            print("stderr: " + result.stderr)
            return 1

        py_files = glob.glob(os.path.join(tmpdir, '**', '*.py'), recursive=True)
        # The generated "<service>-remote" CLI helper script is Python too, but has no
        # .py suffix (matching every other language's convention for this file), so it
        # needs its own glob to be included in the compile check below.
        remote_files = glob.glob(os.path.join(tmpdir, '**', '*-remote'), recursive=True)
        all_files = py_files + remote_files

        if not all_files:
            print("ERROR: No Python files generated")
            return 1

        failed = []
        for py_file in all_files:
            try:
                py_compile.compile(py_file, doraise=True)
            except py_compile.PyCompileError as e:
                failed.append((py_file, str(e)))

        if failed:
            print("ERROR: Generated Python files have syntax errors:")
            for file_path, error in failed:
                print("  " + file_path + ": " + error)
            return 1

        init_files = glob.glob(os.path.join(tmpdir, '**', '__init__.py'), recursive=True)
        bad_all_entries = find_unescaped_all_entries(init_files)
        if bad_all_entries:
            print("ERROR: __all__ lists a name that isn't a valid, non-keyword identifier"
                  " (breaks \"from package import *\"):")
            for file_path, lineno, name in bad_all_entries:
                print("  " + file_path + ":" + str(lineno) + ": " + repr(name))
            return 1

        keyword_literal_excepts = find_keyword_literal_except_types(py_files)
        if keyword_literal_excepts:
            print("ERROR: Generated code catches a keyword literal instead of an exception class:")
            for file_path, lineno, value in keyword_literal_excepts:
                print("  " + file_path + ":" + str(lineno) + ": except " + value + " as ...")
            return 1

        print("OK: All " + str(len(all_files)) + " generated Python files compile successfully")
        return 0


if __name__ == '__main__':
    sys.exit(test_keyword_escape_compilation())
