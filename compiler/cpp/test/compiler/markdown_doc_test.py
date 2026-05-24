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
"""
Tests for the Markdown generator:
  - Output files are given the default .md extension.
  - @param and @return tags in function doc-comments are rendered as a table.
  - @-signs embedded in ordinary prose are not treated as tags.
"""
import os
import shutil
import subprocess
import sys
import tempfile
import unittest


class TestMarkdownDocRendering(unittest.TestCase):

    CURRENT_DIR = os.path.dirname(os.path.realpath(__file__))
    THRIFT_COMPILER = None
    FIXTURE_THRIFT = os.path.join(CURRENT_DIR, "DocTest.thrift")
    GOLDEN_MD = os.path.join(CURRENT_DIR, "DocTest.md")

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.tmp_dir, ignore_errors=True)

    def _run_compiler(self, gen_arg="markdown"):
        cmd = [
            TestMarkdownDocRendering.THRIFT_COMPILER,
            "--gen", gen_arg,
            "-o", self.tmp_dir,
            TestMarkdownDocRendering.FIXTURE_THRIFT,
        ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0,
                         f"Compiler failed:\n{result.stderr}")

    def test_default_md_extension(self):
        """Generated file should use the .md extension by default."""
        self._run_compiler()
        out_file = os.path.join(self.tmp_dir, "gen-markdown", "DocTest.md")
        self.assertTrue(os.path.exists(out_file),
                        "Expected gen-markdown/DocTest.md but file not found")

    def test_suffix_override(self):
        """suffix= option should allow overriding the default extension."""
        self._run_compiler(gen_arg="markdown:suffix=html")
        out_file = os.path.join(self.tmp_dir, "gen-markdown", "DocTest.html")
        self.assertTrue(os.path.exists(out_file),
                        "Expected gen-markdown/DocTest.html but file not found")

    def test_suffix_empty_removes_extension(self):
        """suffix= (empty) should produce files with no extension."""
        self._run_compiler(gen_arg="markdown:suffix=")
        out_file = os.path.join(self.tmp_dir, "gen-markdown", "DocTest")
        self.assertTrue(os.path.exists(out_file),
                        "Expected gen-markdown/DocTest (no extension) but file not found")

    def test_output_matches_golden(self):
        """Full output must match the checked-in golden file."""
        self._run_compiler()
        actual_path = os.path.join(self.tmp_dir, "gen-markdown", "DocTest.md")
        with open(actual_path, "r", encoding="utf-8") as fh:
            actual = fh.read()
        with open(TestMarkdownDocRendering.GOLDEN_MD, "r", encoding="utf-8") as fh:
            expected = fh.read()
        self.assertEqual(actual, expected,
                         "Markdown output does not match golden file")

    def test_inline_at_sign_not_treated_as_tag(self):
        """@-signs embedded in prose must not trigger tag parsing."""
        self._run_compiler()
        actual_path = os.path.join(self.tmp_dir, "gen-markdown", "DocTest.md")
        with open(actual_path, "r", encoding="utf-8") as fh:
            content = fh.read()
        # The 'plain' function has '@param' in its prose; it should NOT produce a table row
        plain_section = content[content.find("Function: DocTest.plain"):]
        plain_section = plain_section[:plain_section.find("Function: DocTest.withbare")]
        self.assertNotIn("| Name | Description |", plain_section,
                         "@-sign in prose should not trigger table rendering")
        self.assertIn("@param", plain_section,
                      "Prose @param should pass through unchanged")

    def test_param_table_rendered(self):
        """@param and @return tags should produce a Markdown table."""
        self._run_compiler()
        actual_path = os.path.join(self.tmp_dir, "gen-markdown", "DocTest.md")
        with open(actual_path, "r", encoding="utf-8") as fh:
            content = fh.read()
        compute_section = content[content.find("Function: DocTest.compute"):]
        compute_section = compute_section[:compute_section.find("Function: DocTest.plain")]
        self.assertIn("| Name | Description |", compute_section)
        self.assertIn("| `i32 x`", compute_section)
        self.assertIn("| `string label`", compute_section)
        self.assertIn("**Returns** `i32`", compute_section)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <path-to-thrift-compiler>", file=sys.stderr)
        sys.exit(1)
    TestMarkdownDocRendering.THRIFT_COMPILER = sys.argv[1]
    # Strip the custom argument so unittest doesn't see it
    sys.argv = [sys.argv[0]] + sys.argv[2:]
    unittest.main()
