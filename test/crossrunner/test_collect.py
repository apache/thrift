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

import os
import re
import subprocess
import sys
import unittest


class TestNameFiltering(unittest.TestCase):
    def dry_run(self, *args):
        test_dir = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
        return subprocess.run(
            [sys.executable, os.path.join(test_dir, 'test.py'), '--dry-run'] + list(args),
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)

    def test_exact_cross_test_name(self):
        args = ['--server', 'rb', '--client', 'rb']
        name = 'rb-rb_binary_buffered-ip'
        inventory = self.dry_run(*args)
        self.assertEqual(inventory.returncode, 0, inventory.stderr)
        self.assertIn(name, inventory.stdout.splitlines())

        selected = self.dry_run(*(args + ['--regex', '^' + re.escape(name) + '$']))
        self.assertEqual(selected.returncode, 0, selected.stderr)
        self.assertEqual(selected.stdout.splitlines(), [name])

    def test_exact_feature_test_name(self):
        args = ['--server', 'rb', '--features', '^theader_']
        name = 'rb-theader_unframed_binary_header_buffered-ip'
        inventory = self.dry_run(*args)
        self.assertEqual(inventory.returncode, 0, inventory.stderr)
        self.assertIn(name, inventory.stdout.splitlines())

        selected = self.dry_run(*(args + ['--regex', '^' + re.escape(name) + '$']))
        self.assertEqual(selected.returncode, 0, selected.stderr)
        self.assertEqual(selected.stdout.splitlines(), [name])

    def test_partial_name_pattern(self):
        selected = self.dry_run('--server', 'rb', '--client', 'rb',
                                '--regex', '_binary_buffered-')
        self.assertEqual(selected.returncode, 0, selected.stderr)
        self.assertCountEqual(selected.stdout.splitlines(), [
            'rb-rb_binary_buffered-domain',
            'rb-rb_binary_buffered-ip',
            'rb-rb_binary_buffered-ip-ssl',
        ])

    def test_regex_does_not_match_command_metadata(self):
        selected = self.dry_run('--server', 'rb', '--client', 'rb',
                                '--regex', 'TestClient')
        self.assertEqual(selected.returncode, 1, selected.stderr)
        self.assertEqual(selected.stdout, '')
        self.assertIn('No test found that matches the criteria', selected.stderr)


if __name__ == '__main__':
    unittest.main()
