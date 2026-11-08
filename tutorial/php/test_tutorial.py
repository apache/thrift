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

"""Run the generated PHP tutorial client against both HTTP server entry points."""

import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import time
import unittest


TUTORIAL_DIR = Path(__file__).resolve().parent
PHP = os.environ.get("PHP", "php")


class TutorialTest(unittest.TestCase):
    def check_server(self, command):
        # PhpClient.php connects to localhost:8080. Do not accidentally test an
        # unrelated server, or stop a server that this test did not start.
        try:
            connection = socket.create_connection(("localhost", 8080), timeout=0.2)
        except ConnectionRefusedError:
            pass
        else:
            connection.close()
            self.fail("Port 8080 is already in use")

        with tempfile.TemporaryFile(mode="w+") as log:
            server = subprocess.Popen(
                command, cwd=TUTORIAL_DIR, stdout=log, stderr=log,
            )
            try:
                deadline = time.monotonic() + 10
                while server.poll() is None and time.monotonic() < deadline:
                    try:
                        with socket.create_connection(("localhost", 8080), timeout=0.2):
                            break
                    except OSError:
                        time.sleep(0.05)
                else:
                    log.seek(0)
                    self.fail("Tutorial server did not start:\n" + log.read())

                client = subprocess.run(
                    [PHP, "PhpClient.php", "--http"], cwd=TUTORIAL_DIR,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    text=True, timeout=15,
                )
                log.seek(0)
                diagnostic = client.stderr + "\nServer output:\n" + log.read()
                self.assertEqual(client.returncode, 0, diagnostic)
                # The tutorial client catches TException and can exit zero on
                # failure, so check the actual RPC results as well.
                self.assertEqual(client.stdout.splitlines(), [
                    "ping()",
                    "1+1=2",
                    "InvalidOperation: Cannot divide by 0",
                    "15-10=5",
                    "Log: PHP is stateless!",
                ], diagnostic)
            finally:
                server.terminate()
                try:
                    server.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    server.kill()
                    server.wait()

    def test_builtin_server(self):
        self.check_server([PHP, "-S", "localhost:8080", "-t", str(TUTORIAL_DIR.parent)])

    def test_runserver(self):
        self.check_server([sys.executable, str(TUTORIAL_DIR / "runserver.py")])


if __name__ == "__main__":
    unittest.main()
