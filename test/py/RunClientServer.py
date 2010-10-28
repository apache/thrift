#!/usr/bin/env python

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

import time
import subprocess
import sys
import os
import signal

def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

FRAMED = ["TNonblockingServer"]

def runTest(server_class):
    print "Testing ", server_class
    serverproc = subprocess.Popen([sys.executable, relfile("TestServer.py"), server_class])
    time.sleep(0.25)
    try:
        argv = [sys.executable, relfile("TestClient.py")]
        if server_class in FRAMED:
            argv.append('--framed')
        if server_class == 'THttpServer':
            argv.append('--http=/')
        ret = subprocess.call(argv)
        if ret != 0:
            raise Exception("subprocess failed")
    finally:
        # fixme: should check that server didn't die
        os.kill(serverproc.pid, signal.SIGKILL)

    # wait for shutdown
    time.sleep(1)

map(runTest, [
  "TSimpleServer",
  "TThreadedServer",
  "TThreadPoolServer",
  "TForkingServer",
  "TNonblockingServer",
  "THttpServer",
  ])
