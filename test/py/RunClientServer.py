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

from __future__ import division
import time
import subprocess
import sys
import os
import signal
from optparse import OptionParser

parser = OptionParser()
parser.add_option("--port", type="int", dest="port", default=9090,
    help="port number for server to listen on")
options, args = parser.parse_args()

FRAMED = ["TNonblockingServer"]
EXTRA_DELAY = ['TProcessPoolServer']
EXTRA_SLEEP = 3.5

PROTOS= [
    'accel',
    'binary',
    'compact' ]

SERVERS = [
  "TSimpleServer",
  "TThreadedServer",
  "TThreadPoolServer",
  "TProcessPoolServer", # new!
  "TForkingServer",
  "TNonblockingServer",
  "THttpServer" ]

# Test for presence of multiprocessing module, and if it is not present, then
# remove it from the list of available servers.
try:
  import multiprocessing
except:
  print 'Warning: the multiprocessing module is unavailable. Skipping tests for TProcessPoolServer'
  SERVERS.remove('TProcessPoolServer')


# commandline permits a single class name to be specified to override SERVERS=[...]
if len(args) == 1:
  if args[0] in SERVERS:
    SERVERS = args
  else:
    print 'Unavailable server type "%s", please choose one of: %s' % (args[0], SERVERS)
    sys.exit(0)


def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

def runTest(server_class, proto, port):
    server_args = [sys.executable, # /usr/bin/python or similar
      relfile('TestServer.py'), # ./TestServer.py
      '--proto=%s' % proto, # accel, binary or compact
      '--port=%d' % port, # usually 9090, given on cmdline
      server_class] # name of class to test, from SERVERS[] or cmdline
    print "Testing server %s: %s" % (server_class, ' '.join(server_args))
    serverproc = subprocess.Popen(server_args)
    time.sleep(0.25)
    try:
        argv = [sys.executable, relfile("TestClient.py"),
           '--proto=%s' % (proto), '--port=%d' % (port) ]
        if server_class in FRAMED:
            argv.append('--framed')
        if server_class == 'THttpServer':
            argv.append('--http=/')
        print 'Testing client %s: %s' % (server_class, ' '.join(argv))
        ret = subprocess.call(argv)
        if ret != 0:
            raise Exception("subprocess %s failed, args: %s" % (server_class, ' '.join(argv)))
    finally:
        # check that server didn't die
        time.sleep(0.05)
        serverproc.poll()
        if serverproc.returncode is not None:
          print 'Server process (%s) failed with retcode %d' % (' '.join(server_args), serverproc.returncode)
          raise Exception('subprocess %s died, args: %s' % (server_class, ' '.join(server_args)))
        else:
          if server_class in EXTRA_DELAY:
            print 'Giving %s (proto=%s) an extra %d seconds for child processes to terminate via alarm' % (server_class, proto, EXTRA_SLEEP)
            time.sleep(EXTRA_SLEEP)
          os.kill(serverproc.pid, signal.SIGKILL)
    # wait for shutdown
    time.sleep(0.5)

for try_server in SERVERS:
  for try_proto in PROTOS:
    runTest(try_server, try_proto, options.port)
