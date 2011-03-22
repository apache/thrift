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
parser.add_option('-v', '--verbose', action="store_const", 
    dest="verbose", const=2,
    help="verbose output")
parser.add_option('-q', '--quiet', action="store_const", 
    dest="verbose", const=0,
    help="minimal output")
parser.set_defaults(verbose=1)
options, args = parser.parse_args()

FRAMED = ["TNonblockingServer"]
SKIP_ZLIB = ['TNonblockingServer', 'THttpServer']
SKIP_SSL = ['TNonblockingServer', 'THttpServer']
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

try:
  import ssl
except:
  print 'Warning, no ssl module available. Skipping all SSL tests.'
  SKIP_SSL.extend(SERVERS)

# commandline permits a single class name to be specified to override SERVERS=[...]
if len(args) == 1:
  if args[0] in SERVERS:
    SERVERS = args
  else:
    print 'Unavailable server type "%s", please choose one of: %s' % (args[0], SERVERS)
    sys.exit(0)


def relfile(fname):
    return os.path.join(os.path.dirname(__file__), fname)

def runTest(server_class, proto, port, use_zlib, use_ssl):
  # Build command line arguments
  server_args = [sys.executable, relfile('TestServer.py') ]
  cli_args = [sys.executable, relfile('TestClient.py') ]
  for which in (server_args, cli_args):
    which.append('--proto=%s' % proto) # accel, binary or compact
    which.append('--port=%d' % port) # default to 9090
    if use_zlib:
      which.append('--zlib')
    if use_ssl:
      which.append('--ssl')
    if options.verbose == 0:
      which.append('-q')
    if options.verbose == 2:
      which.append('-v')
  # server-specific option to select server class
  server_args.append(server_class)
  # client-specific cmdline options
  if server_class in FRAMED:
    cli_args.append('--framed')
  if server_class == 'THttpServer':
    cli_args.append('--http=/')
  if options.verbose > 0:
    print 'Testing server %s: %s' % (server_class, ' '.join(server_args))
  serverproc = subprocess.Popen(server_args)
  time.sleep(0.2)
  try:
    if options.verbose > 0:
      print 'Testing client: %s' % (' '.join(cli_args))
    ret = subprocess.call(cli_args)
    if ret != 0:
      raise Exception("Client subprocess failed, retcode=%d, args: %s" % (ret, ' '.join(cli_args)))
  finally:
    # check that server didn't die
    serverproc.poll()
    if serverproc.returncode is not None:
      print 'FAIL: Server process (%s) failed with retcode %d' % (' '.join(server_args), serverproc.returncode)
      raise Exception('Server subprocess %s died, args: %s' % (server_class, ' '.join(server_args)))
    else:
      if server_class in EXTRA_DELAY:
        if options.verbose > 0:
          print 'Giving %s (proto=%s,zlib=%s,ssl=%s) an extra %d seconds for child processes to terminate via alarm' % (server_class,
                proto, use_zlib, use_ssl, EXTRA_SLEEP)
        time.sleep(EXTRA_SLEEP)
      os.kill(serverproc.pid, signal.SIGKILL)
  # wait for shutdown
  time.sleep(0.1)

test_count = 0
for try_server in SERVERS:
  for try_proto in PROTOS:
    for with_zlib in (False, True):
      # skip any servers that don't work with the Zlib transport
      if with_zlib and try_server in SKIP_ZLIB:
        continue
      for with_ssl in (False, True):
        # skip any servers that don't work with SSL
        if with_ssl and try_server in SKIP_SSL:
          continue
        test_count += 1
        if options.verbose > 0:
          print '\nTest run #%d:  Server=%s,  Proto=%s,  zlib=%s,  SSL=%s' % (test_count, try_server, try_proto, with_zlib, with_ssl)
        runTest(try_server, try_proto, options.port, with_zlib, with_ssl)
        if options.verbose > 0:
          print 'OK: Finished  %s / %s proto / zlib=%s / SSL=%s.   %d combinations tested.' % (try_server, try_proto, with_zlib, with_ssl, test_count)
