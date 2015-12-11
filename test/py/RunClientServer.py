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
from __future__ import print_function
import copy
import glob
import os
import signal
import socket
import subprocess
import sys
import time
from optparse import OptionParser

SCRIPT_DIR = os.path.abspath(os.path.dirname(__file__))
ROOT_DIR = os.path.dirname(os.path.dirname(SCRIPT_DIR))
DEFAULT_LIBDIR_GLOB = os.path.join(ROOT_DIR, 'lib', 'py', 'build', 'lib.*')
DEFAULT_LIBDIR_PY3 = os.path.join(ROOT_DIR, 'lib', 'py', 'build', 'lib')

SCRIPTS = [
  'TestFrozen.py',
  'TSimpleJSONProtocolTest.py',
  'SerializationTest.py',
  'TestEof.py',
  'TestSyntax.py',
  'TestSocket.py',
]
FRAMED = ["TNonblockingServer"]
SKIP_ZLIB = ['TNonblockingServer', 'THttpServer']
SKIP_SSL = ['TNonblockingServer', 'THttpServer']
EXTRA_DELAY = dict(TProcessPoolServer=5.5)

PROTOS = [
  'accel',
  'binary',
  'compact',
  'json',
]

SERVERS = [
  "TSimpleServer",
  "TThreadedServer",
  "TThreadPoolServer",
  "TProcessPoolServer",
  "TForkingServer",
  "TNonblockingServer",
  "THttpServer",
]


def relfile(fname):
    return os.path.join(SCRIPT_DIR, fname)


def setup_pypath(dirs):
  env = copy.copy(os.environ)
  pypath = env.get('PYTHONPATH', None)
  if pypath:
    dirs.append(pypath)
  env['PYTHONPATH'] = ':'.join(dirs)
  return env


def runScriptTest(libdir, genpydir, script):
  env = setup_pypath([libdir, genpydir])
  script_args = [sys.executable, relfile(script)]
  print('\nTesting script: %s\n----' % (' '.join(script_args)))
  ret = subprocess.call(script_args, env=env)
  if ret != 0:
    print('*** FAILED ***', file=sys.stderr)
    print('LIBDIR: %s' % libdir, file=sys.stderr)
    print('PY_GEN: %s' % genpydir, file=sys.stderr)
    print('SCRIPT: %s' % script, file=sys.stderr)
    raise Exception("Script subprocess failed, retcode=%d, args: %s" % (ret, ' '.join(script_args)))


def runServiceTest(libdir, genpydir, server_class, proto, port, use_zlib, use_ssl, verbose):
  env = setup_pypath([libdir, genpydir])
  # Build command line arguments
  server_args = [sys.executable, relfile('TestServer.py')]
  cli_args = [sys.executable, relfile('TestClient.py')]
  for which in (server_args, cli_args):
    which.append('--protocol=%s' % proto)  # accel, binary, compact or json
    which.append('--port=%d' % port)  # default to 9090
    if use_zlib:
      which.append('--zlib')
    if use_ssl:
      which.append('--ssl')
    if verbose == 0:
      which.append('-q')
    if verbose == 2:
      which.append('-v')
  # server-specific option to select server class
  server_args.append(server_class)
  # client-specific cmdline options
  if server_class in FRAMED:
    cli_args.append('--transport=framed')
  else:
     cli_args.append('--transport=buffered')
  if server_class == 'THttpServer':
    cli_args.append('--http=/')
  if verbose > 0:
    print('Testing server %s: %s' % (server_class, ' '.join(server_args)))
  serverproc = subprocess.Popen(server_args, env=env)

  def ensureServerAlive():
    if serverproc.poll() is not None:
      print(('FAIL: Server process (%s) failed with retcode %d')
            % (' '.join(server_args), serverproc.returncode))
      raise Exception('Server subprocess %s died, args: %s'
                      % (server_class, ' '.join(server_args)))

  # Wait for the server to start accepting connections on the given port.
  sock = socket.socket()
  sleep_time = 0.1  # Seconds
  max_attempts = 100
  try:
    attempt = 0
    while sock.connect_ex(('127.0.0.1', port)) != 0:
      attempt += 1
      if attempt >= max_attempts:
        raise Exception("TestServer not ready on port %d after %.2f seconds"
                        % (port, sleep_time * attempt))
      ensureServerAlive()
      time.sleep(sleep_time)
  finally:
    sock.close()

  try:
    if verbose > 0:
      print('Testing client: %s' % (' '.join(cli_args)))
    ret = subprocess.call(cli_args, env=env)
    if ret != 0:
      print('*** FAILED ***', file=sys.stderr)
      print('LIBDIR: %s' % libdir, file=sys.stderr)
      print('PY_GEN: %s' % genpydir, file=sys.stderr)
      raise Exception("Client subprocess failed, retcode=%d, args: %s" % (ret, ' '.join(cli_args)))
  finally:
    # check that server didn't die
    ensureServerAlive()
    extra_sleep = EXTRA_DELAY.get(server_class, 0)
    if extra_sleep > 0 and verbose > 0:
      print('Giving %s (proto=%s,zlib=%s,ssl=%s) an extra %d seconds for child'
            'processes to terminate via alarm'
            % (server_class, proto, use_zlib, use_ssl, extra_sleep))
      time.sleep(extra_sleep)
    os.kill(serverproc.pid, signal.SIGKILL)
    serverproc.wait()


class TestCases(object):
  def __init__(self, libdir, port, gendirs, servers, verbose):
    self.libdir = libdir
    self.port = port
    self.verbose = verbose
    self.gendirs = gendirs
    self.servers = servers

  def default_conf(self):
    return {
      'gendir': self.gendirs[0],
      'server': self.servers[0],
      'proto': PROTOS[0],
      'zlib': False,
      'ssl': False,
    }

  def run(self, conf, test_count):
    with_zlib = conf['zlib']
    with_ssl = conf['ssl']
    try_server = conf['server']
    try_proto = conf['proto']
    genpydir = conf['gendir']
    # skip any servers that don't work with the Zlib transport
    if with_zlib and try_server in SKIP_ZLIB:
      return False
    # skip any servers that don't work with SSL
    if with_ssl and try_server in SKIP_SSL:
      return False
    if self.verbose > 0:
      print('\nTest run #%d:  (includes %s) Server=%s,  Proto=%s,  zlib=%s,  SSL=%s'
            % (test_count, genpydir, try_server, try_proto, with_zlib, with_ssl))
    runServiceTest(self.libdir, genpydir, try_server, try_proto, self.port, with_zlib, with_ssl, self.verbose)
    if self.verbose > 0:
      print('OK: Finished (includes %s)  %s / %s proto / zlib=%s / SSL=%s.   %d combinations tested.'
            % (genpydir, try_server, try_proto, with_zlib, with_ssl, test_count))
    return True

  def test_feature(self, name, values):
    test_count = 0
    conf = self.default_conf()
    for try_server in values:
      conf[name] = try_server
      if self.run(conf, test_count):
        test_count += 1
    return test_count

  def run_all_tests(self):
    test_count = 0
    for try_server in self.servers:
      for genpydir in self.gendirs:
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
              if self.verbose > 0:
                print('\nTest run #%d:  (includes %s) Server=%s,  Proto=%s,  zlib=%s,  SSL=%s'
                      % (test_count, genpydir, try_server, try_proto, with_zlib, with_ssl))
              runServiceTest(self.libdir, genpydir, try_server, try_proto, self.port, with_zlib, with_ssl)
              if self.verbose > 0:
                print('OK: Finished (includes %s)  %s / %s proto / zlib=%s / SSL=%s.   %d combinations tested.'
                      % (genpydir, try_server, try_proto, with_zlib, with_ssl, test_count))
    return test_count


def default_libdir():
  if sys.version_info[0] == 2:
    return glob.glob(DEFAULT_LIBDIR_GLOB)[0]
  else:
    return DEFAULT_LIBDIR_PY3


def main():
  parser = OptionParser()
  parser.add_option('--all', action="store_true", dest='all')
  parser.add_option('--genpydirs', type='string', dest='genpydirs',
                    default='default,slots,newstyle,newstyleslots,dynamic,dynamicslots',
                    help='directory extensions for generated code, used as suffixes for \"gen-py-*\" added sys.path for individual tests')
  parser.add_option("--port", type="int", dest="port", default=9090,
                    help="port number for server to listen on")
  parser.add_option('-v', '--verbose', action="store_const",
                    dest="verbose", const=2,
                    help="verbose output")
  parser.add_option('-q', '--quiet', action="store_const",
                    dest="verbose", const=0,
                    help="minimal output")
  parser.add_option('-L', '--libdir', dest="libdir", default=default_libdir(),
                    help="directory path that contains Thrift Python library")
  parser.set_defaults(verbose=1)
  options, args = parser.parse_args()

  generated_dirs = []
  for gp_dir in options.genpydirs.split(','):
    generated_dirs.append('gen-py-%s' % (gp_dir))

  # commandline permits a single class name to be specified to override SERVERS=[...]
  servers = SERVERS
  if len(args) == 1:
    if args[0] in SERVERS:
      servers = args
    else:
      print('Unavailable server type "%s", please choose one of: %s' % (args[0], servers))
      sys.exit(0)

  tests = TestCases(options.libdir, options.port, generated_dirs, servers, options.verbose)

  # run tests without a client/server first
  print('----------------')
  print(' Executing individual test scripts with various generated code directories')
  print(' Directories to be tested: ' + ', '.join(generated_dirs))
  print(' Scripts to be tested: ' + ', '.join(SCRIPTS))
  print('----------------')
  for genpydir in generated_dirs:
    for script in SCRIPTS:
      runScriptTest(options.libdir, genpydir, script)

  print('----------------')
  print(' Executing Client/Server tests with various generated code directories')
  print(' Servers to be tested: ' + ', '.join(servers))
  print(' Directories to be tested: ' + ', '.join(generated_dirs))
  print(' Protocols to be tested: ' + ', '.join(PROTOS))
  print(' Options to be tested: ZLIB(yes/no), SSL(yes/no)')
  print('----------------')

  if options.all:
    tests.run_all_tests()
  else:
    tests.test_feature('gendir', generated_dirs)
    tests.test_feature('server', servers)
    tests.test_feature('proto', PROTOS)
    tests.test_feature('zlib', [False, True])
    tests.test_feature('ssl', [False, True])


if __name__ == '__main__':
  sys.exit(main())
