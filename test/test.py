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

# Apache Thrift - integration test suite
#
# tests different server-client, protocol and transport combinations
#
# This script supports python 2.7 and later.
# python 3.x is recommended for better stability.
#
# TODO: eliminate a few 2.7 occurrences to support 2.6 ?
#

import json
import logging
import multiprocessing
import optparse
import os
import sys

import crossrunner

TEST_DIR = os.path.realpath(os.path.dirname(__file__))
CONFIG_PATH = os.path.join(TEST_DIR, 'tests.json')


def prepare(server_match, client_match):
  with open(CONFIG_PATH, 'r') as fp:
    j = json.load(fp)
  return crossrunner.prepare(j, TEST_DIR, server_match, client_match)


def run_tests(server_match, client_match, jobs, skip_known_failures):
  logger = multiprocessing.get_logger()
  logger.debug('Collecting tests')
  with open(CONFIG_PATH, 'r') as fp:
    j = json.load(fp)
  tests = list(crossrunner.collect_tests(j, server_match, client_match))
  if skip_known_failures:
    known = crossrunner.load_known_failures(TEST_DIR)
    tests = list(filter(lambda t: crossrunner.test_name(**t) not in known, tests))

  dispatcher = crossrunner.TestDispatcher(TEST_DIR, jobs)
  logger.debug('Executing %d tests' % len(tests))
  try:
    for r in [dispatcher.dispatch(test) for test in tests]:
      r.wait()
    logger.debug('Waiting for completion')
    return dispatcher.wait()
  except (KeyboardInterrupt, SystemExit):
    logger.debug('Interrupted, shutting down')
    dispatcher.terminate()
    return False


def default_concurrenty():
  try:
    return int(os.environ.get('THRIFT_CROSSTEST_CONCURRENCY'))
  except (TypeError, ValueError):
    # Since much time is spent sleeping, use many threads
    return int(multiprocessing.cpu_count() * 1.25) + 1


def main(argv):
  parser = optparse.OptionParser()
  parser.add_option('--server', type='string', dest='servers', default='',
                    help='list of servers to test separated by commas, eg:- --server=cpp,java')
  parser.add_option('--client', type='string', dest='clients', default='',
                    help='list of clients to test separated by commas, eg:- --client=cpp,java')
  parser.add_option('-s', '--skip-known-failures', action='store_true', dest='skip_known_failures',
                    help='do not execute tests that are known to fail')
  parser.add_option('-j', '--jobs', type='int', dest='jobs',
                    default=default_concurrenty(),
                    help='number of concurrent test executions')
  g = optparse.OptionGroup(parser, 'Advanced')
  g.add_option('-v', '--verbose', action='store_const',
               dest='log_level', const=logging.DEBUG, default=logging.WARNING,
               help='show debug output for test runner')
  g.add_option('-P', '--print-expected-failures', choices=['merge', 'overwrite'],
               dest='print_failures', default=None,
               help="generate expected failures based on last result and print to stdout")
  g.add_option('-U', '--update-expected-failures', choices=['merge', 'overwrite'],
               dest='update_failures', default=None,
               help="generate expected failures based on last result and save to default file location")
  g.add_option('--prepare', action='store_true',
               dest='prepare',
               help="try to prepare files needed for cross test (experimental)")
  parser.add_option_group(g)
  logger = multiprocessing.log_to_stderr()
  options, _ = parser.parse_args(argv)
  server_match = options.servers.split(',') if options.servers else []
  client_match = options.clients.split(',') if options.clients else []
  logger.setLevel(options.log_level)

  if options.prepare:
    res = prepare(server_match, client_match)
  elif options.update_failures or options.print_failures:
    res = crossrunner.generate_known_failures(
        TEST_DIR, options.update_failures == 'overwrite',
        options.update_failures, options.print_failures)
  else:
    res = run_tests(server_match, client_match, options.jobs, options.skip_known_failures)
  return 0 if res else 1

if __name__ == '__main__':
  sys.exit(main(sys.argv[1:]))
