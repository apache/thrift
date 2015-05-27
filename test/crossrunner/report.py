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

import datetime
import json
import multiprocessing
import os
import platform
import re
import subprocess
import sys
import time
import traceback

from crossrunner.test import TestEntry

LOG_DIR = 'log'
RESULT_HTML = 'result.html'
RESULT_JSON = 'results.json'
FAIL_JSON = 'known_failures_%s.json'


def generate_known_failures(testdir, overwrite, save, out):
  def collect_failures(results):
    success_index = 5
    for r in results:
      if not r[success_index]:
        yield TestEntry.get_name(*r)
  try:
    with open(os.path.join(testdir, RESULT_JSON), 'r') as fp:
      results = json.load(fp)
  except IOError:
    sys.stderr.write('Unable to load last result. Did you run tests ?\n')
    return False
  fails = collect_failures(results['results'])
  if not overwrite:
    known = load_known_failures(testdir)
    known.extend(fails)
    fails = known
  fails_json = json.dumps(sorted(set(fails)), indent=2, separators=(',', ': '))
  if save:
    with open(os.path.join(testdir, FAIL_JSON % platform.system()), 'w+') as fp:
      fp.write(fails_json)
    sys.stdout.write('Successfully updated known failures.\n')
  if out:
    sys.stdout.write(fails_json)
    sys.stdout.write('\n')
  return True


def load_known_failures(testdir):
  try:
    with open(os.path.join(testdir, FAIL_JSON % platform.system()), 'r') as fp:
      return json.load(fp)
  except IOError:
    return []


class TestReporter(object):
  # Unfortunately, standard library doesn't handle timezone well
  # DATETIME_FORMAT = '%a %b %d %H:%M:%S %Z %Y'
  DATETIME_FORMAT = '%a %b %d %H:%M:%S %Y'

  def __init__(self):
    self._log = multiprocessing.get_logger()
    self._lock = multiprocessing.Lock()

  @classmethod
  def test_logfile(cls, test_name, prog_kind, dir=None):
    relpath = os.path.join('log', '%s_%s.log' % (test_name, prog_kind))
    return relpath if not dir else os.path.realpath(os.path.join(dir, relpath))

  def _start(self):
    self._start_time = time.time()

  @property
  def _elapsed(self):
    return time.time() - self._start_time

  @classmethod
  def _format_date(cls):
    return '%s' % datetime.datetime.now().strftime(cls.DATETIME_FORMAT)

  def _print_date(self):
    self.out.write('%s\n' % self._format_date())

  def _print_bar(self, out=None):
    (out or self.out).write(
      '======================================================================\n')

  def _print_exec_time(self):
    self.out.write('Test execution took {:.1f} seconds.\n'.format(self._elapsed))


class ExecReporter(TestReporter):
  def __init__(self, testdir, test, prog):
    super(ExecReporter, self).__init__()
    self._test = test
    self._prog = prog
    self.logpath = self.test_logfile(test.name, prog.kind, testdir)
    self.out = None

  def begin(self):
    self._start()
    self._open()
    if self.out and not self.out.closed:
      self._print_header()
    else:
      self._log.debug('Output stream is not available.')

  def end(self, returncode):
    self._lock.acquire()
    try:
      if self.out and not self.out.closed:
        self._print_footer(returncode)
        self._close()
        self.out = None
      else:
        self._log.debug('Output stream is not available.')
    finally:
      self._lock.release()

  def killed(self):
    self._lock.acquire()
    try:
      if self.out and not self.out.closed:
        self._print_footer()
        self._close()
        self.out = None
      else:
        self._log.debug('Output stream is not available.')
    finally:
      self._lock.release()

  _init_failure_exprs = {
    'server': list(map(re.compile, [
      '[Aa]ddress already in use',
      'Could not bind',
      'EADDRINUSE',
    ])),
    'client': list(map(re.compile, [
      '[Cc]onnection refused',
      'Could not connect to localhost',
      'ECONNREFUSED',
      'No such file or directory',  # domain socket
    ])),
  }

  def maybe_false_positive(self):
    """Searches through log file for socket bind error.
    Returns True if suspicious expression is found, otherwise False"""
    def match(line):
      for expr in exprs:
        if expr.search(line):
          return True
    try:
      if self.out and not self.out.closed:
        self.out.flush()
      exprs = list(map(re.compile, self._init_failure_exprs[self._prog.kind]))

      server_logfile = self.logpath
      # need to handle unicode errors on Python 3
      kwargs = {} if sys.version_info[0] < 3 else {'errors': 'replace'}
      with open(server_logfile, 'r', **kwargs) as fp:
        if any(map(match, fp)):
          return True
    except (KeyboardInterrupt, SystemExit):
      raise
    except Exception as ex:
      self._log.warn('[%s]: Error while detecting false positive: %s' % (self._test.name, str(ex)))
      self._log.info(traceback.print_exc())
    return False

  def _open(self):
    self.out = open(self.logpath, 'w+')

  def _close(self):
    self.out.close()

  def _print_header(self):
    self._print_date()
    self.out.write('Executing: %s\n' % ' '.join(self._prog.command))
    self.out.write('Directory: %s\n' % self._prog.workdir)
    self.out.write('config:delay: %s\n' % self._test.delay)
    self.out.write('config:timeout: %s\n' % self._test.timeout)
    self._print_bar()
    self.out.flush()

  def _print_footer(self, returncode=None):
    self._print_bar()
    if returncode is not None:
      self.out.write('Return code: %d\n' % returncode)
    else:
      self.out.write('Process is killed.\n')
    self._print_exec_time()
    self._print_date()


class SummaryReporter(TestReporter):
  def __init__(self, testdir, concurrent=True):
    super(SummaryReporter, self).__init__()
    self.testdir = testdir
    self.logdir = os.path.join(testdir, LOG_DIR)
    self.out_path = os.path.join(testdir, RESULT_JSON)
    self.concurrent = concurrent
    self.out = sys.stdout
    self._platform = platform.system()
    self._revision = self._get_revision()
    self._tests = []
    if not os.path.exists(self.logdir):
      os.mkdir(self.logdir)
    self._known_failures = load_known_failures(testdir)
    self._unexpected_success = []
    self._unexpected_failure = []
    self._expected_failure = []
    self._print_header()

  def _get_revision(self):
    p = subprocess.Popen(['git', 'rev-parse', '--short', 'HEAD'],
                         cwd=self.testdir, stdout=subprocess.PIPE)
    out, _ = p.communicate()
    return out.strip()

  def _format_test(self, test, with_result=True):
    name = '%s-%s' % (test.server.name, test.client.name)
    trans = '%s-%s' % (test.transport, test.socket)
    if not with_result:
      return '{:19s}{:13s}{:25s}'.format(name[:18], test.protocol[:12], trans[:24])
    else:
      result = 'success' if test.success else (
          'timeout' if test.expired else 'failure')
      result_string = '%s(%d)' % (result, test.returncode)
      return '{:19s}{:13s}{:25s}{:s}\n'.format(name[:18], test.protocol[:12], trans[:24], result_string)

  def _print_test_header(self):
    self._print_bar()
    self.out.write(
      '{:19s}{:13s}{:25s}{:s}\n'.format('server-client:', 'protocol:', 'transport:', 'result:'))

  def _print_header(self):
    self._start()
    self.out.writelines([
      'Apache Thrift - Integration Test Suite\n',
    ])
    self._print_date()
    self._print_test_header()

  def _print_unexpected_failure(self):
    if len(self._unexpected_failure) > 0:
      self.out.writelines([
        '*** Following %d failures were unexpected ***:\n' % len(self._unexpected_failure),
        'If it is introduced by you, please fix it before submitting the code.\n',
        # 'If not, please report at https://issues.apache.org/jira/browse/THRIFT\n',
      ])
      self._print_test_header()
      for i in self._unexpected_failure:
        self.out.write(self._format_test(self._tests[i]))
      self._print_bar()
    else:
      self.out.write('No unexpected failures.\n')

  def _print_unexpected_success(self):
    if len(self._unexpected_success) > 0:
      self.out.write(
        'Following %d tests were known to fail but succeeded (it\'s normal):\n' % len(self._unexpected_success))
      self._print_test_header()
      for i in self._unexpected_success:
        self.out.write(self._format_test(self._tests[i]))
      self._print_bar()

  def _http_server_command(self, port):
    if sys.version_info[0] < 3:
      return 'python -m SimpleHTTPServer %d' % port
    else:
      return 'python -m http.server %d' % port

  def _print_footer(self):
    fail_count = len(self._expected_failure) + len(self._unexpected_failure)
    self._print_bar()
    self._print_unexpected_success()
    self._print_unexpected_failure()
    self._write_html_data()
    self._assemble_log('unexpected failures', self._unexpected_failure)
    self._assemble_log('known failures', self._expected_failure)
    self.out.writelines([
      'You can browse results at:\n',
      '\tfile://%s/%s\n' % (self.testdir, RESULT_HTML),
      '# If you use Chrome, run:\n',
      '# \tcd %s\n#\t%s\n' % (self.testdir, self._http_server_command(8001)),
      '# then browse:\n',
      '# \thttp://localhost:%d/%s\n' % (8001, RESULT_HTML),
      'Full log for each test is here:\n',
      '\ttest/log/client_server_protocol_transport_client.log\n',
      '\ttest/log/client_server_protocol_transport_server.log\n',
      '%d failed of %d tests in total.\n' % (fail_count, len(self._tests)),
    ])
    self._print_exec_time()
    self._print_date()

  def _render_result(self, test):
    return [
      test.server.name,
      test.client.name,
      test.protocol,
      test.transport,
      test.socket,
      test.success,
      test.as_expected,
      test.returncode,
      {
        'server': self.test_logfile(test.name, test.server.kind),
        'client': self.test_logfile(test.name, test.client.kind),
      },
    ]

  def _write_html_data(self):
    """Writes JSON data to be read by result html"""
    results = [self._render_result(r) for r in self._tests]
    with open(self.out_path, 'w+') as fp:
      fp.write(json.dumps({
        'date': self._format_date(),
        'revision': str(self._revision),
        'platform': self._platform,
        'duration': '{:.1f}'.format(self._elapsed),
        'results': results,
      }, indent=2))

  def _assemble_log(self, title, indexes):
    if len(indexes) > 0:
      def add_prog_log(fp, test, prog_kind):
        fp.write('*************************** %s message ***************************\n'
                 % prog_kind)
        path = self.test_logfile(test.name, prog_kind, self.testdir)
        kwargs = {} if sys.version_info[0] < 3 else {'errors': 'replace'}
        with open(path, 'r', **kwargs) as prog_fp:
          fp.write(prog_fp.read())
      filename = title.replace(' ', '_') + '.log'
      with open(os.path.join(self.logdir, filename), 'w+') as fp:
        for test in map(self._tests.__getitem__, indexes):
          fp.write('TEST: [%s]\n' % test.name)
          add_prog_log(fp, test, test.server.kind)
          add_prog_log(fp, test, test.client.kind)
          fp.write('**********************************************************************\n\n')
      self.out.write('%s are logged to test/%s/%s\n' % (title.capitalize(), LOG_DIR, filename))

  def end(self):
    self._print_footer()
    return len(self._unexpected_failure) == 0

  def add_test(self, test_dict):
    test = TestEntry(self.testdir, **test_dict)
    self._lock.acquire()
    try:
      if not self.concurrent:
        self.out.write(self._format_test(test, False))
        self.out.flush()
      self._tests.append(test)
      return len(self._tests) - 1
    finally:
      self._lock.release()

  def add_result(self, index, returncode, expired):
    self._lock.acquire()
    try:
      failed = returncode is None or returncode != 0
      test = self._tests[index]
      known = test.name in self._known_failures
      if failed:
        if known:
          self._log.debug('%s failed as expected' % test.name)
          self._expected_failure.append(index)
        else:
          self._log.info('unexpected failure: %s' % test.name)
          self._unexpected_failure.append(index)
      elif known:
        self._log.info('unexpected success: %s' % test.name)
        self._unexpected_success.append(index)
      test.success = not failed
      test.returncode = returncode
      test.expired = expired
      test.as_expected = known == failed
      if not self.concurrent:
        result = 'success' if not failed else 'failure'
        result_string = '%s(%d)' % (result, returncode)
        self.out.write(result_string + '\n')
      else:
        self.out.write(self._format_test(test))
    finally:
      self._lock.release()
