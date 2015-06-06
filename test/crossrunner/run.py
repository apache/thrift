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

import contextlib
import multiprocessing
import multiprocessing.managers
import os
import platform
import random
import socket
import signal
import subprocess
import threading
import time
import traceback

from crossrunner.test import TestEntry, domain_socket_path
from crossrunner.report import ExecReporter, SummaryReporter

RESULT_TIMEOUT = 128
RESULT_ERROR = 64


class ExecutionContext(object):
  def __init__(self, cmd, cwd, env, report):
    self._log = multiprocessing.get_logger()
    self.report = report
    self.cmd = cmd
    self.cwd = cwd
    self.env = env
    self.timer = None
    self.expired = False

  def _expire(self):
    self._log.info('Timeout')
    self.expired = True
    self.kill()

  def kill(self):
    self._log.debug('Killing process : %d' % self.proc.pid)
    if platform.system() != 'Windows':
      try:
        os.killpg(self.proc.pid, signal.SIGKILL)
      except Exception as err:
        self._log.info('Failed to kill process group : %s' % str(err))
    try:
      self.proc.kill()
    except Exception as err:
      self._log.info('Failed to kill process : %s' % str(err))
    self.report.killed()

  def _popen_args(self):
    args = {
      'cwd': self.cwd,
      'env': self.env,
      'stdout': self.report.out,
      'stderr': subprocess.STDOUT,
    }
    # make sure child processes doesn't remain after killing
    if platform.system() == 'Windows':
      DETACHED_PROCESS = 0x00000008
      args.update(creationflags=DETACHED_PROCESS | subprocess.CREATE_NEW_PROCESS_GROUP)
    else:
      args.update(preexec_fn=os.setsid)
    return args

  def start(self, timeout=0):
    self._log.debug('COMMAND: %s', ' '.join(self.cmd))
    self._log.debug('WORKDIR: %s', self.cwd)
    self._log.debug('LOGFILE: %s', self.report.logpath)
    self.report.begin()
    self.proc = subprocess.Popen(self.cmd, **self._popen_args())
    if timeout > 0:
      self.timer = threading.Timer(timeout, self._expire)
      self.timer.start()
    return self._scoped()

  @contextlib.contextmanager
  def _scoped(self):
    yield self
    self._log.debug('Killing scoped process')
    self.kill()

  def wait(self):
    self.proc.communicate()
    if self.timer:
      self.timer.cancel()
    self.report.end(self.returncode)

  @property
  def returncode(self):
    return self.proc.returncode if self.proc else None


def exec_context(port, testdir, test, prog):
  report = ExecReporter(testdir, test, prog)
  prog.build_command(port)
  return ExecutionContext(prog.command, prog.workdir, prog.env, report)


def run_test(testdir, test_dict, async=True, max_retry=3):
  try:
    logger = multiprocessing.get_logger()
    retry_count = 0
    test = TestEntry(testdir, **test_dict)
    while True:
      if stop.is_set():
        logger.debug('Skipping because shutting down')
        return None
      logger.debug('Start')
      with PortAllocator.alloc_port_scoped(ports, test.socket) as port:
        logger.debug('Start with port %d' % port)
        sv = exec_context(port, testdir, test, test.server)
        cl = exec_context(port, testdir, test, test.client)

        logger.debug('Starting server')
        with sv.start():
          if test.delay > 0:
            logger.debug('Delaying client for %.2f seconds' % test.delay)
            time.sleep(test.delay)
          cl_retry_count = 0
          cl_max_retry = 10
          cl_retry_wait = 0.5
          while True:
            logger.debug('Starting client')
            cl.start(test.timeout)
            logger.debug('Waiting client')
            cl.wait()
            if not cl.report.maybe_false_positive() or cl_retry_count >= cl_max_retry:
              if cl_retry_count > 0 and cl_retry_count < cl_max_retry:
                logger.warn('[%s]: Connected after %d retry (%.2f sec each)' % (test.server.name, cl_retry_count, cl_retry_wait))
              break
            logger.debug('Server may not be ready, waiting %.2f second...' % cl_retry_wait)
            time.sleep(cl_retry_wait)
            cl_retry_count += 1

      if not sv.report.maybe_false_positive() or retry_count >= max_retry:
        logger.debug('Finish')
        return RESULT_TIMEOUT if cl.expired else cl.proc.returncode
      logger.warn('[%s]: Detected socket bind failure, retrying...' % test.server.name)
      retry_count += 1
  except (KeyboardInterrupt, SystemExit):
    logger.info('Interrupted execution')
    if not async:
      raise
    stop.set()
    return None
  except Exception as ex:
    logger.warn('Error while executing test : %s' % str(ex))
    if not async:
      raise
    logger.info(traceback.print_exc())
    return RESULT_ERROR


class PortAllocator(object):
  def __init__(self):
    self._log = multiprocessing.get_logger()
    self._lock = multiprocessing.Lock()
    self._ports = set()
    self._dom_ports = set()
    self._last_alloc = 0

  def _get_tcp_port(self):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(('127.0.0.1', 0))
    port = sock.getsockname()[1]
    self._lock.acquire()
    try:
      ok = port not in self._ports
      if ok:
        self._ports.add(port)
        self._last_alloc = time.time()
    finally:
      self._lock.release()
      sock.close()
    return port if ok else self._get_tcp_port()

  def _get_domain_port(self):
    port = random.randint(1024, 65536)
    self._lock.acquire()
    try:
      ok = port not in self._dom_ports
      if ok:
        self._dom_ports.add(port)
    finally:
      self._lock.release()
    return port if ok else self._get_domain_port()

  def alloc_port(self, socket_type):
    if socket_type == 'domain':
      return self._get_domain_port()
    else:
      return self._get_tcp_port()

  # static method for inter-process invokation
  @staticmethod
  @contextlib.contextmanager
  def alloc_port_scoped(allocator, socket_type):
    port = allocator.alloc_port(socket_type)
    yield port
    allocator.free_port(socket_type, port)

  def free_port(self, socket_type, port):
    self._log.debug('free_port')
    self._lock.acquire()
    try:
      if socket_type == 'domain':
        self._dom_ports.remove(port)
        path = domain_socket_path(port)
        if os.path.exists(path):
          os.remove(path)
      else:
        self._ports.remove(port)
    except IOError as err:
      self._log.info('Error while freeing port : %s' % str(err))
    finally:
      self._lock.release()


class NonAsyncResult(object):
  def __init__(self, value):
    self._value = value

  def get(self, timeout=None):
    return self._value

  def wait(self, timeout=None):
    pass

  def ready(self):
    return True

  def successful(self):
    return self._value == 0


class TestDispatcher(object):
  def __init__(self, testdir, concurrency):
    self._log = multiprocessing.get_logger()
    self.testdir = testdir
    # seems needed for python 2.x to handle keyboard interrupt
    self._stop = multiprocessing.Event()
    self._async = concurrency > 1
    if not self._async:
      self._pool = None
      global stop
      global ports
      stop = self._stop
      ports = PortAllocator()
    else:
      self._m = multiprocessing.managers.BaseManager()
      self._m.register('ports', PortAllocator)
      self._m.start()
      self._pool = multiprocessing.Pool(concurrency, self._pool_init, (self._m.address,))
    self._report = SummaryReporter(testdir, concurrency > 1)
    self._log.debug(
        'TestDispatcher started with %d concurrent jobs' % concurrency)

  def _pool_init(self, address):
    global stop
    global m
    global ports
    stop = self._stop
    m = multiprocessing.managers.BaseManager(address)
    m.connect()
    ports = m.ports()

  def _dispatch_sync(self, test, cont):
    r = run_test(self.testdir, test, False)
    cont(r)
    return NonAsyncResult(r)

  def _dispatch_async(self, test, cont):
    return self._pool.apply_async(func=run_test, args=(self.testdir, test,), callback=cont)

  def dispatch(self, test):
    index = self._report.add_test(test)

    def cont(r):
      if not self._stop.is_set():
        self._log.debug('freeing port')
        self._log.debug('adding result')
        self._report.add_result(index, r, r == RESULT_TIMEOUT)
        self._log.debug('finish continuation')
    fn = self._dispatch_async if self._async else self._dispatch_sync
    return fn(test, cont)

  def wait(self):
    if self._async:
      self._pool.close()
      self._pool.join()
      self._m.shutdown()
    return self._report.end()

  def terminate(self):
    self._stop.set()
    if self._async:
      self._pool.terminate()
      self._pool.join()
      self._m.shutdown()
