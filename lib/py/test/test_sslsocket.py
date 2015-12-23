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
import platform
import select
import ssl
import sys
import threading
import time
import unittest
import warnings

import _import_local_thrift
from thrift.transport.TSSLSocket import TSSLSocket, TSSLServerSocket

SCRIPT_DIR = os.path.realpath(os.path.dirname(__file__))
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.dirname(SCRIPT_DIR)))
SERVER_PEM = os.path.join(ROOT_DIR, 'test', 'keys', 'server.pem')
SERVER_CERT = os.path.join(ROOT_DIR, 'test', 'keys', 'server.crt')
SERVER_KEY = os.path.join(ROOT_DIR, 'test', 'keys', 'server.key')
CLIENT_CERT = os.path.join(ROOT_DIR, 'test', 'keys', 'client.crt')
CLIENT_KEY = os.path.join(ROOT_DIR, 'test', 'keys', 'client.key')

TEST_PORT = 23458
TEST_ADDR = '/tmp/.thrift.domain.sock.%d' % TEST_PORT
CONNECT_TIMEOUT = 10.0
TEST_CIPHERS = 'DES-CBC3-SHA'


class ServerAcceptor(threading.Thread):
  def __init__(self, server):
    super(ServerAcceptor, self).__init__()
    self._server = server
    self.client = None

  def run(self):
    self._server.listen()
    self.client = self._server.accept()


# Python 2.6 compat
class AssertRaises(object):
  def __init__(self, expected):
    self._expected = expected

  def __enter__(self):
    pass

  def __exit__(self, exc_type, exc_value, traceback):
    if not exc_type or not issubclass(exc_type, self._expected):
      raise Exception('fail')
    return True


class TSSLSocketTest(unittest.TestCase):
  def _assert_connection_failure(self, server, client):
    try:
      acc = ServerAcceptor(server)
      acc.start()
      time.sleep(0.15)
      client.setTimeout(CONNECT_TIMEOUT)
      with self._assert_raises(Exception):
        client.open()
        select.select([], [client.handle], [], CONNECT_TIMEOUT)
      # self.assertIsNone(acc.client)
      self.assertTrue(acc.client is None)
    finally:
      server.close()
      client.close()

  def _assert_raises(self, exc):
    if sys.hexversion >= 0x020700F0:
      return self.assertRaises(exc)
    else:
      return AssertRaises(exc)

  def _assert_connection_success(self, server, client):
    try:
      acc = ServerAcceptor(server)
      acc.start()
      time.sleep(0.15)
      client.setTimeout(CONNECT_TIMEOUT)
      client.open()
      select.select([], [client.handle], [], CONNECT_TIMEOUT)
      # self.assertIsNotNone(acc.client)
      self.assertTrue(acc.client is not None)
    finally:
      server.close()
      client.close()

  # deprecated feature
  def test_deprecation(self):
    with warnings.catch_warnings(record=True) as w:
      warnings.filterwarnings('always', category=DeprecationWarning, module='thrift.*SSL.*')
      TSSLSocket('localhost', TEST_PORT, validate=True, ca_certs=SERVER_CERT)
      self.assertEqual(len(w), 1)

    with warnings.catch_warnings(record=True) as w:
      warnings.filterwarnings('always', category=DeprecationWarning, module='thrift.*SSL.*')
      # Deprecated signature
      # def __init__(self, host='localhost', port=9090, validate=True, ca_certs=None, keyfile=None, certfile=None, unix_socket=None, ciphers=None):
      client = TSSLSocket('localhost', TEST_PORT, True, SERVER_CERT, CLIENT_KEY, CLIENT_CERT, None, TEST_CIPHERS)
      self.assertEqual(len(w), 7)

    with warnings.catch_warnings(record=True) as w:
      warnings.filterwarnings('always', category=DeprecationWarning, module='thrift.*SSL.*')
      # Deprecated signature
      # def __init__(self, host=None, port=9090, certfile='cert.pem', unix_socket=None, ciphers=None):
      server = TSSLServerSocket(None, TEST_PORT, SERVER_PEM, None, TEST_CIPHERS)
      self.assertEqual(len(w), 3)

    self._assert_connection_success(server, client)

  # deprecated feature
  def test_set_cert_reqs_by_validate(self):
    c1 = TSSLSocket('localhost', TEST_PORT, validate=True, ca_certs=SERVER_CERT)
    self.assertEqual(c1.cert_reqs, ssl.CERT_REQUIRED)

    c1 = TSSLSocket('localhost', TEST_PORT, validate=False)
    self.assertEqual(c1.cert_reqs, ssl.CERT_NONE)

  # deprecated feature
  def test_set_validate_by_cert_reqs(self):
    c1 = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_NONE)
    self.assertFalse(c1.validate)

    c2 = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_REQUIRED, ca_certs=SERVER_CERT)
    self.assertTrue(c2.validate)

    c3 = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_OPTIONAL, ca_certs=SERVER_CERT)
    self.assertTrue(c3.validate)

  def test_unix_domain_socket(self):
    if platform.system() == 'Windows':
      print('skipping test_unix_domain_socket')
      return
    server = TSSLServerSocket(unix_socket=TEST_ADDR, keyfile=SERVER_KEY, certfile=SERVER_CERT)
    client = TSSLSocket(None, None, TEST_ADDR, cert_reqs=ssl.CERT_NONE)
    self._assert_connection_success(server, client)

  def test_server_cert(self):
    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT)
    client = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_REQUIRED, ca_certs=SERVER_CERT)
    self._assert_connection_success(server, client)

    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT)
    # server cert on in ca_certs
    client = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_REQUIRED, ca_certs=CLIENT_CERT)
    self._assert_connection_failure(server, client)

    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT)
    client = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_NONE)
    self._assert_connection_success(server, client)

  def test_set_server_cert(self):
    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=CLIENT_CERT)
    with self._assert_raises(Exception):
      server.certfile = 'foo'
    with self._assert_raises(Exception):
      server.certfile = None
    server.certfile = SERVER_CERT
    client = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_REQUIRED, ca_certs=SERVER_CERT)
    self._assert_connection_success(server, client)

  def test_client_cert(self):
    server = TSSLServerSocket(
        port=TEST_PORT, cert_reqs=ssl.CERT_REQUIRED, keyfile=SERVER_KEY,
        certfile=SERVER_CERT, ca_certs=CLIENT_CERT)
    client = TSSLSocket('localhost', TEST_PORT, cert_reqs=ssl.CERT_NONE, certfile=CLIENT_CERT, keyfile=CLIENT_KEY)
    self._assert_connection_success(server, client)

  def test_ciphers(self):
    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ciphers=TEST_CIPHERS)
    client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ciphers=TEST_CIPHERS)
    self._assert_connection_success(server, client)

    if not TSSLSocket._has_ciphers:
      # unittest.skip is not available for Python 2.6
      print('skipping test_ciphers')
      return
    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT)
    client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ciphers='NULL')
    self._assert_connection_failure(server, client)

    server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ciphers=TEST_CIPHERS)
    client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ciphers='NULL')
    self._assert_connection_failure(server, client)

  def test_ssl2_and_ssl3_disabled(self):
    if not hasattr(ssl, 'PROTOCOL_SSLv3'):
      print('PROTOCOL_SSLv3 is not available')
    else:
      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ssl_version=ssl.PROTOCOL_SSLv3)
      self._assert_connection_failure(server, client)

      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ssl_version=ssl.PROTOCOL_SSLv3)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT)
      self._assert_connection_failure(server, client)

    if not hasattr(ssl, 'PROTOCOL_SSLv2'):
      print('PROTOCOL_SSLv2 is not available')
    else:
      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ssl_version=ssl.PROTOCOL_SSLv2)
      self._assert_connection_failure(server, client)

      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ssl_version=ssl.PROTOCOL_SSLv2)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT)
      self._assert_connection_failure(server, client)

  def test_newer_tls(self):
    if not TSSLSocket._has_ssl_context:
      # unittest.skip is not available for Python 2.6
      print('skipping test_newer_tls')
      return
    if not hasattr(ssl, 'PROTOCOL_TLSv1_2'):
      print('PROTOCOL_TLSv1_2 is not available')
    else:
      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ssl_version=ssl.PROTOCOL_TLSv1_2)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ssl_version=ssl.PROTOCOL_TLSv1_2)
      self._assert_connection_success(server, client)

    if not hasattr(ssl, 'PROTOCOL_TLSv1_1'):
      print('PROTOCOL_TLSv1_1 is not available')
    else:
      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ssl_version=ssl.PROTOCOL_TLSv1_1)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ssl_version=ssl.PROTOCOL_TLSv1_1)
      self._assert_connection_success(server, client)

    if not hasattr(ssl, 'PROTOCOL_TLSv1_1') or not hasattr(ssl, 'PROTOCOL_TLSv1_2'):
      print('PROTOCOL_TLSv1_1 and/or PROTOCOL_TLSv1_2 is not available')
    else:
      server = TSSLServerSocket(port=TEST_PORT, keyfile=SERVER_KEY, certfile=SERVER_CERT, ssl_version=ssl.PROTOCOL_TLSv1_2)
      client = TSSLSocket('localhost', TEST_PORT, ca_certs=SERVER_CERT, ssl_version=ssl.PROTOCOL_TLSv1_1)
      self._assert_connection_failure(server, client)

  def test_ssl_context(self):
    if not TSSLSocket._has_ssl_context:
      # unittest.skip is not available for Python 2.6
      print('skipping test_ssl_context')
      return
    server_context = ssl.create_default_context(ssl.Purpose.CLIENT_AUTH)
    server_context.load_cert_chain(SERVER_CERT, SERVER_KEY)
    server_context.load_verify_locations(CLIENT_CERT)

    client_context = ssl.create_default_context(ssl.Purpose.SERVER_AUTH)
    client_context.load_cert_chain(CLIENT_CERT, CLIENT_KEY)
    client_context.load_verify_locations(SERVER_CERT)

    server = TSSLServerSocket(port=TEST_PORT, ssl_context=server_context)
    client = TSSLSocket('localhost', TEST_PORT, ssl_context=client_context)
    self._assert_connection_success(server, client)

if __name__ == '__main__':
  # import logging
  # logging.basicConfig(level=logging.DEBUG)
  unittest.main()
