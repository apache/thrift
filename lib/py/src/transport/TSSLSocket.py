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

import logging
import os
import socket
import ssl
import sys
import warnings

from thrift.transport import TSocket
from thrift.transport.TTransport import TTransportException

logger = logging.getLogger(__name__)
warnings.filterwarnings('default', category=DeprecationWarning, module=__name__)


class TSSLBase(object):
  # SSLContext is not available for Python < 2.7.9
  _has_ssl_context = sys.hexversion >= 0x020709F0

  # ciphers argument is not available for Python < 2.7.0
  _has_ciphers = sys.hexversion >= 0x020700F0

  # For pythoon >= 2.7.9, use latest TLS that both client and server supports.
  # SSL 2.0 and 3.0 are disabled via ssl.OP_NO_SSLv2 and ssl.OP_NO_SSLv3.
  # For pythoon < 2.7.9, use TLS 1.0 since TLSv1_X nare OP_NO_SSLvX are unavailable.
  _default_protocol = ssl.PROTOCOL_SSLv23 if _has_ssl_context else ssl.PROTOCOL_TLSv1

  def _init_context(self, ssl_version):
    if self._has_ssl_context:
      self._context = ssl.SSLContext(ssl_version)
      if self._context.protocol == ssl.PROTOCOL_SSLv23:
        self._context.options |= ssl.OP_NO_SSLv2
        self._context.options |= ssl.OP_NO_SSLv3
    else:
      self._context = None
      self._ssl_version = ssl_version

  @property
  def ssl_version(self):
    if self._has_ssl_context:
      return self.ssl_context.protocol
    else:
      return self._ssl_version

  @property
  def ssl_context(self):
    return self._context

  SSL_VERSION = _default_protocol
  """
  Default SSL version.
  For backword compatibility, it can be modified.
  Use __init__ keywoard argument "ssl_version" instead.
  """

  def _deprecated_arg(self, args, kwargs, pos, key):
    if len(args) <= pos:
      return
    real_pos = pos + 3
    warnings.warn(
        '%dth positional argument is deprecated. Use keyward argument insteand.' % real_pos,
        DeprecationWarning)
    if key in kwargs:
      raise TypeError('Duplicate argument: %dth argument and %s keyward argument.', (real_pos, key))
    kwargs[key] = args[pos]

  def _unix_socket_arg(self, host, port, args, kwargs):
    key = 'unix_socket'
    if host is None and port is None and len(args) == 1 and key not in kwargs:
      kwargs[key] = args[0]
      return True
    return False

  def __getattr__(self, key):
    if key == 'SSL_VERSION':
      warnings.warn('Use ssl_version attribute instead.', DeprecationWarning)
      return self.ssl_version

  def __init__(self, server_side, host, ssl_opts):
    self._server_side = server_side
    if TSSLBase.SSL_VERSION != self._default_protocol:
      warnings.warn('SSL_VERSION is deprecated. Use ssl_version keyward argument instead.', DeprecationWarning)
    self._context = ssl_opts.pop('ssl_context', None)
    self._server_hostname = None
    if not self._server_side:
      self._server_hostname = ssl_opts.pop('server_hostname', host)
    if self._context:
      self._custom_context = True
      if ssl_opts:
        raise ValueError('Incompatible arguments: ssl_context and %s' % ' '.join(ssl_opts.keys()))
      if not self._has_ssl_context:
        raise ValueError('ssl_context is not available for this version of Python')
    else:
      self._custom_context = False
      ssl_version = ssl_opts.pop('ssl_version', TSSLBase.SSL_VERSION)
      self._init_context(ssl_version)
      self.cert_reqs = ssl_opts.pop('cert_reqs', ssl.CERT_REQUIRED)
      self.ca_certs = ssl_opts.pop('ca_certs', None)
      self.keyfile = ssl_opts.pop('keyfile', None)
      self.certfile = ssl_opts.pop('certfile', None)
      self.ciphers = ssl_opts.pop('ciphers', None)

      if ssl_opts:
        raise ValueError('Unknown keyword arguments: ', ' '.join(ssl_opts.keys()))

      if self.cert_reqs != ssl.CERT_NONE:
        if not self.ca_certs:
          raise ValueError('ca_certs is needed when cert_reqs is not ssl.CERT_NONE')
        if not os.access(self.ca_certs, os.R_OK):
          raise IOError('Certificate Authority ca_certs file "%s" '
                        'is not readable, cannot validate SSL '
                        'certificates.' % (self.ca_certs))

  @property
  def certfile(self):
    return self._certfile

  @certfile.setter
  def certfile(self, certfile):
    if self._server_side and not certfile:
      raise ValueError('certfile is needed for server-side')
    if certfile and not os.access(certfile, os.R_OK):
      raise IOError('No such certfile found: %s' % (certfile))
    self._certfile = certfile

  def _wrap_socket(self, sock):
    if self._has_ssl_context:
      if not self._custom_context:
        self.ssl_context.verify_mode = self.cert_reqs
        if self.certfile:
          self.ssl_context.load_cert_chain(self.certfile, self.keyfile)
        if self.ciphers:
          self.ssl_context.set_ciphers(self.ciphers)
        if self.ca_certs:
          self.ssl_context.load_verify_locations(self.ca_certs)
      return self.ssl_context.wrap_socket(sock, server_side=self._server_side,
                                          server_hostname=self._server_hostname)
    else:
      ssl_opts = {
        'ssl_version': self._ssl_version,
        'server_side': self._server_side,
        'ca_certs': self.ca_certs,
        'keyfile': self.keyfile,
        'certfile': self.certfile,
        'cert_reqs': self.cert_reqs,
      }
      if self.ciphers:
        if self._has_ciphers:
          ssl_opts['ciphers'] = self.ciphers
        else:
          logger.warning('ciphers is specified but ignored due to old Python version')
      return ssl.wrap_socket(sock, **ssl_opts)


class TSSLSocket(TSocket.TSocket, TSSLBase):
  """
  SSL implementation of TSocket

  This class creates outbound sockets wrapped using the
  python standard ssl module for encrypted connections.
  """

  # New signature
  # def __init__(self, host='localhost', port=9090, unix_socket=None, **ssl_args):
  # Deprecated signature
  # def __init__(self, host='localhost', port=9090, validate=True, ca_certs=None, keyfile=None, certfile=None, unix_socket=None, ciphers=None):
  def __init__(self, host='localhost', port=9090, *args, **kwargs):
    """Positional arguments: ``host``, ``port``, ``unix_socket``

    Keyword arguments: ``keyfile``, ``certfile``, ``cert_reqs``, ``ssl_version``,
                       ``ca_certs``, ``ciphers`` (Python 2.7.0 or later),
                       ``server_hostname`` (Python 2.7.9 or later)
    Passed to ssl.wrap_socket. See ssl.wrap_socket documentation.

    Alternative keywoard arguments: (Python 2.7.9 or later)
      ``ssl_context``: ssl.SSLContext to be used for SSLContext.wrap_socket
      ``server_hostname``: Passed to SSLContext.wrap_socket
    """
    self.is_valid = False
    self.peercert = None

    if args:
      if len(args) > 6:
        raise TypeError('Too many positional argument')
      if not self._unix_socket_arg(host, port, args, kwargs):
        self._deprecated_arg(args, kwargs, 0, 'validate')
      self._deprecated_arg(args, kwargs, 1, 'ca_certs')
      self._deprecated_arg(args, kwargs, 2, 'keyfile')
      self._deprecated_arg(args, kwargs, 3, 'certfile')
      self._deprecated_arg(args, kwargs, 4, 'unix_socket')
      self._deprecated_arg(args, kwargs, 5, 'ciphers')

    validate = kwargs.pop('validate', None)
    if validate is not None:
      cert_reqs_name = 'CERT_REQUIRED' if validate else 'CERT_NONE'
      warnings.warn(
          'validate is deprecated. Use cert_reqs=ssl.%s instead' % cert_reqs_name,
          DeprecationWarning)
      if 'cert_reqs' in kwargs:
        raise TypeError('Cannot specify both validate and cert_reqs')
      kwargs['cert_reqs'] = ssl.CERT_REQUIRED if validate else ssl.CERT_NONE

    unix_socket = kwargs.pop('unix_socket', None)
    TSSLBase.__init__(self, False, host, kwargs)
    TSocket.TSocket.__init__(self, host, port, unix_socket)

  @property
  def validate(self):
    warnings.warn('Use cert_reqs instead', DeprecationWarning)
    return self.cert_reqs != ssl.CERT_NONE

  @validate.setter
  def validate(self, value):
    warnings.warn('Use cert_reqs instead', DeprecationWarning)
    self.cert_reqs = ssl.CERT_REQUIRED if value else ssl.CERT_NONE

  def open(self):
    try:
      res0 = self._resolveAddr()
      for res in res0:
        sock_family, sock_type = res[0:2]
        ip_port = res[4]
        plain_sock = socket.socket(sock_family, sock_type)
        self.handle = self._wrap_socket(plain_sock)
        self.handle.settimeout(self._timeout)
        try:
          self.handle.connect(ip_port)
        except socket.error as e:
          if res is not res0[-1]:
            logger.warning('Error while connecting with %s. Trying next one.', ip_port, exc_info=True)
            continue
          else:
            raise
        break
    except socket.error as e:
      if self._unix_socket:
        message = 'Could not connect to secure socket %s: %s' \
                  % (self._unix_socket, e)
      else:
        message = 'Could not connect to %s:%d: %s' % (self.host, self.port, e)
      logger.error('Error while connecting with %s.', ip_port, exc_info=True)
      raise TTransportException(type=TTransportException.NOT_OPEN,
                                message=message)
    if self.validate:
      self._validate_cert()

  def _validate_cert(self):
    """internal method to validate the peer's SSL certificate, and to check the
    commonName of the certificate to ensure it matches the hostname we
    used to make this connection.  Does not support subjectAltName records
    in certificates.

    raises TTransportException if the certificate fails validation.
    """
    cert = self.handle.getpeercert()
    self.peercert = cert
    if 'subject' not in cert:
      raise TTransportException(
        type=TTransportException.NOT_OPEN,
        message='No SSL certificate found from %s:%s' % (self.host, self.port))
    fields = cert['subject']
    for field in fields:
      # ensure structure we get back is what we expect
      if not isinstance(field, tuple):
        continue
      cert_pair = field[0]
      if len(cert_pair) < 2:
        continue
      cert_key, cert_value = cert_pair[0:2]
      if cert_key != 'commonName':
        continue
      certhost = cert_value
      # this check should be performed by some sort of Access Manager
      if certhost == self.host:
        # success, cert commonName matches desired hostname
        self.is_valid = True
        return
      else:
        raise TTransportException(
          type=TTransportException.UNKNOWN,
          message='Hostname we connected to "%s" doesn\'t match certificate '
                  'provided commonName "%s"' % (self.host, certhost))
    raise TTransportException(
      type=TTransportException.UNKNOWN,
      message='Could not validate SSL certificate from '
              'host "%s".  Cert=%s' % (self.host, cert))


class TSSLServerSocket(TSocket.TServerSocket, TSSLBase):
  """SSL implementation of TServerSocket

  This uses the ssl module's wrap_socket() method to provide SSL
  negotiated encryption.
  """

  # New signature
  # def __init__(self, host='localhost', port=9090, unix_socket=None, **ssl_args):
  # Deprecated signature
  # def __init__(self, host=None, port=9090, certfile='cert.pem', unix_socket=None, ciphers=None):
  def __init__(self, host=None, port=9090, *args, **kwargs):
    """Positional arguments: ``host``, ``port``, ``unix_socket``

    Keyword arguments: ``keyfile``, ``certfile``, ``cert_reqs``, ``ssl_version``,
                       ``ca_certs``, ``ciphers`` (Python 2.7.0 or later)
    See ssl.wrap_socket documentation.

    Alternative keywoard arguments: (Python 2.7.9 or later)
      ``ssl_context``: ssl.SSLContext to be used for SSLContext.wrap_socket
      ``server_hostname``: Passed to SSLContext.wrap_socket
    """
    if args:
      if len(args) > 3:
        raise TypeError('Too many positional argument')
      if not self._unix_socket_arg(host, port, args, kwargs):
        self._deprecated_arg(args, kwargs, 0, 'certfile')
      self._deprecated_arg(args, kwargs, 1, 'unix_socket')
      self._deprecated_arg(args, kwargs, 2, 'ciphers')

    if 'ssl_context' not in kwargs:
      # Preserve existing behaviors for default values
      if 'cert_reqs' not in kwargs:
        kwargs['cert_reqs'] = ssl.CERT_NONE
      if'certfile' not in kwargs:
        kwargs['certfile'] = 'cert.pem'

    unix_socket = kwargs.pop('unix_socket', None)
    TSSLBase.__init__(self, True, None, kwargs)
    TSocket.TServerSocket.__init__(self, host, port, unix_socket)

  def setCertfile(self, certfile):
    """Set or change the server certificate file used to wrap new connections.

    @param certfile: The filename of the server certificate,
                     i.e. '/etc/certs/server.pem'
    @type certfile: str

    Raises an IOError exception if the certfile is not present or unreadable.
    """
    warnings.warn('Use certfile property instead.', DeprecationWarning)
    self.certfile = certfile

  def accept(self):
    plain_client, addr = self.handle.accept()
    try:
      client = self._wrap_socket(plain_client)
    except ssl.SSLError:
      logger.error('Error while accepting from %s', addr, exc_info=True)
      # failed handshake/ssl wrap, close socket to client
      plain_client.close()
      # raise
      # We can't raise the exception, because it kills most TServer derived
      # serve() methods.
      # Instead, return None, and let the TServer instance deal with it in
      # other exception handling.  (but TSimpleServer dies anyway)
      return None
    result = TSocket.TSocket()
    result.setHandle(client)
    return result
