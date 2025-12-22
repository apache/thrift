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

from __future__ import annotations

import logging
import os
import socket
import ssl
import sys
import warnings
from typing import Any, Callable

from .sslcompat import _match_has_ipaddress
from thrift.transport import TSocket
from thrift.transport.TTransport import TTransportException

_match_hostname: Callable[[dict[str, Any], str], None] = lambda cert, hostname: None

logger: logging.Logger = logging.getLogger(__name__)
warnings.filterwarnings(
    'default', category=DeprecationWarning, module=__name__)


class TSSLBase:
    """Base class for SSL socket implementations."""

    # SSLContext is not available for Python < 2.7.9
    _has_ssl_context: bool = sys.hexversion >= 0x020709F0

    # ciphers argument is not available for Python < 2.7.0
    _has_ciphers: bool = sys.hexversion >= 0x020700F0

    # For python >= 2.7.9, use latest TLS that both client and server
    # supports.
    # SSL 2.0 and 3.0 are disabled via ssl.OP_NO_SSLv2 and ssl.OP_NO_SSLv3.
    # For python < 2.7.9, use TLS 1.0 since TLSv1_X nor OP_NO_SSLvX is
    # unavailable.
    # For python < 3.6, use SSLv23 since TLS is not available
    _default_protocol: int
    if sys.version_info < (3, 6):
        _default_protocol = ssl.PROTOCOL_SSLv23 if _has_ssl_context else \
            ssl.PROTOCOL_TLSv1
    else:
        _default_protocol = ssl.PROTOCOL_TLS_CLIENT if _has_ssl_context else \
            ssl.PROTOCOL_TLSv1

    _context: ssl.SSLContext | None
    _ssl_version: int
    _server_side: bool
    _server_hostname: str | None
    _custom_context: bool
    cert_reqs: int
    ca_certs: str | None
    keyfile: str | None
    _certfile: str | None
    ciphers: str | None

    def _init_context(self, ssl_version: int) -> None:
        if self._has_ssl_context:
            self._context = ssl.SSLContext(ssl_version)
            if self._context.protocol == ssl.PROTOCOL_SSLv23:
                self._context.options |= ssl.OP_NO_SSLv2
                self._context.options |= ssl.OP_NO_SSLv3
        else:
            self._context = None
            self._ssl_version = ssl_version

    @property
    def _should_verify(self) -> bool:
        if self._has_ssl_context:
            return self._context.verify_mode != ssl.CERT_NONE  # type: ignore[union-attr]
        else:
            return self.cert_reqs != ssl.CERT_NONE

    @property
    def ssl_version(self) -> int:
        if self._has_ssl_context:
            return self.ssl_context.protocol  # type: ignore[union-attr]
        else:
            return self._ssl_version

    @property
    def ssl_context(self) -> ssl.SSLContext | None:
        return self._context

    SSL_VERSION: int = _default_protocol
    """
  Default SSL version.
  For backwards compatibility, it can be modified.
  Use __init__ keyword argument "ssl_version" instead.
  """

    def _deprecated_arg(self, args: tuple[Any, ...], kwargs: dict[str, Any], pos: int, key: str) -> None:
        if len(args) <= pos:
            return
        real_pos = pos + 3
        warnings.warn(
            '%dth positional argument is deprecated.'
            'please use keyword argument instead.'
            % real_pos, DeprecationWarning, stacklevel=3)

        if key in kwargs:
            raise TypeError(
                'Duplicate argument: %dth argument and %s keyword argument.'
                % (real_pos, key))
        kwargs[key] = args[pos]

    def _unix_socket_arg(self, host: str | None, port: int | None, args: tuple[Any, ...], kwargs: dict[str, Any]) -> bool:
        key = 'unix_socket'
        if host is None and port is None and len(args) == 1 and key not in kwargs:
            kwargs[key] = args[0]
            return True
        return False

    def __getattr__(self, key: str) -> Any:
        if key == 'SSL_VERSION':
            warnings.warn(
                'SSL_VERSION is deprecated.'
                'please use ssl_version attribute instead.',
                DeprecationWarning, stacklevel=2)
            return self.ssl_version

    def __init__(self, server_side: bool, host: str | None, ssl_opts: dict[str, Any]) -> None:
        self._server_side = server_side
        if TSSLBase.SSL_VERSION != self._default_protocol:
            warnings.warn(
                'SSL_VERSION is deprecated.'
                'please use ssl_version keyword argument instead.',
                DeprecationWarning, stacklevel=2)
        self._context = ssl_opts.pop('ssl_context', None)
        self._server_hostname = None
        if not self._server_side:
            self._server_hostname = ssl_opts.pop('server_hostname', host)
        if self._context:
            self._custom_context = True
            if ssl_opts:
                raise ValueError(
                    'Incompatible arguments: ssl_context and %s'
                    % ' '.join(ssl_opts.keys()))
            if not self._has_ssl_context:
                raise ValueError(
                    'ssl_context is not available for this version of Python')
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
                raise ValueError(
                    'Unknown keyword arguments: ', ' '.join(ssl_opts.keys()))

            if self._should_verify:
                if not self.ca_certs:
                    raise ValueError(
                        'ca_certs is needed when cert_reqs is not ssl.CERT_NONE')
                if not os.access(self.ca_certs, os.R_OK):
                    raise IOError('Certificate Authority ca_certs file "%s" '
                                  'is not readable, cannot validate SSL '
                                  'certificates.' % (self.ca_certs))

    @property
    def certfile(self) -> str | None:
        return self._certfile

    @certfile.setter
    def certfile(self, certfile: str | None) -> None:
        if self._server_side and not certfile:
            raise ValueError('certfile is needed for server-side')
        if certfile and not os.access(certfile, os.R_OK):
            raise IOError('No such certfile found: %s' % (certfile))
        self._certfile = certfile

    def _wrap_socket(self, sock: socket.socket) -> ssl.SSLSocket:
        if not self._has_ssl_context:
            # ssl.wrap_socket was removed in Python 3.12
            raise RuntimeError("SSLContext is required for Python 3.12+")
        ctx = self.ssl_context
        if ctx is None:
            raise RuntimeError("ssl_context is None but _has_ssl_context is True")
        if not self._custom_context:
            ctx.verify_mode = ssl.VerifyMode(self.cert_reqs)
            if self.certfile:
                ctx.load_cert_chain(self.certfile, self.keyfile)
            if self.ciphers:
                ctx.set_ciphers(self.ciphers)
            if self.ca_certs:
                ctx.load_verify_locations(self.ca_certs)
        return ctx.wrap_socket(
            sock, server_side=self._server_side,
            server_hostname=self._server_hostname)


class TSSLSocket(TSocket.TSocket, TSSLBase):
    """
    SSL implementation of TSocket

    This class creates outbound sockets wrapped using the
    python standard ssl module for encrypted connections.
    """

    is_valid: bool
    peercert: dict[str, Any] | None
    _validate_callback: Callable[[dict[str, Any], str | None], None]

    # New signature
    # def __init__(self, host='localhost', port=9090, unix_socket=None,
    #              **ssl_args):
    # Deprecated signature
    # def __init__(self, host='localhost', port=9090, validate=True,
    #              ca_certs=None, keyfile=None, certfile=None,
    #              unix_socket=None, ciphers=None):
    def __init__(self, host: str = 'localhost', port: int = 9090, *args: Any, **kwargs: Any) -> None:
        """Positional arguments: ``host``, ``port``, ``unix_socket``

        Keyword arguments: ``keyfile``, ``certfile``, ``cert_reqs``,
                           ``ssl_version``, ``ca_certs``,
                           ``ciphers`` (Python 2.7.0 or later),
                           ``server_hostname`` (Python 2.7.9 or later)
        Passed to ssl.wrap_socket. See ssl.wrap_socket documentation.

        Alternative keyword arguments: (Python 2.7.9 or later)
          ``ssl_context``: ssl.SSLContext to be used for SSLContext.wrap_socket
          ``server_hostname``: Passed to SSLContext.wrap_socket

        Common keyword argument:
          ``validate_callback`` (cert, hostname) -> None:
              Called after SSL handshake. Can raise when hostname does not
              match the cert.
          ``socket_keepalive`` enable TCP keepalive, default off.
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
                'validate is deprecated. please use cert_reqs=ssl.%s instead'
                % cert_reqs_name,
                DeprecationWarning, stacklevel=2)
            if 'cert_reqs' in kwargs:
                raise TypeError('Cannot specify both validate and cert_reqs')
            kwargs['cert_reqs'] = ssl.CERT_REQUIRED if validate else ssl.CERT_NONE

        unix_socket = kwargs.pop('unix_socket', None)
        socket_keepalive = kwargs.pop('socket_keepalive', False)
        self._validate_callback = kwargs.pop('validate_callback', _match_hostname)
        TSSLBase.__init__(self, False, host, kwargs)
        TSocket.TSocket.__init__(self, host, port, unix_socket,
                                 socket_keepalive=socket_keepalive)

    def close(self) -> None:
        try:
            self.handle.settimeout(0.001)  # type: ignore[union-attr]
            self.handle = self.handle.unwrap()  # type: ignore[union-attr]
        except (ssl.SSLError, socket.error, OSError):
            # could not complete shutdown in a reasonable amount of time.  bail.
            pass
        TSocket.TSocket.close(self)

    @property
    def validate(self) -> bool:
        warnings.warn('validate is deprecated. please use cert_reqs instead',
                      DeprecationWarning, stacklevel=2)
        return self.cert_reqs != ssl.CERT_NONE

    @validate.setter
    def validate(self, value: bool) -> None:
        warnings.warn('validate is deprecated. please use cert_reqs instead',
                      DeprecationWarning, stacklevel=2)
        self.cert_reqs = ssl.CERT_REQUIRED if value else ssl.CERT_NONE

    def _do_open(self, family: socket.AddressFamily, socktype: socket.SocketKind) -> ssl.SSLSocket:
        plain_sock = socket.socket(family, socktype)
        try:
            return self._wrap_socket(plain_sock)
        except Exception as ex:
            plain_sock.close()
            msg = 'failed to initialize SSL'
            logger.exception(msg)
            raise TTransportException(type=TTransportException.NOT_OPEN, message=msg, inner=ex)

    def open(self) -> None:
        super(TSSLSocket, self).open()
        if self._should_verify:
            self.peercert = self.handle.getpeercert()  # type: ignore[union-attr]
            try:
                self._validate_callback(self.peercert, self._server_hostname)  # type: ignore[arg-type]
                self.is_valid = True
            except TTransportException:
                raise
            except Exception as ex:
                raise TTransportException(message=str(ex), inner=ex)


class TSSLServerSocket(TSocket.TServerSocket, TSSLBase):
    """SSL implementation of TServerSocket

    This uses the ssl module's wrap_socket() method to provide SSL
    negotiated encryption.
    """

    _validate_callback: Callable[[dict[str, Any], str], None]

    # New signature
    # def __init__(self, host='localhost', port=9090, unix_socket=None, **ssl_args):
    # Deprecated signature
    # def __init__(self, host=None, port=9090, certfile='cert.pem', unix_socket=None, ciphers=None):
    def __init__(self, host: str | None = None, port: int = 9090, *args: Any, **kwargs: Any) -> None:
        """Positional arguments: ``host``, ``port``, ``unix_socket``

        Keyword arguments: ``keyfile``, ``certfile``, ``cert_reqs``, ``ssl_version``,
                           ``ca_certs``, ``ciphers`` (Python 2.7.0 or later)
        See ssl.wrap_socket documentation.

        Alternative keyword arguments: (Python 2.7.9 or later)
          ``ssl_context``: ssl.SSLContext to be used for SSLContext.wrap_socket
          ``server_hostname``: Passed to SSLContext.wrap_socket

        Common keyword argument:
          ``validate_callback`` (cert, hostname) -> None:
              Called after SSL handshake. Can raise when hostname does not
              match the cert.
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
        self._validate_callback = \
            kwargs.pop('validate_callback', _match_hostname)
        TSSLBase.__init__(self, True, None, kwargs)
        TSocket.TServerSocket.__init__(self, host, port, unix_socket)
        if self._should_verify and not _match_has_ipaddress:
            raise ValueError('Need ipaddress and backports.ssl_match_hostname '
                             'module to verify client certificate')

    def setCertfile(self, certfile: str) -> None:
        """Set or change the server certificate file used to wrap new
        connections.

        @param certfile: The filename of the server certificate,
                         i.e. '/etc/certs/server.pem'
        @type certfile: str

        Raises an IOError exception if the certfile is not present or unreadable.
        """
        warnings.warn(
            'setCertfile is deprecated. please use certfile property instead.',
            DeprecationWarning, stacklevel=2)
        self.certfile = certfile

    def accept(self) -> TSocket.TSocket | None:
        plain_client, addr = self.handle.accept()  # type: ignore[union-attr]
        try:
            client = self._wrap_socket(plain_client)
        except (ssl.SSLError, socket.error, OSError):
            logger.exception('Error while accepting from %s', addr)
            # failed handshake/ssl wrap, close socket to client
            plain_client.close()
            # raise
            # We can't raise the exception, because it kills most TServer derived
            # serve() methods.
            # Instead, return None, and let the TServer instance deal with it in
            # other exception handling.  (but TSimpleServer dies anyway)
            return None

        if self._should_verify:
            client.peercert = client.getpeercert()  # type: ignore[attr-defined]
            try:
                self._validate_callback(client.peercert, addr[0])  # type: ignore[attr-defined]
                client.is_valid = True  # type: ignore[attr-defined]
            except Exception:
                logger.warning('Failed to validate client certificate address: %s',
                               addr[0], exc_info=True)
                client.close()
                plain_client.close()
                return None

        result = TSocket.TSocket()
        result.handle = client
        return result
