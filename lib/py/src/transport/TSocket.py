#!/usr/bin/env python
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

from TTransport import *
import socket

class TSocket(TTransportBase):

  """Socket implementation of TTransport base."""

  def __init__(self, host='localhost', port=9090, unix_socket=None):
    """Initialize a TSocket

    @param host(str)  The host to connect to.
    @param port(int)  The (TCP) port to connect to.
    @param unix_socket(str)  The filename of a unix socket to connect to.
                             (host and port will be ignored.)
    """

    self.host = host
    self.port = port
    self.handle = None
    self._unix_socket = unix_socket
    self._timeout = None

  def setHandle(self, h):
    self.handle = h

  def isOpen(self):
    return self.handle != None

  def setTimeout(self, ms):
    if ms is None:
      self._timeout = None
    else:
      self._timeout = ms/1000.0

    if (self.handle != None):
      self.handle.settimeout(self._timeout)

  def _resolveAddr(self):
    if self._unix_socket is not None:
      return [(socket.AF_UNIX, socket.SOCK_STREAM, None, None, self._unix_socket)]
    else:
      return socket.getaddrinfo(self.host, self.port, socket.AF_UNSPEC, socket.SOCK_STREAM, 0, socket.AI_PASSIVE | socket.AI_ADDRCONFIG)

  def open(self):
    try:
      res0 = self._resolveAddr()
      for res in res0:
        self.handle = socket.socket(res[0], res[1])
        self.handle.settimeout(self._timeout)
        try:
          self.handle.connect(res[4])
        except socket.error, e:
          if res is not res0[-1]:
            continue
          else:
            raise e
        break
    except socket.error, e:
      raise TTransportException(TTransportException.NOT_OPEN, 'Could not connect to %s:%d' % (self.host, self.port))

  def close(self):
    if self.handle != None:
      self.handle.close()
      self.handle = None

  def read(self, sz):
    buff = self.handle.recv(sz)
    if len(buff) == 0:
      raise TTransportException('TSocket read 0 bytes')
    return buff

  def write(self, buff):
    sent = 0
    have = len(buff)
    while sent < have:
      plus = self.handle.send(buff)
      if plus == 0:
        raise TTransportException('TSocket sent 0 bytes')
      sent += plus
      buff = buff[plus:]

  def flush(self):
    pass

class TServerSocket(TServerTransportBase):

  """Socket implementation of TServerTransport base."""

  def __init__(self, port):
    self.port = port
    self.handle = None

  def listen(self):
    res0 = socket.getaddrinfo(None, self.port, socket.AF_UNSPEC, socket.SOCK_STREAM, 0, socket.AI_PASSIVE | socket.AI_ADDRCONFIG)
    for res in res0:
      if res[0] is socket.AF_INET6 or res is res0[-1]:
        break

    self.handle = socket.socket(res[0], res[1])
    self.handle.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    if hasattr(self.handle, 'set_timeout'):
      self.handle.set_timeout(None)
    self.handle.bind(res[4])
    self.handle.listen(128)

  def accept(self):
    (client, addr) = self.handle.accept()
    result = TSocket()
    result.setHandle(client)
    return result

  def close(self):
    self.handle.close()
    self.handle = None
