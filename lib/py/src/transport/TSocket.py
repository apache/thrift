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

  def __init__(self, host='localhost', port=9090):
    self.host = host
    self.port = port
    self.handle = None

  def setHandle(self, h):
    self.handle = h

  def isOpen(self):
    return self.handle != None

  def setTimeout(self, ms):
    if (self.handle != None):
      self.handle.settimeout(ms/1000.00)

  def open(self):
    try:
      self.handle = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      self.handle.connect((self.host, self.port))
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
    self.handle = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    self.handle.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    if hasattr(self.handle, 'set_timeout'):
      self.handle.set_timeout(None)
    self.handle.bind(('', self.port))
    self.handle.listen(128)

  def accept(self):
    (client, addr) = self.handle.accept()
    result = TSocket()
    result.setHandle(client)
    return result

  def close(self):
    self.handle.close()
    self.handle = None
