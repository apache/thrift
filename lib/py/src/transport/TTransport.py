#!/usr/bin/env python
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

from cStringIO import StringIO
from struct import pack,unpack
from thrift.Thrift import TException

class TTransportException(TException):

  """Custom Transport Exception class"""

  UNKNOWN = 0
  NOT_OPEN = 1
  ALREADY_OPEN = 2
  TIMED_OUT = 3
  END_OF_FILE = 4

  def __init__(self, type=UNKNOWN, message=None):
    TException.__init__(self, message)
    self.type = type

class TTransportBase:

  """Base class for Thrift transport layer."""

  def isOpen(self):
    pass

  def open(self):
    pass

  def close(self):
    pass

  def read(self, sz):
    pass

  def readAll(self, sz):
    buff = ''
    have = 0
    while (have < sz):
      chunk = self.read(sz-have)
      have += len(chunk)
      buff += chunk
    return buff

  def write(self, buf):
    pass

  def flush(self):
    pass

class TServerTransportBase:

  """Base class for Thrift server transports."""

  def listen(self):
    pass

  def accept(self):
    pass

  def close(self):
    pass

class TTransportFactoryBase:

  """Base class for a Transport Factory"""

  def getTransport(self, trans):
    return trans

class TBufferedTransportFactory:

  """Factory transport that builds buffered transports"""

  def getTransport(self, trans):
    buffered = TBufferedTransport(trans)
    return buffered


class TBufferedTransport(TTransportBase):

  """Class that wraps another transport and buffers its I/O."""

  def __init__(self, trans):
    self.__trans = trans
    self.__buf = StringIO()

  def isOpen(self):
    return self.__trans.isOpen()

  def open(self):
    return self.__trans.open()

  def close(self):
    return self.__trans.close()

  def read(self, sz):
    return self.__trans.read(sz)

  def write(self, buf):
    self.__buf.write(buf)

  def flush(self):
    self.__trans.write(self.__buf.getvalue())
    self.__trans.flush()
    self.__buf = StringIO()

class TFramedTransportFactory:

  """Factory transport that builds framed transports"""

  def getTransport(self, trans):
    framed = TFramedTransport(trans)
    return framed


class TFramedTransport(TTransportBase):

  """Class that wraps another transport and frames its I/O when writing."""

  def __init__(self, trans, read=True, write=True):
    self.__trans = trans
    if read:
      self.__rbuf = ''
    else:
      self.__rbuf = None
    if write:
      self.__wbuf = StringIO()
    else:
      self.__wbuf = None

  def isOpen(self):
    return self.__trans.isOpen()

  def open(self):
    return self.__trans.open()

  def close(self):
    return self.__trans.close()

  def read(self, sz):
    if self.__rbuf == None:
      return self.__trans.read(sz)
    if len(self.__rbuf) == 0:
      self.readFrame()
    give = min(len(self.__rbuf), sz)
    buff = self.__rbuf[0:give]
    self.__rbuf = self.__rbuf[give:]
    return buff

  def readFrame(self):
    buff = self.__trans.readAll(4)
    sz, = unpack('!i', buff)
    self.__rbuf = self.__trans.readAll(sz)
  
  def write(self, buf):
    if self.__wbuf == None:
      return self.__trans.write(buf)
    self.__wbuf.write(buf)

  def flush(self):
    if self.__wbuf == None:
      return self.__trans.flush()
    wout = self.__wbuf.getvalue()
    wsz = len(wout)
    # N.B.: Doing this string concatenation is WAY cheaper than making
    # two separate calls to the underlying socket object. Socket writes in
    # Python turn out to be REALLY expensive, but it seems to do a pretty
    # good job of managing string buffer operations without excessive copies
    buf = pack("!i", wsz) + wout
    self.__trans.write(buf)
    self.__trans.flush()
    self.__wbuf = StringIO()
