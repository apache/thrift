# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

from TTransport import *
from cStringIO import StringIO

import httplib

class THttpClient(TTransportBase):

  """Http implementation of TTransport base."""

  def __init__(self, host, port, uri):
    self.host = host
    self.port = port
    self.uri = uri
    self.__wbuf = StringIO()
    self.__http = None

  def open(self):
    self.__http = httplib.HTTP(self.host, self.port)

  def close(self):
    self.__http.close()
    self.__http = None

  def isOpen(self):
    return self.__http != None

  def read(self, sz):
    return self.__http.file.read(sz)

  def write(self, buf):
    self.__wbuf.write(buf)

  def flush(self):
    # Pull data out of buffer
    data = self.__wbuf.getvalue()
    self.__wbuf = StringIO()

    # HTTP request
    self.__http.putrequest('POST', self.uri)

    # Write headers
    self.__http.putheader('Host', self.host)
    self.__http.putheader('Content-Type', 'application/x-thrift')
    self.__http.putheader('Content-Length', str(len(data)))
    self.__http.endheaders()

    # Write payload
    self.__http.send(data)

    # Get reply to flush the request
    self.code, self.message, self.headers = self.__http.getreply()
