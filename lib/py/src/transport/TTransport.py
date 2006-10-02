from cStringIO import StringIO

class TTransportException(Exception):

  """Custom Transport Exception class"""

  pass

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
    pass

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

  def getIOTransports(self, trans):
    return (trans, trans)

class TBufferedTransportFactory:

  """Factory transport that builds buffered transports"""

  def getIOTransports(self, trans):
    buffered = TBufferedTransport(trans)
    return (buffered, buffered)


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

  def readAll(self, sz):
    return self.__trans.readAll(sz)

  def write(self, buf):
    self.__buf.write(buf)

  def flush(self):
    self.__trans.write(self.__buf.getvalue())
    self.__buf = StringIO()
