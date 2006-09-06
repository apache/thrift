from TTransport import *
import socket

class TSocket(TTransportBase):

  """Socket implementation of TTransport base."""

  def __init__(self, host='localhost', port=9090):
    self.host = host
    self.port = port
    self.handle = None

  def set_handle(self, h):
    self.handle = h

  def isOpen(self):
    return handle != None

  def open(self):
    self.handle = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    self.handle.connect((self.host, self.port))

  def close(self):
    self.handle.close()
    self.handle = None

  def readAll(self, sz):
    buff = ''
    have = 0
    while (have < sz):
      chunk = self.read(sz-have)
      have += len(chunk)
      buff += chunk
    return buff

  def read(self, sz):
    buff = self.handle.recv(sz)
    if len(buff) == 0:
      raise Exception('TSocket read 0 bytes')
    return buff

  def write(self, buff):
    sent = 0
    have = len(buff)
    while sent < have:
      plus = self.handle.send(buff)
      if plus == 0:
        raise Exception('sent 0 bytes')
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
    self.handle.bind(('', self.port))
    self.handle.listen(128)

  def accept(self):
    (client, addr) = self.handle.accept()
    result = TSocket()
    result.set_handle(client)
    return result

  def close(self):
    self.handle.close()
    self.handle = None
