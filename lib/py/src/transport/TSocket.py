from TTransport import *
import socket

class TSocket(TTransportBase):

  """Socket implementation of TTransport base."""

  handle = None
  host = "localhost"
  port = 9090

  def __init__(self, host, port):
    self.host = host
    self.port = port
    self.handle = None

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
    return buff

  def write(self, buff):
    self.handle.sendall(buff)

  def flush(self):
    pass
