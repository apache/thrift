import sys
import traceback

from thrift.Thrift import TProcessor
from thrift.transport import TTransport

class TServer:

  """Base interface for a server, which must have a serve method."""

  def __init__(self, processor, serverTransport, transportFactory=None):
    self.processor = processor
    self.serverTransport = serverTransport
    if transportFactory == None:
      self.transportFactory = TTransport.TTransportFactoryBase()
    else:
      self.transportFactory = transportFactory

  def serve(self):
    pass

class TSimpleServer(TServer):

  """Simple single-threaded server that just pumps around one transport."""

  def __init__(self, processor, serverTransport, transportFactory=None):
    TServer.__init__(self, processor, serverTransport, transportFactory)

  def serve(self):
    self.serverTransport.listen()
    while True:
      client = self.serverTransport.accept()
      (input, output) = self.transportFactory.getIOTransports(client)
      try:
        while True:
          self.processor.process(input, output)
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())
        print 'Client died.'

      input.close()
      output.close()
