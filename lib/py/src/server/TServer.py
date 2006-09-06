import sys
import traceback

from thrift.Thrift import TProcessor
from thrift.transport import TTransport

class TServer:

  """Base interface for a server, which must have a run method."""

  def __init__(self, proc):
    self.processor = proc

  def run(self):
    pass

class TSimpleServer(TServer):

  """Simple single-threaded server that just pumps around one transport."""

  def __init__(self, proc, trans):
    TServer.__init__(self, proc)
    self.transport = trans

  def run(self):
    self.transport.listen()
    while True:
      client = TTransport.TBufferedTransport(self.transport.accept())
      try:
        while True:
          self.processor.process(client, client)
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())
        print 'Client died.'
      client.close()
