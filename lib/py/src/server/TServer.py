import sys
import traceback
import threading
import Queue

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
      except TTransport.TTransportException, tx:
        pass
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())

      input.close()
      output.close()

class TThreadedServer(TServer):

  """Threaded server that spawns a new thread per each connection."""

  def __init__(self, processor, serverTransport, transportFactory=None):
    TServer.__init__(self, processor, serverTransport, transportFactory)

  def serve(self):
    self.serverTransport.listen()
    while True:
      try:
        client = self.serverTransport.accept()
        t = threading.Thread(target = self.handle, args=(client,))
        t.start()
      except Exception, x:
        print '%s, %s, %s,' % (type(x), x, traceback.format_exc())

  def handle(self, client):
    (input, output) = self.transportFactory.getIOTransports(client)
    try:
      while True:
        self.processor.process(input, output)
    except TTransport.TTransportException, tx:
      pass
    except Exception, x:
      print '%s, %s, %s' % (type(x), x, traceback.format_exc())

class TThreadPoolServer(TServer):

  """Server with a fixed size pool of threads which service requests."""

  def __init__(self, processor, serverTransport, transportFactory=None):
    TServer.__init__(self, processor, serverTransport, transportFactory)
    self.clients = Queue.Queue()
    self.threads = 10

  def setNumThreads(self, num):
    """Set the number of worker threads that should be created"""
    self.threads = num

  def serveThread(self):
    """Loop around getting clients from the shared queue and process them."""
    while True:
      try:
        client = self.client.get()
        self.serveClient(client)
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())
      
  def serveClient(self, client):
    """Process input/output from a client for as long as possible"""
    (input, output) = self.transportFactory.getIOTransports(client)
    try:
      while True:
        self.processor.process(input, output)
    except TTransport.TTransportException, tx:
      pass
    except Exception, x:
      print '%s, %s, %s' % (type(x), x, traceback.format_exc())

  def serve(self):
    """Start a fixed number of worker threads and put client into a queue"""
    for i in range(self.threads):
      try:
        t = threading.Thread(target = self.serveThread)
        t.start()
      except Exception, x:
        print '%s, %s, %s,' % (type(x), x, traceback.format_exc())
    
    # Pump the socket for clients
    self.serverTransport.listen()
    while True:
      try:
        client = self.serverTransport.accept()
        self.clients.put(client)
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())
