#!/usr/bin/env python
#
# Copyright (c) 2006- Facebook
# Distributed under the Thrift Software License
#
# See accompanying file LICENSE or visit the Thrift site at:
# http://developers.facebook.com/thrift/

import sys
import traceback
import threading
import Queue

from thrift.Thrift import TProcessor
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

class TServer:

  """Base interface for a server, which must have a serve method."""

  """ 3 constructors for all servers:
  1) (processor, serverTransport)
  2) (processor, serverTransport, transportFactory, protocolFactory)
  3) (processor, serverTransport,
      inputTransportFactory, outputTransportFactory,
      inputProtocolFactory, outputProtocolFactory)"""
  def __init__(self, *args):
    if (len(args) == 2):
      self.__initArgs__(args[0], args[1],
                        TTransport.TTransportFactoryBase(),
                        TTransport.TTransportFactoryBase(),
                        TBinaryProtocol.TBinaryProtocolFactory(),
                        TBinaryProtocol.TBinaryProtocolFactory())
    elif (len(args) == 4):
      self.__initArgs__(args[0], args[1], args[2], args[2], args[3], args[3])
    elif (len(args) == 6):
      self.__initArgs__(args[0], args[1], args[2], args[3], args[4], args[5])

  def __initArgs__(self, processor, serverTransport,
                   inputTransportFactory, outputTransportFactory,
                   inputProtocolFactory, outputProtocolFactory):
    self.processor = processor
    self.serverTransport = serverTransport
    self.inputTransportFactory = inputTransportFactory
    self.outputTransportFactory = outputTransportFactory
    self.inputProtocolFactory = inputProtocolFactory
    self.outputProtocolFactory = outputProtocolFactory

  def serve(self):
    pass

class TSimpleServer(TServer):

  """Simple single-threaded server that just pumps around one transport."""

  def __init__(self, *args):
    TServer.__init__(self, *args)

  def serve(self):
    self.serverTransport.listen()
    while True:
      client = self.serverTransport.accept()
      itrans = self.inputTransportFactory.getTransport(client)
      otrans = self.outputTransportFactory.getTransport(client)
      iprot = self.inputProtocolFactory.getProtocol(itrans)
      oprot = self.outputProtocolFactory.getProtocol(otrans)
      try:
        while True:
          self.processor.process(iprot, oprot)
      except TTransport.TTransportException, tx:
        pass
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())

      itrans.close()
      otrans.close()

class TThreadedServer(TServer):

  """Threaded server that spawns a new thread per each connection."""

  def __init__(self, *args):
    TServer.__init__(self, *args)

  def serve(self):
    self.serverTransport.listen()
    while True:
      try:
        client = self.serverTransport.accept()
        t = threading.Thread(target = self.handle, args=(client,))
        t.start()
      except KeyboardInterrupt:
        raise
      except Exception, x:
        print '%s, %s, %s,' % (type(x), x, traceback.format_exc())

  def handle(self, client):
    itrans = self.inputTransportFactory.getTransport(client)
    otrans = self.outputTransportFactory.getTransport(client)
    iprot = self.inputProtocolFactory.getProtocol(itrans)
    oprot = self.outputProtocolFactory.getProtocol(otrans)
    try:
      while True:
        self.processor.process(iprot, oprot)
    except TTransport.TTransportException, tx:
      pass
    except Exception, x:
      print '%s, %s, %s' % (type(x), x, traceback.format_exc())

    itrans.close()
    otrans.close()

class TThreadPoolServer(TServer):

  """Server with a fixed size pool of threads which service requests."""

  def __init__(self, *args):
    TServer.__init__(self, *args)
    self.clients = Queue.Queue()
    self.threads = 10

  def setNumThreads(self, num):
    """Set the number of worker threads that should be created"""
    self.threads = num

  def serveThread(self):
    """Loop around getting clients from the shared queue and process them."""
    while True:
      try:
        client = self.clients.get()
        self.serveClient(client)
      except Exception, x:
        print '%s, %s, %s' % (type(x), x, traceback.format_exc())

  def serveClient(self, client):
    """Process input/output from a client for as long as possible"""
    itrans = self.inputTransportFactory.getTransport(client)
    otrans = self.outputTransportFactory.getTransport(client)
    iprot = self.inputProtocolFactory.getProtocol(itrans)
    oprot = self.outputProtocolFactory.getProtocol(otrans)
    try:
      while True:
        self.processor.process(iprot, oprot)
    except TTransport.TTransportException, tx:
      pass
    except Exception, x:
      print '%s, %s, %s' % (type(x), x, traceback.format_exc())

    itrans.close()
    otrans.close()

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
