#!/usr/bin/python

import sys
sys.path.append('./gen-py')

import ThriftTest
from ThriftTest_types import *
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer

class TestHandler:

  def testVoid(self):
    print 'testVoid()'

  def testString(self, str):
    print 'testString(%s)' % str
    return str

  def testByte(self, byte):
    print 'testByte(%d)' % byte
    return byte

  def testException(self, str):
    print 'testException(%s)' % str
    x = Xception()
    x.errorCode = 1001
    x.message = str
    raise x

transport = TSocket.TServerSocket(9090)
protocol = TBinaryProtocol.TBinaryProtocol()
handler = TestHandler()
iface = ThriftTest.Server(handler, protocol)
server = TServer.TSimpleServer(iface, transport)
server.run()
