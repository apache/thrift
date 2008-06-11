#!/usr/bin/env python

import sys, glob, time
sys.path.insert(0, './gen-py')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *
from thrift.transport import TTransport
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

  def testI16(self, i16):
    print 'testI16(%d)' % i16
    return i16

  def testI32(self, i32):
    print 'testI32(%d)' % i32
    return i32

  def testI64(self, i64):
    print 'testI64(%d)' % i64
    return i64

  def testDouble(self, dub):
    print 'testDouble(%f)' % dub
    return dub

  def testStruct(self, thing):
    print 'testStruct({%s, %d, %d, %d})' % (thing.string_thing, thing.byte_thing, thing.i32_thing, thing.i64_thing)
    return thing

  def testException(self, str):
    print 'testException(%s)' % str
    if str == 'Xception':
      x = Xception()
      x.errorCode = 1001
      x.message = str
      raise x
    elif str == "throw_undeclared":
      raise ValueError("foo")

  def testAsync(self, seconds):
    print 'testAsync(%d) => sleeping...' % seconds
    time.sleep(seconds)
    print 'done sleeping'

  def testAsync(self, seconds):
    print 'testAsync(%d) => sleeping...' % seconds
    time.sleep(seconds)
    print 'done sleeping'

handler = TestHandler()
processor = ThriftTest.Processor(handler)
transport = TSocket.TServerSocket(9090)
tfactory = TTransport.TBufferedTransportFactory()
pfactory = TBinaryProtocol.TBinaryProtocolFactory()

ServerClass = getattr(TServer, sys.argv[1])

server = ServerClass(processor, transport, tfactory, pfactory)
server.serve()
