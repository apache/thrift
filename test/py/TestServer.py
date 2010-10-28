#!/usr/bin/env python

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

import sys, glob, time
sys.path.insert(0, './gen-py')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer, TNonblockingServer, THttpServer

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

  def testOneway(self, seconds):
    print 'testOneway(%d) => sleeping...' % seconds
    time.sleep(seconds)
    print 'done sleeping'

  def testNest(self, thing):
    return thing

  def testMap(self, thing):
    return thing

  def testSet(self, thing):
    return thing

  def testList(self, thing):
    return thing

  def testEnum(self, thing):
    return thing

  def testTypedef(self, thing):
    return thing

pfactory = TBinaryProtocol.TBinaryProtocolFactory()
handler = TestHandler()
processor = ThriftTest.Processor(handler)

if sys.argv[1] == "THttpServer":
  server = THttpServer.THttpServer(processor, ('', 9090), pfactory)
else:
  transport = TSocket.TServerSocket(9090)
  tfactory = TTransport.TBufferedTransportFactory()

  if sys.argv[1] == "TNonblockingServer":
    server = TNonblockingServer.TNonblockingServer(processor, transport)
  else:
    ServerClass = getattr(TServer, sys.argv[1])
    server = ServerClass(processor, transport, tfactory, pfactory)

server.serve()
