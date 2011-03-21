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
from __future__ import division
import sys, glob, time
sys.path.insert(0, './gen-py')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])
from optparse import OptionParser

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol
from thrift.protocol import TCompactProtocol
from thrift.server import TServer, TNonblockingServer, THttpServer

parser = OptionParser()
parser.set_defaults(port=9090, verbose=1, proto='binary')
parser.add_option("--port", type="int", dest="port",
    help="port number for server to listen on")
parser.add_option('-v', '--verbose', action="store_const", 
    dest="verbose", const=2,
    help="verbose output")
parser.add_option('--proto',  dest="proto", type="string",
    help="protocol to use, one of: accel, binary, compact")
options, args = parser.parse_args()

class TestHandler:

  def testVoid(self):
    if options.verbose:
      print 'testVoid()'

  def testString(self, str):
    if options.verbose:
      print 'testString(%s)' % str
    return str

  def testByte(self, byte):
    if options.verbose:
      print 'testByte(%d)' % byte
    return byte

  def testI16(self, i16):
    if options.verbose:
      print 'testI16(%d)' % i16
    return i16

  def testI32(self, i32):
    if options.verbose:
      print 'testI32(%d)' % i32
    return i32

  def testI64(self, i64):
    if options.verbose:
      print 'testI64(%d)' % i64
    return i64

  def testDouble(self, dub):
    if options.verbose:
      print 'testDouble(%f)' % dub
    return dub

  def testStruct(self, thing):
    if options.verbose:
      print 'testStruct({%s, %d, %d, %d})' % (thing.string_thing, thing.byte_thing, thing.i32_thing, thing.i64_thing)
    return thing

  def testException(self, str):
    if options.verbose:
      print 'testException(%s)' % str
    if str == 'Xception':
      x = Xception()
      x.errorCode = 1001
      x.message = str
      raise x
    elif str == "throw_undeclared":
      raise ValueError("foo")

  def testOneway(self, seconds):
    if options.verbose:
      print 'testOneway(%d) => sleeping...' % seconds
    time.sleep(seconds / 3) # be quick
    if options.verbose:
      print 'done sleeping'

  def testNest(self, thing):
    if options.verbose:
      print 'testNest(%s)' % thing
    return thing

  def testMap(self, thing):
    if options.verbose:
      print 'testMap(%s)' % thing
    return thing

  def testSet(self, thing):
    if options.verbose:
      print 'testSet(%s)' % thing
    return thing

  def testList(self, thing):
    if options.verbose:
      print 'testList(%s)' % thing
    return thing

  def testEnum(self, thing):
    if options.verbose:
      print 'testEnum(%s)' % thing
    return thing

  def testTypedef(self, thing):
    if options.verbose:
      print 'testTypedef(%s)' % thing
    return thing

  def testMapMap(self, thing):
    if options.verbose:
      print 'testMapMap(%s)' % thing
    return thing

  def testMulti(self, arg0, arg1, arg2, arg3, arg4, arg5):
    if options.verbose:
      print 'testMulti(%s)' % [arg0, arg1, arg2, arg3, arg4, arg5]
    x = Xtruct(byte_thing=arg0, i32_thing=arg1, i64_thing=arg2)
    return x

if options.proto == 'binary':
  pfactory = TBinaryProtocol.TBinaryProtocolFactory()
elif options.proto == 'accel':
  pfactory = TBinaryProtocol.TBinaryProtocolAcceleratedFactory()
elif options.proto == 'compact':
  pfactory = TCompactProtocol.TCompactProtocolFactory()
else:
  raise AssertionError('Unknown --proto option: %s' % options.proto)
handler = TestHandler()
processor = ThriftTest.Processor(handler)

if args[0] == "THttpServer":
  server = THttpServer.THttpServer(processor, ('', options.port), pfactory)
else:
  host = None
  transport = TSocket.TServerSocket(host, options.port)
  tfactory = TTransport.TBufferedTransportFactory()

  if args[0] == "TNonblockingServer":
    server = TNonblockingServer.TNonblockingServer(processor, transport, inputProtocolFactory=pfactory)
  elif args[0] == "TProcessPoolServer":
    import signal
    def set_alarm():
      def clean_shutdown(signum, frame):
        for worker in server.workers:
          print 'Terminating worker: %s' % worker
          worker.terminate()
        print 'Requesting server to stop()'
        server.stop()
      signal.signal(signal.SIGALRM, clean_shutdown)
      signal.alarm(2)
    from thrift.server import TProcessPoolServer
    server = TProcessPoolServer.TProcessPoolServer(processor, transport, tfactory, pfactory)
    server.setNumWorkers(5)
    set_alarm()
  else:
    ServerClass = getattr(TServer, args[0])
    server = ServerClass(processor, transport, tfactory, pfactory)

server.serve()
