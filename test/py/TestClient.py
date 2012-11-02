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

import sys, glob, os
sys.path.insert(0, glob.glob(os.path.join(os.path.dirname(__file__),'../../lib/py/build/lib.*'))[0])

import unittest
import time
from optparse import OptionParser

parser = OptionParser()
parser.add_option('--genpydir', type='string', dest='genpydir',
                  default='gen-py',
                  help='include this local directory in sys.path for locating generated code')
parser.add_option("--port", type="int", dest="port",
    help="connect to server at port")
parser.add_option("--host", type="string", dest="host",
    help="connect to server")
parser.add_option("--zlib", action="store_true", dest="zlib",
    help="use zlib wrapper for compressed transport")
parser.add_option("--ssl", action="store_true", dest="ssl",
    help="use SSL for encrypted transport")
parser.add_option("--framed", action="store_true", dest="framed",
    help="use framed transport")
parser.add_option("--http", dest="http_path",
    help="Use the HTTP transport with the specified path")
parser.add_option('-v', '--verbose', action="store_const", 
    dest="verbose", const=2,
    help="verbose output")
parser.add_option('-q', '--quiet', action="store_const", 
    dest="verbose", const=0,
    help="minimal output")
parser.add_option('--proto',  dest="proto", type="string",
    help="protocol to use, one of: accel, binary, compact")
parser.set_defaults(framed=False, http_path=None, verbose=1, host='localhost', port=9090, proto='binary')
options, args = parser.parse_args()

sys.path.insert(0, options.genpydir)

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.transport import THttpClient
from thrift.transport import TZlibTransport
from thrift.protocol import TBinaryProtocol
from thrift.protocol import TCompactProtocol
from thrift.protocol import TJSONProtocol

class AbstractTest(unittest.TestCase):
  def setUp(self):
    if options.http_path:
      self.transport = THttpClient.THttpClient(options.host, port=options.port, path=options.http_path)
    else:
      if options.ssl:
        from thrift.transport import TSSLSocket
        socket = TSSLSocket.TSSLSocket(options.host, options.port, validate=False)
      else:
        socket = TSocket.TSocket(options.host, options.port)
      # frame or buffer depending upon args
      if options.framed:
        self.transport = TTransport.TFramedTransport(socket)
      else:
        self.transport = TTransport.TBufferedTransport(socket)
      if options.zlib:
        self.transport = TZlibTransport.TZlibTransport(self.transport, 9)
    self.transport.open()
    protocol = self.protocol_factory.getProtocol(self.transport)
    self.client = ThriftTest.Client(protocol)

  def tearDown(self):
    # Close!
    self.transport.close()

  def testVoid(self):
    self.client.testVoid()

  def testString(self):
    self.assertEqual(self.client.testString('Python' * 20), 'Python' * 20)
    self.assertEqual(self.client.testString(''), '')

  def testByte(self):
    self.assertEqual(self.client.testByte(63), 63)
    self.assertEqual(self.client.testByte(-127), -127)

  def testI32(self):
    self.assertEqual(self.client.testI32(-1), -1)
    self.assertEqual(self.client.testI32(0), 0)

  def testI64(self):
    self.assertEqual(self.client.testI64(1), 1)
    self.assertEqual(self.client.testI64(-34359738368), -34359738368)

  def testDouble(self):
    self.assertEqual(self.client.testDouble(-5.235098235), -5.235098235)
    self.assertEqual(self.client.testDouble(0), 0)
    self.assertEqual(self.client.testDouble(-1), -1)

  def testStruct(self):
    x = Xtruct()
    x.string_thing = "Zero"
    x.byte_thing = 1
    x.i32_thing = -3
    x.i64_thing = -5
    y = self.client.testStruct(x)
    self.assertEqual(y, x)

  def testNest(self):
    inner = Xtruct(string_thing="Zero", byte_thing=1, i32_thing=-3,
      i64_thing=-5)
    x = Xtruct2(struct_thing=inner, byte_thing=0, i32_thing=0)
    y = self.client.testNest(x)
    self.assertEqual(y, x)

  def testMap(self):
    x = {0:1, 1:2, 2:3, 3:4, -1:-2}
    y = self.client.testMap(x)
    self.assertEqual(y, x)

  def testSet(self):
    x = set([8, 1, 42])
    y = self.client.testSet(x)
    self.assertEqual(y, x)

  def testList(self):
    x = [1, 4, 9, -42]
    y = self.client.testList(x)
    self.assertEqual(y, x)

  def testEnum(self):
    x = Numberz.FIVE
    y = self.client.testEnum(x)
    self.assertEqual(y, x)

  def testTypedef(self):
    x = 0xffffffffffffff # 7 bytes of 0xff
    y = self.client.testTypedef(x)
    self.assertEqual(y, x)

  def testMapMap(self):
    # does not work: dict() is not a hashable type, so a dict() cannot be used as a key in another dict()
    #x = { {1:10, 2:20}, {1:100, 2:200, 3:300}, {1:1000, 2:2000, 3:3000, 4:4000} }
    try:
      y = self.client.testMapMap()
    except:
      pass

  def testMulti(self):
    xpected = Xtruct(string_thing='Hello2', byte_thing=74, i32_thing=0xff00ff, i64_thing=0xffffffffd0d0)
    y = self.client.testMulti(xpected.byte_thing,
          xpected.i32_thing,
          xpected.i64_thing,
          { 0:'abc' },
          Numberz.FIVE,
          0xf0f0f0)  
    self.assertEqual(y, xpected)

  def testException(self):
    self.client.testException('Safe')
    try:
      self.client.testException('Xception')
      self.fail("should have gotten exception")
    except Xception, x:
      self.assertEqual(x.errorCode, 1001)
      self.assertEqual(x.message, 'Xception')
      # TODO ensure same behavior for repr within generated python variants
      # ensure exception's repr method works
      #x_repr = repr(x)
      #self.assertEqual(x_repr, 'Xception(errorCode=1001, message=\'Xception\')')

    try:
      self.client.testException("throw_undeclared")
      self.fail("should have thrown exception")
    except Exception: # type is undefined
      pass

  def testOneway(self):
    start = time.time()
    self.client.testOneway(1) # type is int, not float
    end = time.time()
    self.assertTrue(end - start < 3,
                    "oneway sleep took %f sec" % (end - start))
  
  def testOnewayThenNormal(self):
    self.client.testOneway(1) # type is int, not float
    self.assertEqual(self.client.testString('Python'), 'Python')

class NormalBinaryTest(AbstractTest):
  protocol_factory = TBinaryProtocol.TBinaryProtocolFactory()

class CompactTest(AbstractTest):
  protocol_factory = TCompactProtocol.TCompactProtocolFactory()

class JSONTest(AbstractTest):
  protocol_factory = TJSONProtocol.TJSONProtocolFactory()

class AcceleratedBinaryTest(AbstractTest):
  protocol_factory = TBinaryProtocol.TBinaryProtocolAcceleratedFactory()

def suite():
  suite = unittest.TestSuite()
  loader = unittest.TestLoader()
  if options.proto == 'binary': # look for --proto on cmdline
    suite.addTest(loader.loadTestsFromTestCase(NormalBinaryTest))
  elif options.proto == 'accel':
    suite.addTest(loader.loadTestsFromTestCase(AcceleratedBinaryTest))
  elif options.proto == 'compact':
    suite.addTest(loader.loadTestsFromTestCase(CompactTest))
  elif options.proto == 'json':
    suite.addTest(loader.loadTestsFromTestCase(JSONTest))
  else:
    raise AssertionError('Unknown protocol given with --proto: %s' % options.proto)
  return suite

class OwnArgsTestProgram(unittest.TestProgram):
    def parseArgs(self, argv):
        if args:
            self.testNames = args
        else:
            self.testNames = (self.defaultTest,)
        self.createTests()

if __name__ == "__main__":
  OwnArgsTestProgram(defaultTest="suite", testRunner=unittest.TextTestRunner(verbosity=1))
