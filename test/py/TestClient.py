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
parser.add_option("--http", dest="http_path",
    help="Use the HTTP transport with the specified path")
parser.add_option('-v', '--verbose', action="store_const",
    dest="verbose", const=2,
    help="verbose output")
parser.add_option('-q', '--quiet', action="store_const",
    dest="verbose", const=0,
    help="minimal output")
parser.add_option('--protocol',  dest="proto", type="string",
    help="protocol to use, one of: accel, binary, compact, json")
parser.add_option('--transport',  dest="trans", type="string",
    help="transport to use, one of: buffered, framed")
parser.set_defaults(framed=False, http_path=None, verbose=1, host='localhost', port=9090, proto='binary')
options, args = parser.parse_args()

script_dir = os.path.dirname(__file__)
sys.path.insert(0, os.path.join(script_dir, options.genpydir))

from ThriftTest import ThriftTest, SecondService
from ThriftTest.ttypes import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.transport import THttpClient
from thrift.transport import TZlibTransport
from thrift.protocol import TBinaryProtocol
from thrift.protocol import TCompactProtocol
from thrift.protocol import TJSONProtocol

TEST_BASETYPES = 1
TEST_STRUCTS = 2
TEST_CONTAINERS = 4
TEST_EXCEPTIONS = 8
TEST_COMPLEX = 16
TEST_ENUMTYPEDEF = 32
TEST_ONEWAY = 64
TEST_NOTUSED = 128

test_basetypes_fails = False
test_structs_fails = False
test_containers_fails = False
test_exceptions_fails = False
test_complex_fails = False
test_enumtypedef_fails = False
test_oneway_fails = False

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
      self.transport = TTransport.TBufferedTransport(socket)
      if options.trans == 'framed':
        self.transport = TTransport.TFramedTransport(socket)
      elif options.trans == 'buffered':
        self.transport = TTransport.TBufferedTransport(socket)
      elif options.trans == '':
        raise AssertionError('Unknown --transport option: %s' % options.trans)
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
    global test_basetypes_fails
    try:
      self.assertEqual(self.client.testString('Python' * 20), 'Python' * 20)
    except AssertionError, e:
      test_basetypes_fails = True
      print test_basetypes_fails
      raise AssertionError( e.args )
    try:
      self.assertEqual(self.client.testString(''), '')
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )

  def testByte(self):
    global test_basetypes_fails
    try:
      self.assertEqual(self.client.testByte(63), 63)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )
    try:
      self.assertEqual(self.client.testByte(-127), -127)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )

  def testI32(self):
    global test_basetypes_fails
    try:
      self.assertEqual(self.client.testI32(-1), -1)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )
    try:
      self.assertEqual(self.client.testI32(0), 0)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )

  def testI64(self):
    global test_basetypes_fails
    try:
      self.assertEqual(self.client.testI64(1), 1)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )
    try:
      self.assertEqual(self.client.testI64(-34359738368), -34359738368)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )

  def testDouble(self):
    global test_basetypes_fails
    try:
      self.assertEqual(self.client.testDouble(-5.235098235), -5.235098235)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )
    try:
      self.assertEqual(self.client.testDouble(0), 0)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )
    try:
      self.assertEqual(self.client.testDouble(-1), -1)
    except AssertionError, e:
      test_basetypes_fails = True
      raise AssertionError( e.args )

  def testStruct(self):
    global test_structs_fails
    x = Xtruct()
    x.string_thing = "Zero"
    x.byte_thing = 1
    x.i32_thing = -3
    x.i64_thing = -5
    y = self.client.testStruct(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_structs_fails = True
      raise AssertionError( e.args )

  def testNest(self):
    global test_structs_fails
    inner = Xtruct(string_thing="Zero", byte_thing=1, i32_thing=-3,
      i64_thing=-5)
    x = Xtruct2(struct_thing=inner, byte_thing=0, i32_thing=0)
    y = self.client.testNest(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_structs_fails = True
      raise AssertionError( e.args )

  def testMap(self):
    global test_containers_fails
    x = {0:1, 1:2, 2:3, 3:4, -1:-2}
    y = self.client.testMap(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_containers_fails = True
      raise AssertionError( e.args )

  def testSet(self):
    global test_containers_fails
    x = set([8, 1, 42])
    y = self.client.testSet(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_containers_fails = True
      raise AssertionError( e.args )

  def testList(self):
    global test_containers_fails
    x = [1, 4, 9, -42]
    y = self.client.testList(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_containers_fails = True
      raise AssertionError( e.args )

  def testEnum(self):
    global test_enumtypedef_fails
    x = Numberz.FIVE
    y = self.client.testEnum(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_enumtypedef_fails = True
      raise AssertionError( e.args )

  def testTypedef(self):
    global test_enumtypedef_fails
    x = 0xffffffffffffff # 7 bytes of 0xff
    y = self.client.testTypedef(x)
    try:
      self.assertEqual(y, x)
    except AssertionError, e:
      test_enumtypedef_fails = True
      raise AssertionError( e.args )

  def testMapMap(self):
    # does not work: dict() is not a hashable type, so a dict() cannot be used as a key in another dict()
    #x = { {1:10, 2:20}, {1:100, 2:200, 3:300}, {1:1000, 2:2000, 3:3000, 4:4000} }
    try:
      y = self.client.testMapMap()
    except:
      pass

  def testMulti(self):
    global test_complex_fails
    xpected = Xtruct(string_thing='Hello2', byte_thing=74, i32_thing=0xff00ff, i64_thing=0xffffffffd0d0)
    y = self.client.testMulti(xpected.byte_thing,
          xpected.i32_thing,
          xpected.i64_thing,
          { 0:'abc' },
          Numberz.FIVE,
          0xf0f0f0)
    try:
      self.assertEqual(y, xpected)
    except AssertionError, e:
      test_complex_fails = True
      raise AssertionError( e.args )

  def testException(self):
    global test_exceptions_fails
    self.client.testException('Safe')
    try:
      self.client.testException('Xception')
      test_exceptions_fails = True
      self.fail("should have gotten exception")
    except Xception, x:
      try:
        self.assertEqual(x.errorCode, 1001)
      except AssertionError, e:
        test_complex_fails = True
        raise AssertionError( e.args )
      try:
        self.assertEqual(x.message, 'Xception')
      except AssertionError, e:
        test_complex_fails = True
        raise AssertionError( e.args )
      # TODO ensure same behavior for repr within generated python variants
      # ensure exception's repr method works
      #x_repr = repr(x)
      #self.assertEqual(x_repr, 'Xception(errorCode=1001, message=\'Xception\')')

    try:
      self.client.testException("throw_undeclared")
      self.fail("should have thrown exception")
      test_exceptions_fails = True
    except Exception: # type is undefined
      pass

  def testOneway(self):
    global test_oneway_fails
    start = time.time()
    self.client.testOneway(1) # type is int, not float
    end = time.time()
    try:
      self.assertTrue(end - start < 3,
                    "oneway sleep took %f sec" % (end - start))
    except AssertionError, e:
      test_oneway_fails = True
      raise AssertionError( e.args )

  def testOnewayThenNormal(self):
    global test_oneway_fails
    self.client.testOneway(1) # type is int, not float
    try:
      self.assertEqual(self.client.testString('Python'), 'Python')
    except AssertionError, e:
      test_oneway_fails = True
      raise AssertionError( e.args )

  @classmethod
  def tearDownClass(cls):
    global test_basetypes_fails
    global test_structs_fails
    global test_containers_fails
    global test_exceptions_fails
    global test_complex_fails
    global test_enumtypedef_fails
    global test_oneway_fails

    global TEST_BASETYPES
    global TEST_STRUCTS
    global TEST_CONTAINERS
    global TEST_EXCEPTIONS
    global TEST_COMPLEX
    global TEST_ENUMTYPEDEF
    global TEST_ONEWAY
    global TEST_NOTUSED

    ret = 255 - TEST_NOTUSED
    if(not test_basetypes_fails):
      ret = ret - TEST_BASETYPES
    if(not test_structs_fails):
      ret = ret - TEST_STRUCTS
    if(not test_containers_fails):
      ret = ret - TEST_CONTAINERS
    if(not test_exceptions_fails):
      ret = ret - TEST_EXCEPTIONS
    if(not test_complex_fails):
      ret = ret - TEST_COMPLEX
    if(not test_enumtypedef_fails):
      ret = ret - TEST_ENUMTYPEDEF
    if(not test_oneway_fails):
      ret = ret - TEST_ONEWAY
    if(ret != 0):
      sys.exit(ret)

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
    raise AssertionError('Unknown protocol given with --protocol: %s' % options.proto)
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
