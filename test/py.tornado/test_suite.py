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

import datetime
import glob
import sys
import time
import unittest

sys.path.insert(0, './gen-py.tornado')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])

try:
    __import__('tornado')
except ImportError:
    print "module `tornado` not found, skipping test"
    sys.exit(0)

from tornado import gen, ioloop, stack_context
from tornado.testing import AsyncTestCase, get_unused_port

from thrift import TTornado
from thrift.protocol import TBinaryProtocol

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *


class TestHandler(object):
    def __init__(self, test_instance):
        self.test_instance = test_instance

    def testVoid(self, callback):
        callback()

    def testString(self, s, callback):
        callback(s)

    def testByte(self, b, callback):
        callback(b)

    def testI16(self, i16, callback):
        callback(i16)

    def testI32(self, i32, callback):
        callback(i32)

    def testI64(self, i64, callback):
        callback(i64)

    def testDouble(self, dub, callback):
        callback(dub)

    def testStruct(self, thing, callback):
        callback(thing)

    def testException(self, s, callback):
        if s == 'Xception':
            x = Xception()
            x.errorCode = 1001
            x.message = s
            raise x
        elif s == 'throw_undeclared':
            raise ValueError("foo")
        callback()

    def testOneway(self, seconds, callback=None):
        start = time.time()
        def fire_oneway():
            end = time.time()
            self.test_instance.stop((start, end, seconds))

        ioloop.IOLoop.instance().add_timeout(
            datetime.timedelta(seconds=seconds),
            fire_oneway)

        if callback:
            callback()

    def testNest(self, thing, callback):
        callback(thing)

    def testMap(self, thing, callback):
        callback(thing)

    def testSet(self, thing, callback):
        callback(thing)

    def testList(self, thing, callback):
        callback(thing)

    def testEnum(self, thing, callback):
        callback(thing)

    def testTypedef(self, thing, callback):
        callback(thing)


class ThriftTestCase(AsyncTestCase):
    def get_new_ioloop(self):
        return ioloop.IOLoop.instance()

    def setUp(self):
        self.port = get_unused_port()
        self.io_loop = self.get_new_ioloop()

        # server
        self.handler = TestHandler(self)
        self.processor = ThriftTest.Processor(self.handler)
        self.pfactory = TBinaryProtocol.TBinaryProtocolFactory()

        self.server = TTornado.TTornadoServer(self.processor, self.pfactory)
        self.server.bind(self.port)
        self.server.start(1)

        # client
        transport = TTornado.TTornadoStreamTransport('localhost', self.port)
        pfactory = TBinaryProtocol.TBinaryProtocolFactory()
        self.client = ThriftTest.Client(transport, pfactory)
        transport.open(callback=self.stop)
        self.wait(timeout=1)

    def test_void(self):
        self.client.testVoid(callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, None)

    def test_string(self):
        self.client.testString('Python', callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, 'Python')

    def test_byte(self):
        self.client.testByte(63, callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, 63)

    def test_i32(self):
        self.client.testI32(-1, callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, -1)

        self.client.testI32(0, callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, 0)

    def test_i64(self):
        self.client.testI64(-34359738368, callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, -34359738368)

    def test_double(self):
        self.client.testDouble(-5.235098235, callback=self.stop)
        v = self.wait(timeout=1)
        self.assertEquals(v, -5.235098235)

    def test_struct(self):
        x = Xtruct()
        x.string_thing = "Zero"
        x.byte_thing = 1
        x.i32_thing = -3
        x.i64_thing = -5
        self.client.testStruct(x, callback=self.stop)

        y = self.wait(timeout=1)
        self.assertEquals(y.string_thing, "Zero")
        self.assertEquals(y.byte_thing, 1)
        self.assertEquals(y.i32_thing, -3)
        self.assertEquals(y.i64_thing, -5)

    def test_exception(self):
        self.client.testException('Safe', callback=self.stop)
        v = self.wait(timeout=1)

        self.client.testException('Xception', callback=self.stop)
        ex = self.wait(timeout=1)
        if type(ex) == Xception:
            self.assertEquals(ex.errorCode, 1001)
            self.assertEquals(ex.message, 'Xception')
        else:
            self.fail("should have gotten exception")

    def test_oneway(self):
        def return_from_send():
            self.stop('done with send')
        self.client.testOneway(0.5, callback=return_from_send)
        self.assertEquals(self.wait(timeout=1), 'done with send')

        start, end, seconds = self.wait(timeout=1)
        self.assertAlmostEquals(seconds, (end - start), places=3)


def suite():
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    suite.addTest(loader.loadTestsFromTestCase(ThriftTestCase))
    return suite


if __name__ == '__main__':
    unittest.TestProgram(defaultTest='suite',
                         testRunner=unittest.TextTestRunner(verbosity=1))
