#!/usr/bin/env python
# -*- coding: utf-8 -*-
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

import asyncio
import os
import ssl
import sys
import time
import unittest
from optparse import OptionParser

from util import local_libpath

SCRIPT_DIR = os.path.abspath(os.path.dirname(__file__))


def async_test(f):
    def wrapper(*args, **kwargs):
        coro = asyncio.coroutine(f)
        future = coro(*args, **kwargs)
        loop = asyncio.get_event_loop()
        loop.run_until_complete(future)
    return wrapper


class AbstractTest(unittest.TestCase):
    @async_test
    def setUp(self):
        if options.ssl:
            ssl_ctx = ssl.create_default_context()
            ssl_ctx.check_hostname = False
            ssl_ctx.verify_mode = ssl.CERT_NONE
        else:
            ssl_ctx = None

        if options.trans == 'framed':
            self.transport = \
                yield from TAsyncio.TAsyncioFramedTransport.connect(
                    options.host, options.port, ssl=ssl_ctx)
        elif options.trans == 'buffered':
            self.transport = \
                yield from TAsyncio.TAsyncioBufferedTransport.connect(
                    options.host, options.port, ssl=ssl_ctx)
        elif options.trans == '':
            raise AssertionError('Unknown --transport option: %s' % options.trans)
        if options.zlib:
            self.transport = TAsyncio.TAsyncioZlibTransport(self.transport, 9)
        self.transport.open()
        protocol = self.get_protocol(self.transport)
        self.client = ThriftTest.Client(protocol)

    def tearDown(self):
        self.transport.close()

    @async_test
    def testVoid(self):
        print('testVoid')
        yield from self.client.testVoid()

    @async_test
    def testString(self):
        print('testString')
        self.assertEqual((yield from self.client.testString('Python' * 20)), 'Python' * 20)
        self.assertEqual((yield from self.client.testString('')), '')
        s1 = u'\b\t\n/\\\\\r{}:パイソン"'
        s2 = u"""Afrikaans, Alemannisch, Aragonés, العربية, مصرى,
        Asturianu, Aymar aru, Azərbaycan, Башҡорт, Boarisch, Žemaitėška,
        Беларуская, Беларуская (тарашкевіца), Български, Bamanankan,
        বাংলা, Brezhoneg, Bosanski, Català, Mìng-dĕ̤ng-ngṳ̄, Нохчийн,
        Cebuano, ᏣᎳᎩ, Česky, Словѣ́ньскъ / ⰔⰎⰑⰂⰡⰐⰠⰔⰍⰟ, Чӑвашла, Cymraeg,
        Dansk, Zazaki, ދިވެހިބަސް, Ελληνικά, Emiliàn e rumagnòl, English,
        Esperanto, Español, Eesti, Euskara, فارسی, Suomi, Võro, Føroyskt,
        Français, Arpetan, Furlan, Frysk, Gaeilge, 贛語, Gàidhlig, Galego,
        Avañe'ẽ, ગુજરાતી, Gaelg, עברית, हिन्दी, Fiji Hindi, Hrvatski,
        Kreyòl ayisyen, Magyar, Հայերեն, Interlingua, Bahasa Indonesia,
        Ilokano, Ido, Íslenska, Italiano, 日本語, Lojban, Basa Jawa,
        ქართული, Kongo, Kalaallisut, ಕನ್ನಡ, 한국어, Къарачай-Малкъар,
        Ripoarisch, Kurdî, Коми, Kernewek, Кыргызча, Latina, Ladino,
        Lëtzebuergesch, Limburgs, Lingála, ລາວ, Lietuvių, Latviešu, Basa
        Banyumasan, Malagasy, Македонски, മലയാളം, मराठी, مازِرونی, Bahasa
        Melayu, Nnapulitano, Nedersaksisch, नेपाल भाषा, Nederlands, ‪
        Norsk (nynorsk)‬, ‪Norsk (bokmål)‬, Nouormand, Diné bizaad,
        Occitan, Иронау, Papiamentu, Deitsch, Polski, پنجابی, پښتو,
        Norfuk / Pitkern, Português, Runa Simi, Rumantsch, Romani, Română,
        Русский, Саха тыла, Sardu, Sicilianu, Scots, Sámegiella, Simple
        English, Slovenčina, Slovenščina, Српски / Srpski, Seeltersk,
        Svenska, Kiswahili, தமிழ், తెలుగు, Тоҷикӣ, ไทย, Türkmençe, Tagalog,
        Türkçe, Татарча/Tatarça, Українська, اردو, Tiếng Việt, Volapük,
        Walon, Winaray, 吴语, isiXhosa, ייִדיש, Yorùbá, Zeêuws, 中文,
        Bân-lâm-gú, 粵語"""
        self.assertEqual((yield from self.client.testString(s1)), s1)
        self.assertEqual((yield from self.client.testString(s2)), s2)

    @async_test
    def testBool(self):
        print('testBool')
        self.assertEqual((yield from self.client.testBool(True)), True)
        self.assertEqual((yield from self.client.testBool(False)), False)

    @async_test
    def testByte(self):
        print('testByte')
        self.assertEqual((yield from self.client.testByte(63)), 63)
        self.assertEqual((yield from self.client.testByte(-127)), -127)

    @async_test
    def testI32(self):
        print('testI32')
        self.assertEqual((yield from self.client.testI32(-1)), -1)
        self.assertEqual((yield from self.client.testI32(0)), 0)

    @async_test
    def testI64(self):
        print('testI64')
        self.assertEqual((yield from self.client.testI64(1)), 1)
        self.assertEqual((yield from self.client.testI64(-34359738368)), -34359738368)

    @async_test
    def testDouble(self):
        print('testDouble')
        self.assertEqual((yield from self.client.testDouble(-5.235098235)), -5.235098235)
        self.assertEqual((yield from self.client.testDouble(0)), 0)
        self.assertEqual((yield from self.client.testDouble(-1)), -1)
        self.assertEqual((yield from self.client.testDouble(-0.000341012439638598279)), -0.000341012439638598279)

    @async_test
    def testBinary(self):
        print('testBinary')
        val = bytearray([i for i in range(0, 256)])
        self.assertEqual(bytearray((yield from self.client.testBinary(bytes(val)))), val)

    @async_test
    def testStruct(self):
        print('testStruct')
        x = Xtruct()
        x.string_thing = "Zero"
        x.byte_thing = 1
        x.i32_thing = -3
        x.i64_thing = -5
        y = yield from self.client.testStruct(x)
        self.assertEqual(y, x)

    @async_test
    def testNest(self):
        print('testNest')
        inner = Xtruct(string_thing="Zero", byte_thing=1, i32_thing=-3, i64_thing=-5)
        x = Xtruct2(struct_thing=inner, byte_thing=0, i32_thing=0)
        y = yield from self.client.testNest(x)
        self.assertEqual(y, x)

    @async_test
    def testMap(self):
        print('testMap')
        x = {0: 1, 1: 2, 2: 3, 3: 4, -1: -2}
        y = yield from self.client.testMap(x)
        self.assertEqual(y, x)

    @async_test
    def testSet(self):
        print('testSet')
        x = set([8, 1, 42])
        y = yield from self.client.testSet(x)
        self.assertEqual(y, x)

    @async_test
    def testList(self):
        print('testList')
        x = [1, 4, 9, -42]
        y = yield from self.client.testList(x)
        self.assertEqual(y, x)

    @async_test
    def testEnum(self):
        print('testEnum')
        x = Numberz.FIVE
        y = yield from self.client.testEnum(x)
        self.assertEqual(y, x)

    @async_test
    def testTypedef(self):
        print('testTypedef')
        x = 0xffffffffffffff  # 7 bytes of 0xff
        y = yield from self.client.testTypedef(x)
        self.assertEqual(y, x)

    @async_test
    def testMapMap(self):
        print('testMapMap')
        x = {
            -4: {-4: -4, -3: -3, -2: -2, -1: -1},
            4: {4: 4, 3: 3, 2: 2, 1: 1},
        }
        y = yield from self.client.testMapMap(42)
        self.assertEqual(y, x)

    @async_test
    def testMulti(self):
        print('testMulti')
        xpected = Xtruct(string_thing='Hello2', byte_thing=74, i32_thing=0xff00ff, i64_thing=0xffffffffd0d0)
        y = yield from self.client.testMulti(xpected.byte_thing,
                                             xpected.i32_thing,
                                             xpected.i64_thing,
                                             {0: 'abc'},
                                             Numberz.FIVE,
                                             0xf0f0f0)
        self.assertEqual(y, xpected)

    @async_test
    def testException(self):
        print('testException')
        yield from self.client.testException('Safe')
        try:
            yield from self.client.testException('Xception')
            self.fail("should have gotten exception")
        except Xception as x:
            self.assertEqual(x.errorCode, 1001)
            self.assertEqual(x.message, 'Xception')
            # TODO ensure same behavior for repr within generated python variants
            # ensure exception's repr method works
            # x_repr = repr(x)
            # self.assertEqual(x_repr, 'Xception(errorCode=1001, message=\'Xception\')')

        try:
            yield from self.client.testException('TException')
            self.fail("should have gotten exception")
        except TException as x:
            pass

        # Should not throw
        yield from self.client.testException('success')

    @async_test
    def testMultiException(self):
        print('testMultiException')
        try:
            yield from self.client.testMultiException('Xception', 'ignore')
        except Xception as ex:
            self.assertEqual(ex.errorCode, 1001)
            self.assertEqual(ex.message, 'This is an Xception')

        try:
            yield from self.client.testMultiException('Xception2', 'ignore')
        except Xception2 as ex:
            self.assertEqual(ex.errorCode, 2002)
            self.assertEqual(ex.struct_thing.string_thing, 'This is an Xception2')

        y = yield from self.client.testMultiException('success', 'foobar')
        self.assertEqual(y.string_thing, 'foobar')

    @async_test
    def testOneway(self):
        print('testOneway')
        start = time.time()
        yield from self.client.testOneway(1)  # type is int, not float
        end = time.time()
        self.assertTrue(end - start < 1,
                        "oneway sleep took %f sec" % (end - start))

    @async_test
    def testOnewayThenNormal(self):
        print('testOnewayThenNormal')
        yield from self.client.testOneway(1)  # type is int, not float
        self.assertEqual((yield from self.client.testString('Python')), 'Python')


class NormalBinaryTest(AbstractTest):
    def get_protocol(self, transport):
        return TAsyncio.TAsyncioBinaryProtocolFactory().getProtocol(transport)


class CompactTest(AbstractTest):
    def get_protocol(self, transport):
        return TAsyncio.TAsyncioCompactProtocolFactory().getProtocol(transport)


def suite():
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    if options.proto == 'binary':  # look for --proto on cmdline
        suite.addTest(loader.loadTestsFromTestCase(NormalBinaryTest))
    elif options.proto == 'compact':
        suite.addTest(loader.loadTestsFromTestCase(CompactTest))
    else:
        raise AssertionError('Unknown protocol given with --protocol: %s' % options.proto)
    return suite


class OwnArgsTestProgram(unittest.TestProgram):
    def parseArgs(self, argv):
        if args:
            self.testNames = args
        else:
            self.testNames = ([self.defaultTest])
        self.createTests()

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option('--libpydir', type='string', dest='libpydir',
                      help='include this directory in sys.path for locating library code')
    parser.add_option('--genpydir', type='string', dest='genpydir',
                      help='include this directory in sys.path for locating generated code')
    parser.add_option("--port", type="int", dest="port",
                      help="connect to server at port")
    parser.add_option("--host", type="string", dest="host",
                      help="connect to server")
    parser.add_option("--zlib", action="store_true", dest="zlib",
                      help="use zlib wrapper for compressed transport")
    parser.add_option("--ssl", action="store_true", dest="ssl",
                      help="use SSL for encrypted transport")
    parser.add_option('-v', '--verbose', action="store_const",
                      dest="verbose", const=2,
                      help="verbose output")
    parser.add_option('-q', '--quiet', action="store_const",
                      dest="verbose", const=0,
                      help="minimal output")
    parser.add_option('--protocol', dest="proto", type="string",
                      help="protocol to use, one of: binary, compact")
    parser.add_option('--transport', dest="trans", type="string",
                      help="transport to use, one of: buffered, framed")
    parser.set_defaults(framed=False, verbose=1, host='localhost', port=9090, proto='binary')
    options, args = parser.parse_args()

    if options.genpydir:
        sys.path.insert(0, os.path.join(SCRIPT_DIR, options.genpydir))
    sys.path.insert(0, local_libpath())

    from ThriftTest import ThriftTest
    from ThriftTest.ttypes import Xtruct, Xtruct2, Numberz, Xception, Xception2
    from thrift.Thrift import TException
    import thrift.TAsyncio as TAsyncio

    OwnArgsTestProgram(defaultTest="suite", testRunner=unittest.TextTestRunner(verbosity=1))
