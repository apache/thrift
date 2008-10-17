#!/usr/bin/env python

import sys, glob
sys.path.insert(0, './gen-py')
sys.path.insert(0, glob.glob('../../lib/py/build/lib.*')[0])

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol
import unittest
import time
import socket
import random
from optparse import OptionParser

class TimeoutTest(unittest.TestCase):
    def setUp(self):
        for i in xrange(50):
            try:
                # find a port we can use
                self.listen_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                self.port = random.randint(10000, 30000)
                self.listen_sock.bind(('localhost', self.port))
                self.listen_sock.listen(5)
                break
            except:
                if i == 49:
                    raise

    def testConnectTimeout(self):
        starttime = time.time()

        try:
            leaky = []
            for i in xrange(100):
                socket = TSocket.TSocket('localhost', self.port)
                socket.setTimeout(10)
                socket.open()
                leaky.append(socket)
        except:
            self.assert_(time.time() - starttime < 5.0)

    def testWriteTimeout(self):
        starttime = time.time()

        try:
            socket = TSocket.TSocket('localhost', self.port)
            socket.setTimeout(10)
            socket.open()
            lsock = self.listen_sock.accept()
            while True:
                socket.write("hi" * 100)

        except:
            self.assert_(time.time() - starttime < 5.0)

suite = unittest.TestSuite()
loader = unittest.TestLoader()

suite.addTest(loader.loadTestsFromTestCase(TimeoutTest))

testRunner = unittest.TextTestRunner(verbosity=2)
testRunner.run(suite)
