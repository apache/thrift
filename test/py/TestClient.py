#!/usr/bin/python

import sys
sys.path.append('./gen-py')

import ThriftTest
from ThriftTest_types import *
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol

transport = TSocket.TSocket('localhost', 9090)
protocol = TBinaryProtocol.TBinaryProtocol()
client = ThriftTest.Client(transport, protocol)

transport.open()

print "testVoid()"
print client.testVoid()

print "testString('PythonTest')"
print client.testString('PythonTest')

print "testByte(63)"
print client.testByte(63)

print "testException('Safe')"
print client.testException('Safe')

print "textException('Xception')"
try:
  print client.testException('Xception')
except Xception, x:
  print 'Xception (%d, %s)' % (x.errorCode, x.message)

transport.close()
