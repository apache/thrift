#!/usr/bin/python

import sys
sys.path.append('./gen-py')

import ThriftTest
from ThriftTest_types import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol

import timing

transport = TTransport.TBufferedTransport(TSocket.TSocket('localhost', 9090))
protocol = TBinaryProtocol.TBinaryProtocol()
client = ThriftTest.Client(transport, protocol)

transport.open()

# Start debug timing
timing.start()

print "testVoid()"
print client.testVoid()

print "testString('Python')"
print client.testString('Python')

print "testByte(63)"
print client.testByte(63)

print "testI32(-1)"
print client.testI32(-1)

print "testI64(-34359738368)"
print client.testI64(-34359738368)

print "testStruct({Zero, 1, -3, -5})"
x = Xtruct()
x.string_thing = "Zero"
x.byte_thing = 1
x.i32_thing = -3
x.i64_thing = -5
x = client.testStruct(x)
print "{%s, %d, %d, %d}" % (x.string_thing, x.byte_thing, x.i32_thing, x.i64_thing)

print "testException('Safe')"
print client.testException('Safe')

try:
  print "textException('Xception')"
  print client.testException('Xception')

except Xception, x:
  print "Xception (%d, %s)" % (x.errorCode, x.message)

timing.finish()
print "Total time: %d microsecs" % timing.micro()

transport.close()
