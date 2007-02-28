#!/usr/bin/env python

import sys
sys.path.append('./gen-py')

from ThriftTest import ThriftTest
from ThriftTest.ttypes import *
from thrift.transport import TTransport
from thrift.transport import TSocket
from thrift.protocol import TBinaryProtocol

import time

import hotshot
from hotshot import stats
prof = None

# Uncomment if you want to profile this biznizzy
#prof = hotshot.Profile('hotshot_thrift_stats')
#prof.start()

host = 'localhost'
port = 9090
framed = False
framedInput = True
argi = 1

# Parse args
while argi < len(sys.argv):
  if sys.argv[argi] == '-h':
    parts = sys.argv[argi+1].split(':') 
    host = parts[0]
    port = int(parts[1])
    argi += 1
  elif sys.argv[argi] == '-f' or sys.argv[argi] == '-framed':
    framed = True
  elif sys.argv[argi] == '-fo':
    framed = True
    framedInput = False
  argi += 1

# Make socket
socket = TSocket.TSocket(host, port)

# Frame or buffer depending upon args
if framed:
  transport = TTransport.TFramedTransport(socket, framedInput, True)
else:
  transport = TTransport.TBufferedTransport(socket)

protocol = TBinaryProtocol.TBinaryProtocol(transport)
client = ThriftTest.Client(protocol)

# Connect!
transport.open()

# Start debug timing
tstart = time.time()

try:
  print "testVoid()"
  print client.testVoid()
except TApplicationException, x:
  print x.message
  print x.type
  
print "testString('Python')"
print client.testString('Python')

print "testByte(63)"
print client.testByte(63)

print "testI32(-1)"
print client.testI32(-1)

print "testI32(0)"
print client.testI32(0)

print "testI64(-34359738368)"
print client.testI64(-34359738368)

print "testDouble(-5.235098235)"
print client.testDouble(-5.235098235)

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

tend = time.time()
ttotal = (tend-tstart)*1000
print "Total time: %f ms" % (ttotal)

# Close!
transport.close()

# Profiler output
if prof != None:
  prof.stop()
  prof.close()
  s = stats.load('hotshot_thrift_stats')
  s.sort_stats('time').print_stats()
