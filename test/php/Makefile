# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = thrift

# Compiler flags
LIBS  = ../../lib/cpp/server/TSimpleServer.cc \
	../../lib/cpp/protocol/TBinaryProtocol.cc \
	../../lib/cpp/transport/TBufferedTransport.cc \
	../../lib/cpp/transport/TServerSocket.cc \
	../../lib/cpp/transport/TSocket.cc
CFL   = -Wall -O3 -Igen-cpp -I../../lib/cpp $(LIBS)
CFL   = -Wall -O3 -Igen-cpp -I../../lib/cpp -lthrift

all: stubs

stubs: ../ThriftTest.thrift
	$(THRIFT) -php ../ThriftTest.thrift

clean:
	rm -fr gen-php
