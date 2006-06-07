# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = thrift
CC     = g++
LD     = g++

# Compiler flags
LIBS  = ../../lib/cpp/server/TSimpleServer.cc \
	../../lib/cpp/protocol/TBinaryProtocol.cc \
	../../lib/cpp/transport/TBufferedTransport.cc \
	../../lib/cpp/transport/TServerSocket.cc \
	../../lib/cpp/transport/TSocket.cc
CFL   = -Wall -O3 -Igen-cpp -I../../lib/cpp $(LIBS)
CFL   = -Wall -O3 -Igen-cpp -I../../lib/cpp -lthrift

all: server client

stubs: ../ThriftTest.thrift
	$(THRIFT) -cpp ../ThriftTest.thrift

server: stubs
	g++ -o TestServer $(CFL) src/TestServer.cc gen-cpp/ThriftTest.cc

client: stubs
	g++ -o TestClient $(CFL) src/TestClient.cc gen-cpp/ThriftTest.cc

clean:
	rm -fr TestServer TestClient gen-cpp
