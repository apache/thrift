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
LIBS  = ../../lib/cpp/src/server/TSimpleServer.cc \
	../../lib/cpp/src/protocol/TBinaryProtocol.cc \
	../../lib/cpp/src/transport/TBufferedTransport.cc \
	../../lib/cpp/src/transport/TChunkedTransport.cc \
	../../lib/cpp/src/transport/TServerSocket.cc \
	../../lib/cpp/src/transport/TSocket.cc
DCFL  = -Wall -O3 -g -Igen-cpp -I../../lib/cpp/src $(LIBS)
CFL   = -Wall -O3 -Igen-cpp -I../../lib/cpp/src -lthrift

all: server client

debug: server-debug client-debug

stubs: ../ThriftTest.thrift
	$(THRIFT) -cpp ../ThriftTest.thrift

server-debug: stubs
	g++ -o TestServer $(DCFL) src/TestServer.cc gen-cpp/ThriftTest.cc

client-debug: stubs
	g++ -o TestClient $(DCFL) src/TestClient.cc gen-cpp/ThriftTest.cc

server: stubs
	g++ -o TestServer $(CFL) src/TestServer.cc gen-cpp/ThriftTest.cc

client: stubs
	g++ -o TestClient $(CFL) src/TestClient.cc gen-cpp/ThriftTest.cc

clean:
	rm -fr TestServer TestClient gen-cpp
