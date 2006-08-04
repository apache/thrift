# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything

ifndef thrift_home
thrift_home=../../build
endif #thrift_home
target: all

# Tools
THRIFT = thrift
CC     = g++
LD     = g++

include_flags = $(patsubst %,-I$(thrift_home)/include/%, thrift boost-1_33_1)

# Compiler flags
LIBS  = ../../lib/cpp/src/server/TSimpleServer.cc \
	../../lib/cpp/src/protocol/TBinaryProtocol.cc \
	../../lib/cpp/src/transport/TBufferedTransport.cc \
	../../lib/cpp/src/transport/TChunkedTransport.cc \
	../../lib/cpp/src/transport/TServerSocket.cc \
	../../lib/cpp/src/transport/TSocket.cc
DCFL  = -Wall -O3 -g -Igen-cpp $(include_flags) $(LIBS)
CFL   = -Wall -O3 -Igen-cpp $(include_flags) -L$(thrift_home)/lib -lthrift

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
