# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = /usr/local/bin/thrift
CC     = g++
LD     = g++

# Compiler flags
LIBS  = ../../lib/cpp/server/TSimpleServer.cc \
	../../lib/cpp/protocol/TBinaryProtocol.cc \
	../../lib/cpp/transport/TServerSocket.cc \
	../../lib/cpp/transport/TSocket.cc
CFL   = -Wall -g -I../../lib/cpp $(LIBS)
CFL   = -Wall -g -lthrift -I../../lib/cpp

all: server client

stubs: ../ThriftTest.thrift
	$(THRIFT) ../ThriftTest.thrift

server: stubs
	g++ -o TestServer $(CFL) TestServer.cc gen-cpp/ThriftTest.cc

client: stubs
	g++ -o TestClient $(CFL) TestClient.cc gen-cpp/ThriftTest.cc

clean:
	rm -fr TestServer TestClient gen-cpp
