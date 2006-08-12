# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything

ifndef thrift_home
thrift_home=../../build
endif #thrift_home

target: all

ifndef boost_home
boost_home=../../../../../thirdparty/boost_1_33_1
endif #thrift_home
target: all

include_paths = $(thrift_home)/include/thrift \
		$(boost_home)

include_flags = $(patsubst %,-I%, $(include_paths))

# Tools
THRIFT = python ../../compiler/src/thrift.py ~/ws/thrift/dev/test/ThriftTest.thrift  --cpp
CC     = g++
LD     = g++

# Compiler flags
LIBS  = ../../lib/cpp/src/server/TSimpleServer.cc \
	../../lib/cpp/src/protocol/TBinaryProtocol.cc \
	../../lib/cpp/src/transport/TBufferedTransport.cc \
	../../lib/cpp/src/transport/TChunkedTransport.cc \
	../../lib/cpp/src/transport/TServerSocket.cc \
	../../lib/cpp/src/transport/TSocket.cc
DCFL  = -Wall -O3 -g -I../cpp-gen $(include_flags) $(LIBS)
CFL   = -Wall -O3 -I../cpp-gen $(include_flags) -L$(thrift_home)/lib -lthrift

all: server client

debug: server-debug client-debug

stubs: ../ThriftTest.thrift
	$(THRIFT) -cpp ../ThriftTest.thrift

server-debug: stubs
	g++ -o TestServer $(DCFL) src/TestServer.cc ../cpp-gen/ThriftTest.cc

client-debug: stubs
	g++ -o TestClient $(DCFL) src/TestClient.cc ../cpp-gen/ThriftTest.cc

server: stubs
	g++ -o TestServer $(CFL) src/TestServer.cc ../cpp-gen/ThriftTest.cc

client: stubs
	g++ -o TestClient $(CFL) src/TestClient.cc ../cpp-gen/ThriftTest.cc

clean:
	rm -fr TestServer TestClient ../cpp-gen
