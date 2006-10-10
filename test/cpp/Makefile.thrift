# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything

ifndef thrift_home
thrift_home=../..
endif #thrift_home

target: all

ifndef boost_home
#boost_home=../../../../../thirdparty/boost_1_33_1
boost_home=/usr/local/include/boost-1_33_1
endif #boost_home
target: all

include_paths = $(thrift_home)/lib/cpp/src \
		$(boost_home)

include_flags = $(patsubst %,-I%, $(include_paths))

# Tools
ifndef THRIFT
THRIFT = ../../compiler/cpp/bin/thrift
endif # THRIFT

CC     = g++
LD     = g++

# Compiler flags
DCFL  = -Wall -O3 -g -I./gen-cpp $(include_flags) -L$(thrift_home)/lib/cpp -lthrift -levent
CFL   = -Wall -O3 -I./gen-cpp $(include_flags) -L$(thrift_home)/lib/cpp -lthrift -levent

all: server client

debug: server-debug client-debug

stubs: ../ThriftTest.thrift
	$(THRIFT) --cpp ../ThriftTest.thrift

server-debug: stubs
	g++ -o TestServer $(DCFL) src/TestServer.cc ./gen-cpp/ThriftTest.cc ./gen-cpp/ThriftTest_types.cc 

client-debug: stubs
	g++ -o TestClient $(DCFL) src/TestClient.cc ./gen-cpp/ThriftTest.cc ./gen-cpp/ThriftTest_types.cc

server: stubs
	g++ -o TestServer $(CFL) src/TestServer.cc ./gen-cpp/ThriftTest.cc ./gen-cpp/ThriftTest_types.cc

client: stubs
	g++ -o TestClient $(CFL) src/TestClient.cc ./gen-cpp/ThriftTest.cc ./gen-cpp/ThriftTest_types.cc

clean:
	rm -fr TestServer TestClient gen-cpp
