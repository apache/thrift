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
THRIFT = ../../compiler/cpp/thrift
endif # THRIFT

CC     = g++
LD     = g++

# Compiler flags
DCFL  = -Wall -O3 -g -I./gen-cpp $(include_flags) -L$(thrift_home)/lib/cpp/.libs -lthrift -lthriftnb -levent
LFL   =  -L$(thrift_home)/lib/cpp/.libs -lthrift -lthriftnb -levent
CCFL  = -Wall -O3 -I./gen-cpp $(include_flags)
CFL   = $(CCFL) $(LFL)

all: server client

debug: server-debug client-debug

stubs: ../ThriftTest.thrift
	$(THRIFT) --gen cpp ../ThriftTest.thrift

server-debug: stubs
	g++ -o TestServer $(DCFL) src/TestServer.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp

client-debug: stubs
	g++ -o TestClient $(DCFL) src/TestClient.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp

server: stubs
	g++ -o TestServer $(CFL) src/TestServer.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp

client: stubs
	g++ -o TestClient $(CFL) src/TestClient.cpp ./gen-cpp/ThriftTest.cpp ./gen-cpp/ThriftTest_types.cpp

small:
	$(THRIFT) --gen cpp ../SmallTest.thrift
	g++ -c $(CCFL) ./gen-cpp/SmallService.cpp ./gen-cpp/SmallTest_types.cpp

clean:
	rm -fr *.o TestServer TestClient gen-cpp
