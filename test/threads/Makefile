# Makefile for Thrift test project.
#
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything

ifndef thrift_home
thrift_home=../../
endif #thrift_home

target: all

ifndef boost_home
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
LFL   =  -L$(thrift_home)/lib/cpp/.libs -lthrift
CCFL  = -Wall -O3 -g -I./gen-cpp $(include_flags)
CFL   = $(CCFL) $(LFL)

all: server client

stubs: ThreadsTest.thrift
	$(THRIFT) --gen cpp --gen py ThreadsTest.thrift

server: stubs
	g++ -o ThreadsServer $(CFL) ThreadsServer.cpp ./gen-cpp/ThreadsTest.cpp ./gen-cpp/ThreadsTest_types.cpp

client: stubs
	g++ -o ThreadsClient $(CFL) ThreadsClient.cpp ./gen-cpp/ThreadsTest.cpp ./gen-cpp/ThreadsTest_types.cpp

clean:
	$(RM) -r *.o ThreadsServer ThreadsClient gen-cpp gen-py
