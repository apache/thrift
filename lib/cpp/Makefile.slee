# Makefile for Thrift C++ library. Generates a shared object that can be
# installed to /usr/local/lib
#
# TODO(mcslee): Add the ability to compile separate statis modules that can
# be compiled directly into Thrift applications instead of dynamic runtime
# loading of the full libs
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

target: libthrift.so libconcurrency.so

# Tools
LD    = g++
CPP   = g++

CC_COMMON_FLAGS  = -g -c -Wall -Isrc -fPIC -fno-common

LD_COMMON_FLAGS=

LD_APP_FLAGS= $(LD_COMMON_FLAGS)

LD_LIB_FLAGS= -dynamiclib $(LD_COMMON_FLAGS)

# Source files
SRCS  = src/protocol/TBinaryProtocol.cc \
	src/transport/TBufferedTransport.cc \
	src/transport/TChunkedTransport.cc \
	src/transport/TSocket.cc \
	src/transport/TServerSocket.cc \
	src/server/TSimpleServer.cc

# Concurreny Utility Source files
CONCURRENCY_SRCS  = src/concurrency/Monitor.cc \
	            src/concurrency/Mutex.cc \
	            src/concurrency/PosixThreadFactory.cc \
	            src/concurrency/ThreadManager.cc \
	            src/concurrency/TimerManager.cc

CONCURRENCY_OBJS  = $(patsubst %.cc,%.o,$(CONCURRENCY_SRCS))

$(CONCURRENCY_OBJS): %.o : %.cc
	$(CC) $(CC_COMMON_FLAGS) $< -o $@

CONCURRENCY_TEST_SRCS  = src/concurrency/test/TimerManagerTests.cc

CONCURRENCY_TEST_OBJS  = $(patsubst %.cc,%.o,$(CONCURRENCY_TEST_SRCS))

$(CONCURRENCY_TEST_OBJS): %.o : %.cc
	$(CC) $(CC_COMMON_FLAGS) -I src/concurrency $< -o $@

# Linked libraries

# thrift library

THRIFT_OBJS  = $(patsubst %.cc,%.o, $(SRCS))

$(THRIFT_OBJS): %.o : %.cc
	$(CC) $(CC_COMMON_FLAGS) $< -o $@

libthrift.so: $(THRIFT_OBJS)
	 $(LD) -o $@ $(LD_LIB_FLAGS) $(THRIFT_OBJS)

# concurrency util library

libconcurrency.so: $(CONCURRENCY_OBJS)
	$(LD) -o $@ $(LD_LIB_FLAGS) $(CONCURRENCY_OBJS)

concurrency_tests: libconcurrency.so $(CONCURRENCY_TEST_OBJS)
	$(LD) -o $@ $(LD_APP_FLAGS) -L. $(CONCURRENCY_TEST_OBJS)  libconcurrency.so

tests: concurrency_tests

clean_libthrift:
	rm -f libthrift.so
	rm -f $(THRIFT_OBJS)

clean_libconcurrency:
	rm -f libconcurrency.so
	rm -f $(CONCURRENCY_OBJS)

clean_tests:
	rm -f concurrency_tests
	rm -f $(CONCURRENTY_TEST_OBJS)

# Clean it up
clean: clean_libthrift clean_libconcurrency clean_tests

# Install
install: libthrift
	sudo install libthrift.so /usr/local/lib
