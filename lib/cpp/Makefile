# Makefile for Thrift C++ library. Generates a shared object that can be
# installed to /usr/local/lib
#
# TODO(mcslee): Add the ability to compile separate statis modules that can
# be compiled directly into Thrift applications instead of dynamic runtime
# loading of the full libs
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

target: libthrift

# Tools
LD    = g++
LDFL  = -shared -Wall -I. -fPIC -Wl,-soname=libthrift.so

# Source files
SRCS  = protocol/TBinaryProtocol.cc \
	transport/TBufferedTransport.cc \
	transport/TSocket.cc \
	transport/TServerSocket.cc \
	server/TSimpleServer.cc

# Linked library
libthrift:
	$(LD) -o libthrift.so $(LDFL) $(SRCS)

# Clean it up
clean:
	rm -f libthrift.so

# Install
install: libthrift
	sudo install libthrift.so /usr/local/lib
