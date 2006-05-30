# Makefile for Thrift C++ library.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

target: libthrift

# Tools
LD    = g++
LDFL  = -shared -Wall -I. -fPIC -Wl,-soname=libthrift.so

# Source files
SRCS  = client/TSimpleClient.cc \
	protocol/TBinaryProtocol.cc \
	server/TSimpleServer.cc \
	transport/TSocket.cc \
	transport/TServerSocket.cc

# Linked library
libthrift:
	$(LD) -o libthrift.so $(LDFL) $(SRCS)

clean:
	rm -f libthrift.so

# Install
install: libthrift
	sudo install libthrift.so /usr/local/lib
