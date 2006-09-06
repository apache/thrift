# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = ../../compiler/cpp/bin/thrift

all: stubs

stubs: ../ThriftTest.thrift
	$(THRIFT) --py ../ThriftTest.thrift

clean:
	rm -fr gen-py
