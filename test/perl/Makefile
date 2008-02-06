# Makefile for Thrift test project.
#
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = ../../compiler/cpp/thrift

all: ../ThriftTest.thrift
	$(THRIFT) -perl ../ThriftTest.thrift

clean:
	rm -fr gen-perl
