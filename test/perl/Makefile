# Makefile for Thrift test project.
#
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = ../../compiler/cpp/thrift

all: ../ThriftTest.thrift
	$(THRIFT) --gen perl ../ThriftTest.thrift

clean:
	$(RM) -r gen-perl
