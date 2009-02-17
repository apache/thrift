# Makefile for Thrift test project.
#
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = ../../compiler/cpp/thrift

all: normal inline

normal: stubs

inline: stubs-inline

stubs: ../ThriftTest.thrift
	$(THRIFT) --gen php ../ThriftTest.thrift

stubs-inline: ../ThriftTest.thrift
	$(THRIFT) --gen php:inlined ../ThriftTest.thrift

clean:
	$(RM) -r gen-php gen-phpi
