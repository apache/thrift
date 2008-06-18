# Makefile for Thrift test project.
#
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = ../../compiler/cpp/thrift

all: stubs tests

stubs: ../ThriftTest.thrift ../SmallTest.thrift
	$(THRIFT) --gen rb ../ThriftTest.thrift
	$(THRIFT) --gen rb ../SmallTest.thrift

tests: stubs
	ruby TestSuite.rb

clean:
	$(RM) -r gen-rb
