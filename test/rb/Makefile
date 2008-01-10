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
	$(THRIFT) --rb ../ThriftTest.thrift
	$(THRIFT) --rb ../SmallTest.thrift

tests: stubs
	ruby TestSuite.rb

clean:
	rm -fr gen-rb
