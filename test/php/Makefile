# Makefile for Thrift test project.
# 
# Author:
#   Mark Slee <mcslee@facebook.com>

# Default target is everything
target: all

# Tools
THRIFT = thrift

all: normal inline

normal: stubs

inline: stubs-inline

stubs: ../ThriftTest.thrift
	$(THRIFT) --php ../ThriftTest.thrift

stubs-inline: ../ThriftTest.thrift
	$(THRIFT) --phpi ../ThriftTest.thrift

clean:
	rm -fr gen-php gen-phpi
