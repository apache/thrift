THRIFT = @top_builddir@/compiler/cpp/thrift
THRIFT_IF = @top_srcdir@/test/ThriftTest.thrift

all-local: gen-perl/ThriftTest/Types.pm

gen-perl/ThriftTest/Types.pm: $(THRIFT_IF)
	$(THRIFT) --gen perl $(THRIFT_IF)

clean-local:
	rm -rf gen-perl

EXTRA_DIST = memory_buffer.t
