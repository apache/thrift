THRIFT = $(top_srcdir)/compiler/cpp/thrift

stubs: ../ThriftTest.thrift ../SmallTest.thrift
	$(THRIFT) --gen rb ../ThriftTest.thrift
	$(THRIFT) --gen rb ../SmallTest.thrift

check: stubs
	$(RUBY) test_suite.rb

