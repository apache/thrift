SUBDIRS =

if WITH_PYTHON
SUBDIRS += py
endif

if WITH_RUBY
SUBDIRS += rb
endif

noinst_LTLIBRARIES = libtestgencpp.la
libtestgencpp_la_SOURCES = \
	gen-cpp/DebugProtoTest_types.cpp \
	gen-cpp/OptionalRequiredTest_types.cpp \
	gen-cpp/DebugProtoTest_types.cpp \
	gen-cpp/PartiallyReflectable.cpp \
	gen-cpp/Service.cpp \
	gen-cpp/StressTest_types.cpp \
	gen-cpp/SecondService.cpp \
	gen-cpp/ThriftTest_constants.cpp \
	gen-cpp/ThriftTest.cpp \
	gen-cpp/ThriftTest_types.cpp \
	ThriftTest_extras.cpp

libtestgencpp_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

noinst_PROGRAMS = Benchmark

Benchmark_SOURCES = \
	Benchmark.cpp

Benchmark_LDADD = libtestgencpp.la

check_PROGRAMS = \
	TFDTransportTest \
	TPipedTransportTest \
	DebugProtoTest \
	JSONProtoTest \
	OptionalRequiredTest \
	ReflectionTest \
	UnitTests

TESTS = \
	$(check_PROGRAMS)

UnitTests_SOURCES = \
	UnitTestMain.cpp \
	TMemoryBufferTest.cpp \
	TBufferBaseTest.cpp

UnitTests_LDADD = libtestgencpp.la

#
# TFDTransportTest
#
TFDTransportTest_SOURCES = \
	TFDTransportTest.cpp

TFDTransportTest_LDADD = \
	$(top_builddir)/lib/cpp/libthrift.la


#
# TPipedTransportTest
#
TPipedTransportTest_SOURCES = \
	TPipedTransportTest.cpp

TPipedTransportTest_LDADD = \
	$(top_builddir)/lib/cpp/libthrift.la

#
# DebugProtoTest
#
DebugProtoTest_SOURCES = \
	DebugProtoTest.cpp

DebugProtoTest_LDADD = libtestgencpp.la


#
# JSONProtoTest
#
JSONProtoTest_SOURCES = \
	JSONProtoTest.cpp

JSONProtoTest_LDADD = libtestgencpp.la

#
# OptionalRequiredTest
#
OptionalRequiredTest_SOURCES = \
	OptionalRequiredTest.cpp

OptionalRequiredTest_LDADD = libtestgencpp.la


#
# ReflectionTest
#
ReflectionTest_SOURCES = \
	ReflectionTest.cpp

ReflectionTest_LDADD = libtestgencpp.la

#
# Common thrift code generation rules
#
THRIFT = $(top_builddir)/compiler/cpp/thrift

gen-cpp/DebugProtoTest_types.cpp gen-cpp/PartiallyReflectable.cpp: DebugProtoTest.thrift
	$(THRIFT) --gen cpp:dense,reflection_limited $<

gen-cpp/OptionalRequiredTest_types.cpp: OptionalRequiredTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp: StressTest.thrift
	$(THRIFT) --gen cpp:dense,reflection_limited $<

gen-cpp/SecondService.cpp gen-cpp/ThriftTest_constants.cpp gen-cpp/ThriftTest.cpp gen-cpp/ThriftTest_types.cpp: ThriftTest.thrift
	$(THRIFT) --gen cpp:dense,reflection_limited $<

INCLUDES = \
	-I$(top_srcdir)/lib/cpp/src

AM_CPPFLAGS = $(BOOST_CPPFLAGS)

clean-local:
	$(RM) -r gen-cpp
