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
	gen-cpp/ThriftTest_types.cpp \
	gen-cpp/DebugProtoTest_types.h \
	gen-cpp/OptionalRequiredTest_types.h \
	gen-cpp/ThriftTest_types.h \
	ThriftTest_extras.cpp \
	DebugProtoTest_extras.cpp

ThriftTest_extras.o: gen-cpp/ThriftTest_types.h
DebugProtoTest_extras.o: gen-cpp/DebugProtoTest_types.h

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
# Common thrift code generation rules
#
THRIFT = $(top_builddir)/compiler/cpp/thrift

gen-cpp/DebugProtoTest_types.cpp gen-cpp/DebugProtoTest_types.h: DebugProtoTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/OptionalRequiredTest_types.cpp gen-cpp/OptionalRequiredTest_types.h: OptionalRequiredTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp: StressTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/SecondService.cpp gen-cpp/ThriftTest_constants.cpp gen-cpp/ThriftTest.cpp gen-cpp/ThriftTest_types.cpp gen-cpp/ThriftTest_types.h: ThriftTest.thrift
	$(THRIFT) --gen cpp:dense $<

INCLUDES = \
	-I$(top_srcdir)/lib/cpp/src

AM_CPPFLAGS = $(BOOST_CPPFLAGS)

clean-local:
	$(RM) -r gen-cpp

EXTRA_DIST = \
	cpp \
	threads \
	csharp \
	py \
	rb \
	perl \
	php \
	erl \
	hs \
	ocaml \
	AnnotationTest.thrift \
	BrokenConstants.thrift \
	ConstantsDemo.thrift \
	DebugProtoTest.thrift \
	DenseLinkingTest.thrift \
	DocTest.thrift \
	JavaBeansTest.thrift \
	ManyTypedefs.thrift \
	OptionalRequiredTest.thrift \
	SmallTest.thrift \
	StressTest.thrift \
	ThriftTest.thrift \
	ZlibTest.cpp \
	DenseProtoTest.cpp \
	FastbinaryTest.py
