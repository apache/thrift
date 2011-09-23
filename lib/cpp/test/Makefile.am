#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

noinst_LTLIBRARIES = libtestgencpp.la
nodist_libtestgencpp_la_SOURCES = \
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
	SpecializationTest \
	AllProtocolsTest \
	TransportTest \
	ZlibTest \
	TFileTransportTest \
	UnitTests

TESTS_ENVIRONMENT= \
	BOOST_TEST_LOG_SINK=tests.xml \
	BOOST_TEST_LOG_LEVEL=test_suite \
	BOOST_TEST_LOG_FORMAT=xml

TESTS = \
	$(check_PROGRAMS)

UnitTests_SOURCES = \
	UnitTestMain.cpp \
	TMemoryBufferTest.cpp \
	TBufferBaseTest.cpp

UnitTests_LDADD = \
  $(BOOST_LDFLAGS) \
  libtestgencpp.la \
  $(BOOST_ROOT_PATH)/lib/libboost_unit_test_framework.a

TransportTest_SOURCES = \
	TransportTest.cpp

TransportTest_LDADD = \
  libtestgencpp.la \
  $(top_builddir)/lib/cpp/libthriftz.la \
  $(BOOST_ROOT_PATH)/lib/libboost_unit_test_framework.a \
  -lz

ZlibTest_SOURCES = \
	ZlibTest.cpp

ZlibTest_LDADD = \
  libtestgencpp.la \
  $(top_builddir)/lib/cpp/libthriftz.la \
  $(BOOST_ROOT_PATH)/lib/libboost_unit_test_framework.a \
  -lz

TFileTransportTest_SOURCES = \
	TFileTransportTest.cpp

TFileTransportTest_LDADD = \
  libtestgencpp.la \
  $(BOOST_ROOT_PATH)/lib/libboost_unit_test_framework.a

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
# AllProtocolsTest
#
AllProtocolsTest_SOURCES = \
	AllProtocolTests.cpp \
	AllProtocolTests.tcc \
	GenericHelpers.h

AllProtocolsTest_LDADD = libtestgencpp.la

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
# SpecializationTest
#
SpecializationTest_SOURCES = \
	SpecializationTest.cpp

SpecializationTest_LDADD = libtestgencpp.la


#
# Common thrift code generation rules
#
THRIFT = $(top_builddir)/compiler/cpp/thrift

gen-cpp/DebugProtoTest_types.cpp gen-cpp/DebugProtoTest_types.h: $(top_srcdir)/test/DebugProtoTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/OptionalRequiredTest_types.cpp gen-cpp/OptionalRequiredTest_types.h: $(top_srcdir)/test/OptionalRequiredTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/Service.cpp gen-cpp/StressTest_types.cpp: $(top_srcdir)/test/StressTest.thrift
	$(THRIFT) --gen cpp:dense $<

gen-cpp/SecondService.cpp gen-cpp/ThriftTest_constants.cpp gen-cpp/ThriftTest.cpp gen-cpp/ThriftTest_types.cpp gen-cpp/ThriftTest_types.h: $(top_srcdir)/test/ThriftTest.thrift
	$(THRIFT) --gen cpp:dense $<

INCLUDES = \
	-I$(top_srcdir)/lib/cpp/src

AM_CPPFLAGS = $(BOOST_CPPFLAGS)
AM_CXXFLAGS = -Wall

clean-local:
	$(RM) -r gen-cpp

EXTRA_DIST = \
	DenseProtoTest.cpp \
	ThriftTest_extras.cpp \
	DebugProtoTest_extras.cpp
