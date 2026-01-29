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
AUTOMAKE_OPTIONS = subdir-objects serial-tests nostdinc

BUILT_SOURCES = gen-cpp/ThriftTest.cpp \
                gen-cpp/ThriftTest_types.cpp \
                gen-cpp/ThriftTest_constants.cpp \
                gen-cpp/SecondService.cpp \
                gen-cpp/Service.cpp \
                gen-cpp-forward/ThriftTest_types.cpp \
                gen-cpp-forward/ThriftTest_constants.cpp \
                gen-cpp-private/ThriftTest_types.cpp \
                gen-cpp-private/ThriftTest_constants.cpp \
                gen-cpp-enumclass/ThriftTest_types.cpp \
                gen-cpp-enumclass/ThriftTest_constants.cpp

noinst_LTLIBRARIES = libtestgencpp.la libstresstestgencpp.la
nodist_libtestgencpp_la_SOURCES = \
	gen-cpp/SecondService.cpp \
	gen-cpp/SecondService.h \
	gen-cpp/SecondService.tcc \
	gen-cpp/ThriftTest_constants.cpp \
	gen-cpp/ThriftTest_constants.h \
	gen-cpp/ThriftTest_types.cpp \
	gen-cpp/ThriftTest_types.h \
	gen-cpp/ThriftTest_types.tcc \
	gen-cpp/ThriftTest.cpp \
	gen-cpp/ThriftTest.h \
	gen-cpp/ThriftTest.tcc \
	src/ThriftTest_extras.cpp

libtestgencpp_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

# Libraries for option-specific tests
noinst_LTLIBRARIES += \
	libforwardsettertestgencpp.la \
	libprivateoptonaltestgencpp.la \
	libenumclasstestgencpp.la

nodist_libforwardsettertestgencpp_la_SOURCES = \
	gen-cpp-forward/ThriftTest_types.cpp \
	gen-cpp-forward/ThriftTest_types.h \
	gen-cpp-forward/ThriftTest_types.tcc \
	gen-cpp-forward/ThriftTest_constants.cpp \
	gen-cpp-forward/ThriftTest_constants.h \
	src/ThriftTest_extras.cpp

libforwardsettertestgencpp_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

nodist_libprivateoptonaltestgencpp_la_SOURCES = \
	gen-cpp-private/ThriftTest_types.cpp \
	gen-cpp-private/ThriftTest_types.h \
	gen-cpp-private/ThriftTest_constants.cpp \
	gen-cpp-private/ThriftTest_constants.h \
	src/ThriftTest_extras.cpp

libprivateoptonaltestgencpp_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

nodist_libenumclasstestgencpp_la_SOURCES = \
	gen-cpp-enumclass/ThriftTest_types.cpp \
	gen-cpp-enumclass/ThriftTest_types.h \
	gen-cpp-enumclass/ThriftTest_constants.cpp \
	gen-cpp-enumclass/ThriftTest_constants.h \
	src/ThriftTest_extras.cpp

libenumclasstestgencpp_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

nodist_libstresstestgencpp_la_SOURCES = \
	gen-cpp/StressTest_types.h \
	gen-cpp/Service.cpp \
	gen-cpp/Service.h

libstresstestgencpp_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

precross: TestServer TestClient

check_PROGRAMS = \
	TestServer \
	TestClient \
	StressTest \
	StressTestNonBlocking \
	ForwardSetterTest \
	PrivateOptionalTest \
	EnumClassTest

# we currently do not run the testsuite, stop c++ server issue
# TESTS = \
#	$(check_PROGRAMS)

TestServer_SOURCES = \
	src/TestServer.cpp

TestServer_LDADD = \
	libtestgencpp.la \
	$(top_builddir)/lib/cpp/libthrift.la \
	$(top_builddir)/lib/cpp/libthriftz.la \
	$(top_builddir)/lib/cpp/libthriftnb.la \
	-levent -lboost_program_options -lboost_system -lboost_filesystem $(ZLIB_LIBS)

TestClient_SOURCES = \
	src/TestClient.cpp

TestClient_LDADD = \
	libtestgencpp.la \
	$(top_builddir)/lib/cpp/libthrift.la \
	$(top_builddir)/lib/cpp/libthriftz.la \
	$(top_builddir)/lib/cpp/libthriftnb.la \
	-levent -lboost_program_options -lboost_system -lboost_filesystem $(ZLIB_LIBS)

StressTest_SOURCES = \
	src/StressTest.cpp

StressTest_LDADD = \
	libstresstestgencpp.la \
	$(top_builddir)/lib/cpp/libthrift.la

StressTestNonBlocking_SOURCES = \
	src/StressTestNonBlocking.cpp

StressTestNonBlocking_LDADD = \
	libstresstestgencpp.la \
	$(top_builddir)/lib/cpp/libthriftnb.la \
	-levent

ForwardSetterTest_SOURCES = \
	src/ForwardSetterTest.cpp

ForwardSetterTest_CPPFLAGS = -Igen-cpp-forward $(AM_CPPFLAGS)
ForwardSetterTest_LDADD = \
	libforwardsettertestgencpp.la \
	$(top_builddir)/lib/cpp/libthrift.la

PrivateOptionalTest_SOURCES = \
	src/PrivateOptionalTest.cpp

PrivateOptionalTest_CPPFLAGS = -Igen-cpp-private $(AM_CPPFLAGS)
PrivateOptionalTest_LDADD = \
	libprivateoptonaltestgencpp.la \
	$(top_builddir)/lib/cpp/libthrift.la

EnumClassTest_SOURCES = \
	src/EnumClassTest.cpp

EnumClassTest_CPPFLAGS = -Igen-cpp-enumclass $(AM_CPPFLAGS)
EnumClassTest_LDADD = \
	libenumclasstestgencpp.la \
	$(top_builddir)/lib/cpp/libthrift.la

#
# Common thrift code generation rules
#
gen-cpp/ThriftTest.cpp gen-cpp/ThriftTest_types.cpp gen-cpp/ThriftTest_constants.cpp gen-cpp/SecondService.cpp gen-cpp/SecondService.h gen-cpp/SecondService.tcc: $(top_srcdir)/test/ThriftTest.thrift $(THRIFT)
	$(THRIFT) --gen cpp:templates,cob_style -r $<

# Generate ThriftTest with forward_setter option
gen-cpp-forward/ThriftTest_types.cpp gen-cpp-forward/ThriftTest_types.h gen-cpp-forward/ThriftTest_types.tcc gen-cpp-forward/ThriftTest_constants.cpp: $(top_srcdir)/test/ThriftTest.thrift $(THRIFT)
	$(MKDIR_P) gen-cpp-forward
	$(THRIFT) --gen cpp:moveable_types=forward_setter -out gen-cpp-forward $<

# Generate ThriftTest with private_optional option
gen-cpp-private/ThriftTest_types.cpp gen-cpp-private/ThriftTest_types.h gen-cpp-private/ThriftTest_constants.cpp: $(top_srcdir)/test/ThriftTest.thrift $(THRIFT)
	$(MKDIR_P) gen-cpp-private
	$(THRIFT) --gen cpp:private_optional -out gen-cpp-private $<

# Generate ThriftTest with enum_class option
gen-cpp-enumclass/ThriftTest_types.cpp gen-cpp-enumclass/ThriftTest_types.h gen-cpp-enumclass/ThriftTest_constants.cpp: $(top_srcdir)/test/ThriftTest.thrift $(THRIFT)
	$(MKDIR_P) gen-cpp-enumclass
	$(THRIFT) --gen cpp:pure_enums=enum_class -out gen-cpp-enumclass $<

gen-cpp/Service.cpp: $(top_srcdir)/test/StressTest.thrift $(THRIFT)
	$(THRIFT) --gen cpp $<

gen-cpp/SpecificNameTest_types.cpp gen-cpp/EchoService.cpp: $(top_srcdir)/test/SpecificName.thrift $(THRIFT)
	$(THRIFT) --gen cpp $<

AM_CPPFLAGS = $(BOOST_CPPFLAGS) $(LIBEVENT_CPPFLAGS) -I$(top_srcdir)/lib/cpp/src -Igen-cpp -I.
AM_CXXFLAGS = -Wall -Wextra -pedantic -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS
AM_LDFLAGS = $(BOOST_LDFLAGS) $(LIBEVENT_LDFLAGS) $(ZLIB_LIBS)

clean-local:
	$(RM) -r gen-cpp/ gen-cpp-forward/ gen-cpp-private/ gen-cpp-enumclass/

style-local:
	$(CPPSTYLE_CMD)

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	src/TestClient.cpp \
	src/TestServer.cpp \
	src/StressTest.cpp \
	src/StressTestNonBlocking.cpp \
	src/ForwardSetterTest.cpp \
	src/PrivateOptionalTest.cpp \
	src/EnumClassTest.cpp
