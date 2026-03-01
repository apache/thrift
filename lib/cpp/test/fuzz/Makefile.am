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

# Generate FuzzTest code
BUILT_SOURCES = gen-cpp/FuzzTest_types.h

noinst_LTLIBRARIES = libfuzztest_gen.la

nodist_libfuzztest_gen_la_SOURCES = \
    gen-cpp/FuzzTest_types.cpp \
    gen-cpp/FuzzTest_types.h 

libfuzztest_gen_la_LIBADD = $(top_builddir)/lib/cpp/libthrift.la

# Common fuzzing headers
noinst_HEADERS = FuzzCommon.tcc

# Fuzzing executables
AM_CPPFLAGS = -I$(top_srcdir)/lib/cpp/src -I$(top_srcdir)/lib/cpp/src/thrift -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I.

check_PROGRAMS = FuzzParseCompact FuzzParseBinary FuzzRoundtripCompact FuzzRoundtripBinary FuzzParseJson FuzzRoundtripJson

FuzzParseCompact_SOURCES = FuzzParseCompact.cpp FuzzCommon.tcc
FuzzParseCompact_LDADD = libfuzztest_gen.la $(top_builddir)/lib/cpp/libthrift.la
FuzzParseCompact_CXXFLAGS = $(AM_CXXFLAGS) $(AM_CPPFLAGS) -g
if USING_CLANG
FuzzParseCompact_LDFLAGS = $(AM_LDFLAGS) -fsanitize=fuzzer
endif

FuzzRoundtripCompact_SOURCES = FuzzRoundtripCompact.cpp FuzzCommon.tcc
FuzzRoundtripCompact_LDADD = libfuzztest_gen.la $(top_builddir)/lib/cpp/libthrift.la
FuzzRoundtripCompact_CXXFLAGS = $(AM_CXXFLAGS) $(AM_CPPFLAGS) -g
if USING_CLANG
FuzzRoundtripCompact_LDFLAGS = $(AM_LDFLAGS) -fsanitize=fuzzer
endif

FuzzParseBinary_SOURCES = FuzzParseBinary.cpp FuzzCommon.tcc
FuzzParseBinary_LDADD = libfuzztest_gen.la $(top_builddir)/lib/cpp/libthrift.la
FuzzParseBinary_CXXFLAGS = $(AM_CXXFLAGS) $(AM_CPPFLAGS) -g
if USING_CLANG
FuzzParseBinary_LDFLAGS = $(AM_LDFLAGS) -fsanitize=fuzzer
endif

FuzzRoundtripBinary_SOURCES = FuzzRoundtripBinary.cpp FuzzCommon.tcc
FuzzRoundtripBinary_LDADD = libfuzztest_gen.la $(top_builddir)/lib/cpp/libthrift.la
FuzzRoundtripBinary_CXXFLAGS = $(AM_CXXFLAGS) $(AM_CPPFLAGS) -g
if USING_CLANG
FuzzRoundtripBinary_LDFLAGS = $(AM_LDFLAGS) -fsanitize=fuzzer
endif

FuzzParseJson_SOURCES = FuzzParseJson.cpp FuzzCommon.tcc
FuzzParseJson_LDADD = libfuzztest_gen.la $(top_builddir)/lib/cpp/libthrift.la
FuzzParseJson_CXXFLAGS = $(AM_CXXFLAGS) $(AM_CPPFLAGS) -g
if USING_CLANG
FuzzParseJson_LDFLAGS = $(AM_LDFLAGS) -fsanitize=fuzzer
endif

FuzzRoundtripJson_SOURCES = FuzzRoundtripJson.cpp FuzzCommon.tcc
FuzzRoundtripJson_LDADD = libfuzztest_gen.la $(top_builddir)/lib/cpp/libthrift.la
FuzzRoundtripJson_CXXFLAGS = $(AM_CXXFLAGS) $(AM_CPPFLAGS) -g
if USING_CLANG
FuzzRoundtripJson_LDFLAGS = $(AM_LDFLAGS) -fsanitize=fuzzer
endif

# Generate thrift files
gen-cpp/FuzzTest_types.cpp gen-cpp/FuzzTest_types.h: $(top_srcdir)/test/FuzzTest.thrift
	$(THRIFT) --gen cpp $<

# Clean target
clean-local:
	$(RM) -rf gen-cpp
	$(RM) -f $(check_PROGRAMS)
	$(RM) -f *.o *.lo *.la
	$(RM) -rf .libs

CLEANFILES = gen-cpp/*

EXTRA_DIST = CMakeLists.txt FuzzParseCompact.cpp FuzzParseBinary.cpp FuzzRoundtripCompact.cpp FuzzRoundtripBinary.cpp FuzzParseJson.cpp FuzzRoundtripJson.cpp 