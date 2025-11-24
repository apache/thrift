#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

AUTOMAKE_OPTIONS = subdir-objects serial-tests nostdinc

BUILT_SOURCES = \
    gen-c_glib/fuzz_test_no_uuid_types.h \
    gen-c_glib/fuzz_test_no_uuid_types.c

noinst_LTLIBRARIES = libtestgencfuzz.la
nodist_libtestgencfuzz_la_SOURCES = \
    gen-c_glib/fuzz_test_no_uuid_types.c \
    gen-c_glib/fuzz_test_no_uuid_types.h

libtestgencfuzz_la_LIBADD = $(top_builddir)/lib/c_glib/libthrift_c_glib.la

AM_CPPFLAGS = -I$(top_srcdir)/lib/c_glib/src -I../gen-c_glib -I./gen-c_glib -I$(top_builddir)
AM_CFLAGS = -g $(GLIB_CFLAGS) $(GOBJECT_CFLAGS) -I$(top_srcdir)/lib/c_glib/src -I../gen-c_glib -I./gen-c_glib -I$(top_builddir)
AM_LDFLAGS = $(GLIB_LIBS) $(GOBJECT_LIBS)

if USING_CLANG
AM_LDFLAGS += -fsanitize=fuzzer
endif

check_PROGRAMS = fuzz_parse_compact fuzz_parse_binary

fuzz_parse_compact_SOURCES = fuzz_parse_compact.c
fuzz_parse_compact_LDADD = \
    libtestgencfuzz.la \
    $(top_builddir)/lib/c_glib/src/thrift/c_glib/protocol/libthrift_c_glib_la-thrift_protocol.o \
    $(top_builddir)/lib/c_glib/src/thrift/c_glib/transport/libthrift_c_glib_la-thrift_transport.o \
    $(top_builddir)/lib/c_glib/src/thrift/c_glib/libthrift_c_glib_la-thrift_configuration.o

fuzz_parse_binary_SOURCES = fuzz_parse_binary.c
fuzz_parse_binary_LDADD = \
    libtestgencfuzz.la \
    $(top_builddir)/lib/c_glib/src/thrift/c_glib/protocol/libthrift_c_glib_la-thrift_protocol.o \
    $(top_builddir)/lib/c_glib/src/thrift/c_glib/transport/libthrift_c_glib_la-thrift_transport.o \
    $(top_builddir)/lib/c_glib/src/thrift/c_glib/libthrift_c_glib_la-thrift_configuration.o

#
# Common thrift code generation rules
#
gen-c_glib/fuzz_test_no_uuid_types.c gen-c_glib/fuzz_test_no_uuid_types.h: $(top_srcdir)/test/v0.16/FuzzTestNoUuid.thrift
	$(THRIFT) --gen c_glib -r $<

clean-local:
	$(RM) -r gen-c_glib/
	$(RM) *.o
	$(RM) libtestgencfuzz.la 