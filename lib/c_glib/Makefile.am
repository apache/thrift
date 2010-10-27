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

SUBDIRS = . test

pkgconfigdir = $(libdir)/pkgconfig

lib_LTLIBRARIES = libthrift_c_glib.la
pkgconfig_DATA = thrift_c_glib.pc

common_cflags = -g -Wall -W -Werror -Isrc $(GLIB_CFLAGS)
common_ldflags = -g -Wall -W $(GLIB_LDFLAGS) @GCOV_LDFLAGS@

# this removes optimizations and adds coverage flags
CFLAGS = @GCOV_CFLAGS@

# Define the source files for the module

libthrift_c_glib_la_SOURCES = src/thrift.c \
                              src/thrift_struct.c \
                              src/thrift_application_exception.c \
                              src/processor/thrift_processor.c \
                              src/protocol/thrift_protocol.c \
                              src/protocol/thrift_protocol_factory.c \
                              src/protocol/thrift_binary_protocol.c \
                              src/protocol/thrift_binary_protocol_factory.c \
                              src/transport/thrift_transport.c \
                              src/transport/thrift_transport_factory.c \
                              src/transport/thrift_socket.c \
                              src/transport/thrift_server_transport.c \
                              src/transport/thrift_server_socket.c \
                              src/transport/thrift_buffered_transport.c \
                              src/transport/thrift_framed_transport.c \
                              src/transport/thrift_memory_buffer.c \
                              src/server/thrift_server.c \
                              src/server/thrift_simple_server.c

libthrift_c_glib_la_CFLAGS = $(common_cflags)

include_thriftdir = $(includedir)/thrift
include_thrift_HEADERS = \
                         $(top_builddir)/config.h \
                         src/thrift.h \
                         src/thrift_application_exception.h \
                         src/thrift_struct.h

include_protocoldir = $(include_thriftdir)/protocol
include_protocol_HEADERS = src/protocol/thrift_protocol.h \
                           src/protocol/thrift_protocol_factory.h \
                           src/protocol/thrift_binary_protocol.h \
                           src/protocol/thrift_binary_protocol_factory.h

include_transportdir = $(include_thriftdir)/transport
include_transport_HEADERS = src/transport/thrift_buffered_transport.h \
                            src/transport/thrift_framed_transport.h \
                            src/transport/thrift_memory_buffer.h \
                            src/transport/thrift_server_socket.h \
                            src/transport/thrift_server_transport.h \
                            src/transport/thrift_socket.h \
                            src/transport/thrift_transport.h \
                            src/transport/thrift_transport_factory.h

include_serverdir = $(include_thriftdir)/server
include_server_HEADERS = src/server/thrift_server.h \
                         src/server/thrift_simple_server.h

include_processordir = $(include_thriftdir)/processor
include_processor_HEADERS = src/processor/thrift_processor.h


EXTRA_DIST = \
             README \
             thrift_c_glib.pc.in

CLEANFILES = \
             *.gcno \
             *.gcda
