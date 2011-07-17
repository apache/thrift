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

lib_LTLIBRARIES = libthrift.la
pkgconfig_DATA = thrift.pc

## We only build the extra libraries if we have the dependencies,
## but we install all of the headers unconditionally.
if AMX_HAVE_LIBEVENT
lib_LTLIBRARIES += libthriftnb.la
pkgconfig_DATA += thrift-nb.pc
endif
if AMX_HAVE_ZLIB
lib_LTLIBRARIES += libthriftz.la
pkgconfig_DATA += thrift-z.pc
endif

AM_CXXFLAGS = -Wall
AM_CPPFLAGS = $(BOOST_CPPFLAGS) -I$(srcdir)/src

# Define the source files for the module

libthrift_la_SOURCES = src/Thrift.cpp \
                       src/TApplicationException.cpp \
                       src/concurrency/Mutex.cpp \
                       src/concurrency/Monitor.cpp \
                       src/concurrency/PosixThreadFactory.cpp \
                       src/concurrency/ThreadManager.cpp \
                       src/concurrency/TimerManager.cpp \
                       src/concurrency/Util.cpp \
                       src/protocol/TDebugProtocol.cpp \
                       src/protocol/TDenseProtocol.cpp \
                       src/protocol/TJSONProtocol.cpp \
                       src/protocol/TBase64Utils.cpp \
                       src/transport/TTransportException.cpp \
                       src/transport/TFDTransport.cpp \
                       src/transport/TFileTransport.cpp \
                       src/transport/TSimpleFileTransport.cpp \
                       src/transport/THttpTransport.cpp \
                       src/transport/THttpClient.cpp \
                       src/transport/THttpServer.cpp \
                       src/transport/TSocket.cpp \
                       src/transport/TSSLSocket.cpp \
                       src/transport/TSocketPool.cpp \
                       src/transport/TServerSocket.cpp \
                       src/transport/TSSLServerSocket.cpp \
                       src/transport/TTransportUtils.cpp \
                       src/transport/TBufferTransports.cpp \
                       src/server/TServer.cpp \
                       src/server/TSimpleServer.cpp \
                       src/server/TThreadPoolServer.cpp \
                       src/server/TThreadedServer.cpp \
                       src/async/TAsyncChannel.cpp \
                       src/processor/PeekProcessor.cpp

libthriftnb_la_SOURCES = src/server/TNonblockingServer.cpp \
                         src/async/TAsyncProtocolProcessor.cpp \
                         src/async/TEvhttpServer.cpp \
                         src/async/TEvhttpClientChannel.cpp 

libthriftz_la_SOURCES = src/transport/TZlibTransport.cpp


# Flags for the various libraries
libthriftnb_la_CPPFLAGS = $(AM_CPPFLAGS) $(LIBEVENT_CPPFLAGS)
libthriftz_la_CPPFLAGS  = $(AM_CPPFLAGS) $(ZLIB_CPPFLAGS)
libthriftnb_la_CXXFLAGS = $(AM_CXXFLAGS)
libthriftz_la_CXXFLAGS  = $(AM_CXXFLAGS)

include_thriftdir = $(includedir)/thrift
include_thrift_HEADERS = \
                         $(top_builddir)/config.h \
                         src/Thrift.h \
                         src/TReflectionLocal.h \
                         src/TProcessor.h \
                         src/TApplicationException.h \
                         src/TLogging.h

include_concurrencydir = $(include_thriftdir)/concurrency
include_concurrency_HEADERS = \
                         src/concurrency/Exception.h \
                         src/concurrency/Mutex.h \
                         src/concurrency/Monitor.h \
                         src/concurrency/PosixThreadFactory.h \
                         src/concurrency/Thread.h \
                         src/concurrency/ThreadManager.h \
                         src/concurrency/TimerManager.h \
                         src/concurrency/FunctionRunner.h \
                         src/concurrency/Util.h

include_protocoldir = $(include_thriftdir)/protocol
include_protocol_HEADERS = \
                         src/protocol/TBinaryProtocol.h \
                         src/protocol/TBinaryProtocol.tcc \
                         src/protocol/TCompactProtocol.h \
                         src/protocol/TCompactProtocol.tcc \
                         src/protocol/TDenseProtocol.h \
                         src/protocol/TDebugProtocol.h \
                         src/protocol/TBase64Utils.h \
                         src/protocol/TJSONProtocol.h \
                         src/protocol/TProtocolTap.h \
                         src/protocol/TProtocolException.h \
                         src/protocol/TVirtualProtocol.h \
                         src/protocol/TProtocol.h

include_transportdir = $(include_thriftdir)/transport
include_transport_HEADERS = \
                         src/transport/TFDTransport.h \
                         src/transport/TFileTransport.h \
                         src/transport/TSimpleFileTransport.h \
                         src/transport/TServerSocket.h \
                         src/transport/TSSLServerSocket.h \
                         src/transport/TServerTransport.h \
                         src/transport/THttpTransport.h \
                         src/transport/THttpClient.h \
                         src/transport/THttpServer.h \
                         src/transport/TSocket.h \
                         src/transport/TSSLSocket.h \
                         src/transport/TSocketPool.h \
                         src/transport/TVirtualTransport.h \
                         src/transport/TTransport.h \
                         src/transport/TTransportException.h \
                         src/transport/TTransportUtils.h \
                         src/transport/TBufferTransports.h \
                         src/transport/TShortReadTransport.h \
                         src/transport/TZlibTransport.h

include_serverdir = $(include_thriftdir)/server
include_server_HEADERS = \
                         src/server/TServer.h \
                         src/server/TSimpleServer.h \
                         src/server/TThreadPoolServer.h \
                         src/server/TThreadedServer.h \
                         src/server/TNonblockingServer.h

include_processordir = $(include_thriftdir)/processor
include_processor_HEADERS = \
                         src/processor/PeekProcessor.h \
                         src/processor/StatsProcessor.h

include_asyncdir = $(include_thriftdir)/async
include_async_HEADERS = \
                     src/async/TAsyncChannel.h \
                     src/async/TAsyncProcessor.h \
                     src/async/TAsyncBufferProcessor.h \
                     src/async/TAsyncProtocolProcessor.h \
                     src/async/TEvhttpClientChannel.h \
                     src/async/TEvhttpServer.h


noinst_PROGRAMS = concurrency_test

concurrency_test_SOURCES = src/concurrency/test/Tests.cpp \
                           src/concurrency/test/ThreadFactoryTests.h \
                           src/concurrency/test/ThreadManagerTests.h \
                           src/concurrency/test/TimerManagerTests.h

concurrency_test_LDADD = libthrift.la

EXTRA_DIST = \
             README \
             thrift-nb.pc.in \
             thrift.pc.in \
             thrift-z.pc.in
