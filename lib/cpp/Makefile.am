lib_LTLIBRARIES = libthrift.la
## We only build the extra libraries if we have the dependencies,
## but we install all of the headers unconditionally.
if AMX_HAVE_LIBEVENT
lib_LTLIBRARIES += libthriftnb.la
endif

common_cxxflags = -Wall -Isrc $(BOOST_CPPFLAGS)
common_ldflags = -Wall $(BOOST_LDFLAGS)

# Define the source file for the module

libthrift_la_SOURCES = src/Thrift.cpp \
                       src/reflection_limited_types.cpp \
                       src/concurrency/Mutex.cpp \
                       src/concurrency/Monitor.cpp \
                       src/concurrency/PosixThreadFactory.cpp \
                       src/concurrency/ThreadManager.cpp \
                       src/concurrency/TimerManager.cpp \
                       src/protocol/TBinaryProtocol.cpp \
                       src/protocol/TDebugProtocol.cpp \
                       src/protocol/TDenseProtocol.cpp \
                       src/transport/TTransportException.cpp \
                       src/transport/TFileTransport.cpp \
                       src/transport/THttpClient.cpp \
                       src/transport/TSocket.cpp \
                       src/transport/TSocketPool.cpp \
                       src/transport/TServerSocket.cpp \
                       src/transport/TTransportUtils.cpp \
                       src/server/TSimpleServer.cpp \
                       src/server/TThreadPoolServer.cpp \
                       src/server/TThreadedServer.cpp \
                       src/processor/PeekProcessor.cpp


libthriftnb_la_SOURCES = src/server/TNonblockingServer.cpp

libthrift_la_CXXFLAGS =  $(common_cxxflags)
libthriftnb_la_CXXFLAGS =  $(common_cxxflags) $(LIBEVENT_CPPFLAGS)

include_thriftdir = $(includedir)/thrift
include_thrift_HEADERS = \
                         config.h \
                         src/Thrift.h \
                         src/reflection_limited_types.h \
                         src/TProcessor.h \
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
                         src/concurrency/Util.h 

include_protocoldir = $(include_thriftdir)/protocol
include_protocol_HEADERS = \
                         src/protocol/TBinaryProtocol.h \
                         src/protocol/TDenseProtocol.cpp \
                         src/protocol/TDebugProtocol.h \
                         src/protocol/TOneWayProtocol.h \
                         src/protocol/TProtocolException.h \
                         src/protocol/TProtocol.h

include_transportdir = $(include_thriftdir)/transport
include_transport_HEADERS = \
                         src/transport/TFileTransport.h \
                         src/transport/TServerSocket.h \
                         src/transport/TServerTransport.h \
                         src/transport/THttpClient.h \
                         src/transport/TSocket.h \
                         src/transport/TSocketPool.h \
                         src/transport/TTransport.h \
                         src/transport/TTransportException.h \
                         src/transport/TTransportUtils.h

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

bin_PROGRAMS = concurrency_test

concurrency_test_SOURCES = src/concurrency/test/Tests.cpp \
                           src/concurrency/test/ThreadFactoryTests.h \
                           src/concurrency/test/ThreadManagerTests.h \
                           src/concurrency/test/TimerManagerTests.h

concurrency_test_LDADD = libthrift.la

concurrency_test_CXXFLAGS =  $(common_cxxflags)
concurrency_test_LDFLAGS =  $(common_ldflags)
