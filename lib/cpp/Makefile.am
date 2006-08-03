#lib_LTLIBRARIES = libthrift.la

lib_LIBRARIES = libthrift.a

common_cxxflags = -Isrc $(BOOST_CPPFLAGS)
common_ldflags = $(BOOST_LDFLAGS)

# Define the source file for the module

libthrift_sources = src/concurrency/Monitor.cc \
                    src/concurrency/PosixThreadFactory.cc \
                    src/concurrency/ThreadManager.cc \
                    src/concurrency/TimerManager.cc \
	            src/protocol/TBinaryProtocol.cc \
                    src/transport/TBufferedTransport.cc \
                    src/transport/TChunkedTransport.cc \
                    src/transport/TSocket.cc \
                    src/transport/TServerSocket.cc \
                    src/server/TSimpleServer.cc \
                    src/server/TThreadPoolServer.cc

libthrift_a_SOURCES = $(libthrift_sources)
#libthrift_la_SOURCES = $(libthrift_sources)

libthrift_cxxflags =  $(common_cxxflags)
libthrift_ldflags =  $(common_ldflags)

libthrift_la_CXXFLAGS =  $(libthrift_cxxflags)
libthrift_a_CXXFLAGS =  $(libthrift_cxxflags)

libthrift_inst_headers = src/concurrency/Exception.h \
                         src/concurrency/Monitor.h \
                         src/concurrency/PosixThreadFactory.h \
                         src/concurrency/Thread.h \
                         src/concurrency/ThreadManager.h \
                         src/concurrency/TimerManager.h \
                         src/protocol/TBinaryProtocol.h \
                         src/protocol/TProtocol.h \
                         src/transport/TBufferedTransport.h \
                         src/transport/TChunkedTransport.h \
                         src/transport/TNullTransport.h \
                         src/transport/TServerSocket.h \
                         src/transport/TServerTransport.h \
                         src/transport/TSocket.h \
                         src/transport/TTransport.h \
                         src/transport/TTransport.h \
                         src/transport/TTransportException.h \
                         src/server/TSimpleServer.h \
                         src/server/TThreadPoolServer.h

bin_PROGRAMS = concurrency_test

include_HEADERS = $(libconcurrency_inst_headers) \
                  $(libthrift_inst_headers)

concurrency_test_SOURCES = src/concurrency/test/Tests.cc \
                           src/concurrency/test/ThreadFactoryTests.h \
                           src/concurrency/test/ThreadManagerTests.h \
                           src/concurrency/test/TimerManagerTests.h

concurrency_test_LDADD = libthrift.a

concurrency_test_CXXFLAGS =  $(common_cxxflags)
concurrency_test_LDFLAGS =  $(common_ldflags)
