lib_LTLIBRARIES = libconcurrency.la \
                     libthrift.la

# Define the source file for the module

libconcurrency_la_SOURCES = src/concurrency/Monitor.cc \
                            src/concurrency/PosixThreadFactory.cc \
                            src/concurrency/ThreadManager.cc \
                            src/concurrency/TimerManager.cc

libconcurrency_inst_headers = src/concurrency/Exception.h \
                              src/concurrency/Monitor.h \
                              src/concurrency/PosixThreadFactory.h \
                              src/concurrency/Thread.h \
                              src/concurrency/ThreadManager.h \
                              src/concurrency/TimerManager.h

libthrift_la_SOURCES = src/protocol/TBinaryProtocol.cc \
                       src/transport/TBufferedTransport.cc \
                       src/transport/TChunkedTransport.cc \
                       src/transport/TSocket.cc \
                       src/transport/TServerSocket.cc \
                       src/server/TSimpleServer.cc

libthrift_la_CXXFLAGS =  -Isrc

libthrift_inst_headers = src/protocol/TBinaryProtocol.h \
                         src/protocol/TProtocol.h \
                         src/transport/TBufferedTransport.h \
                         src/transport/TChunkedTransport.h \
                         src/transport/TNullTransport.h \
                         src/transport/TServerSocket.h \
                         src/transport/TServerTransport.h \
                         src/transport/TSocket.h \
                         src/transport/TTransport.h \
                         src/transport/TTransport.h \
                         src/transport/TTransportException.h

bin_PROGRAMS = concurrency_test

include_HEADERS = $(libconcurrency_inst_headers) \
                  $(libthrift_inst_headers)

concurrency_test_SOURCES = src/concurrency/test/Tests.cc \
                           src/concurrency/test/ThreadFactoryTests.h \
                           src/concurrency/test/ThreadManagerTests.h \
                           src/concurrency/test/TimerManagerTests.h

concurrency_test_LDADD = libconcurrency.la

concurrency_test_CXXFLAGS =  -Isrc/concurrency
