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

# Remove the following once lib/cpp no longer depends on boost headers:
include(BoostMacros)
REQUIRE_BOOST_HEADERS()

include_directories(src)

if(NOT BUILD_SHARED_LIBS)
  add_definitions("-DTHRIFT_STATIC_DEFINE")
endif()

# SYSLIBS contains libraries that need to be linked to all lib targets
set(SYSLIBS "")

# Create the thrift C++ library
set( thriftcpp_SOURCES
   src/thrift/TApplicationException.cpp
   src/thrift/TOutput.cpp
   src/thrift/async/TAsyncChannel.cpp
   src/thrift/async/TAsyncProtocolProcessor.cpp
   src/thrift/async/TConcurrentClientSyncInfo.h
   src/thrift/async/TConcurrentClientSyncInfo.cpp
   src/thrift/concurrency/ThreadManager.cpp
   src/thrift/concurrency/TimerManager.cpp
   src/thrift/processor/PeekProcessor.cpp
   src/thrift/protocol/TBase64Utils.cpp
   src/thrift/protocol/TDebugProtocol.cpp
   src/thrift/protocol/TJSONProtocol.cpp
   src/thrift/protocol/TMultiplexedProtocol.cpp
   src/thrift/protocol/TProtocol.cpp
   src/thrift/transport/TTransportException.cpp
   src/thrift/transport/TFDTransport.cpp
   src/thrift/transport/TSimpleFileTransport.cpp
   src/thrift/transport/THttpTransport.cpp
   src/thrift/transport/THttpClient.cpp
   src/thrift/transport/THttpServer.cpp
   src/thrift/transport/TSocket.cpp
   src/thrift/transport/TSocketPool.cpp
   src/thrift/transport/TServerSocket.cpp
   src/thrift/transport/TTransportUtils.cpp
   src/thrift/transport/TBufferTransports.cpp
   src/thrift/transport/TWebSocketServer.h
   src/thrift/transport/TWebSocketServer.cpp
   src/thrift/transport/SocketCommon.cpp
   src/thrift/server/TConnectedClient.cpp
   src/thrift/server/TServerFramework.cpp
   src/thrift/server/TSimpleServer.cpp
   src/thrift/server/TThreadPoolServer.cpp
   src/thrift/server/TThreadedServer.cpp
)

# These files don't work on Windows CE as there is no pipe support
# TODO: These files won't work with UNICODE support on windows. If fixed this can be re-added.
if (NOT WINCE)
    list(APPEND thriftcpp_SOURCES
       src/thrift/transport/TPipe.cpp
       src/thrift/transport/TPipeServer.cpp
       src/thrift/transport/TFileTransport.cpp
    )
endif()


if (WIN32)
    list(APPEND thriftcpp_SOURCES
        src/thrift/windows/TWinsockSingleton.cpp
        src/thrift/windows/SocketPair.cpp
        src/thrift/windows/GetTimeOfDay.cpp
        src/thrift/windows/WinFcntl.cpp
    )
    if(NOT WINCE)
        # This file uses pipes so it currently won't work on Windows CE
        list(APPEND thriftcpp_SOURCES
            src/thrift/windows/OverlappedSubmissionThread.cpp
        )
    endif()
else()
    # These files evaluate to nothing on Windows, so omit them from the
    # Windows build
    list(APPEND thriftcpp_SOURCES
        src/thrift/VirtualProfiling.cpp
        src/thrift/server/TServer.cpp
    )
endif()

# If OpenSSL is not found or disabled just ignore the OpenSSL stuff
if(OPENSSL_FOUND AND WITH_OPENSSL)
    list( APPEND thriftcpp_SOURCES
       src/thrift/transport/TSSLSocket.cpp
       src/thrift/transport/TSSLServerSocket.cpp
    )
    include_directories(SYSTEM "${OPENSSL_INCLUDE_DIR}")
    list(APPEND SYSLIBS "${OPENSSL_LIBRARIES}")
endif()

if(UNIX)
    if(ANDROID)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
    else()
        list(APPEND SYSLIBS pthread)
    endif()
endif()
set( thriftcpp_threads_SOURCES
    src/thrift/concurrency/ThreadFactory.cpp
    src/thrift/concurrency/Thread.cpp
    src/thrift/concurrency/Monitor.cpp
    src/thrift/concurrency/Mutex.cpp
)

# Thrift non blocking server
set( thriftcppnb_SOURCES
    src/thrift/server/TNonblockingServer.cpp
    src/thrift/transport/TNonblockingServerSocket.cpp
    src/thrift/async/TEvhttpServer.cpp
    src/thrift/async/TEvhttpClientChannel.cpp
)

# If OpenSSL is not found or disabled just ignore the OpenSSL stuff
if(OPENSSL_FOUND AND WITH_OPENSSL)
    list( APPEND thriftcppnb_SOURCES
    src/thrift/transport/TNonblockingSSLServerSocket.cpp
    )
endif()

# Thrift zlib transport
set( thriftcppz_SOURCES
    src/thrift/transport/TZlibTransport.cpp
    src/thrift/protocol/THeaderProtocol.cpp
    src/thrift/transport/THeaderTransport.cpp
    src/thrift/protocol/THeaderProtocol.cpp
    src/thrift/transport/THeaderTransport.cpp
)

# Contains the thrift specific ADD_LIBRARY_THRIFT and TARGET_LINK_LIBRARIES_THRIFT
include(ThriftMacros)

ADD_LIBRARY_THRIFT(thrift ${thriftcpp_SOURCES} ${thriftcpp_threads_SOURCES})
if(WIN32)
    TARGET_LINK_LIBRARIES_THRIFT(thrift ${SYSLIBS} ws2_32)
else()
    TARGET_LINK_LIBRARIES_THRIFT(thrift ${SYSLIBS})
endif()
ADD_PKGCONFIG_THRIFT(thrift)

if(WITH_LIBEVENT)
    find_package(Libevent REQUIRED)  # Libevent comes with CMake support from upstream
    include_directories(SYSTEM ${LIBEVENT_INCLUDE_DIRS})

    ADD_LIBRARY_THRIFT(thriftnb ${thriftcppnb_SOURCES})
    LINK_AGAINST_THRIFT_LIBRARY(thriftnb thrift)
    TARGET_LINK_LIBRARIES_THRIFT(thriftnb ${SYSLIBS} ${LIBEVENT_LIBRARIES})
    ADD_PKGCONFIG_THRIFT(thrift-nb)
endif()

if(WITH_ZLIB)
    find_package(ZLIB REQUIRED)
    include_directories(SYSTEM ${ZLIB_INCLUDE_DIRS})

    ADD_LIBRARY_THRIFT(thriftz ${thriftcppz_SOURCES})
    TARGET_LINK_LIBRARIES_THRIFT(thriftz ${SYSLIBS} ${ZLIB_LIBRARIES})
    TARGET_LINK_LIBRARIES_THRIFT_AGAINST_THRIFT_LIBRARY(thriftz thrift)
    ADD_PKGCONFIG_THRIFT(thrift-z)
endif()

if(WITH_QT5)
    add_subdirectory(src/thrift/qt)
    ADD_PKGCONFIG_THRIFT(thrift-qt5)
endif()

if(MSVC)
    add_definitions("-DUNICODE -D_UNICODE")
endif()

# Install the headers
install(DIRECTORY "src/thrift" DESTINATION "${INCLUDE_INSTALL_DIR}"
    FILES_MATCHING PATTERN "*.h" PATTERN "*.tcc")
# Copy config.h file
install(DIRECTORY "${CMAKE_BINARY_DIR}/thrift" DESTINATION "${INCLUDE_INSTALL_DIR}"
    FILES_MATCHING PATTERN "*.h")

if(BUILD_TESTING)
    add_subdirectory(test)
endif()
