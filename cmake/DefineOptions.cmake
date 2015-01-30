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


include(CMakeDependentOption)

# Additional components
option(BUILD_COMPILER "Build Thrift compiler" ON)
option(BUILD_TESTING "Build with unit tests" ON)
option(BUILD_EXAMPLES "Build examples" ON)
option(BUILD_LIBRARIES "Build Thrfit libraries" ON)

# Libraries to build

# Each language library can be enabled or disabled using the WITH_<LANG> flag.
# By default CMake checks if the required dependencies for a language are present
# and enables the library if all are found. This means the default is to build as
# much as possible but leaving out libraries if their dependencies are not met.

# C++
find_package(Boost 1.53 QUIET)
CMAKE_DEPENDENT_OPTION(WITH_CPP "Build C++ library" ON
                       "BUILD_LIBRARIES;Boost_FOUND" OFF)
# NOTE: Currently the following options are C++ specific,
# but in future other libraries might reuse them.
# So they are not dependent on WIHT_CPP but setting them without WITH_CPP currently
# has no effect.
find_package(ZLIB QUIET)
CMAKE_DEPENDENT_OPTION(WITH_ZLIB "Build with ZLIB support" ON
                       "ZLIB_FOUND" OFF)
find_package(Libevent QUIET)
CMAKE_DEPENDENT_OPTION(WITH_LIBEVENT "Build with libevent support" ON
                       "Libevent_FOUND" OFF)
find_package(Qt4 QUIET COMPONENTS QtCore QtNetwork)
CMAKE_DEPENDENT_OPTION(WITH_QT4 "Build with Qt4 support" ON
                       "QT4_FOUND" OFF)
find_package(Qt5 QUIET COMPONENTS Core Network)
CMAKE_DEPENDENT_OPTION(WITH_QT5 "Build with Qt5 support" ON
                       "Qt5_FOUND" OFF)
if(${WITH_QT4} AND ${WITH_QT5} AND ${CMAKE_MAJOR_VERSION} LESS 3)
  # cmake < 3.0.0 causes conflict when building both Qt4 and Qt5
  set(WITH_QT4 OFF)
endif()
find_package(OpenSSL QUIET)
CMAKE_DEPENDENT_OPTION(WITH_OPENSSL "Build with OpenSSL support" ON
                       "OPENSSL_FOUND" OFF)
option(WITH_BOOSTTHREADS "Build with Boost thread support" OFF)
option(WITH_STDTHREADS "Build with C++ std::thread support" OFF)

# C GLib
find_package(GLIB QUIET COMPONENTS gobject)
CMAKE_DEPENDENT_OPTION(WITH_C_GLIB "Build C (GLib) library" ON
                       "BUILD_LIBRARIES;GLIB_FOUND" OFF)
# Java
find_package(Java QUIET)
find_package(Ant QUIET)
CMAKE_DEPENDENT_OPTION(WITH_JAVA "Build Java library" ON
                       "BUILD_LIBRARIES;JAVA_FOUND;Ant_FOUND" OFF)

# Common library options
option(WITH_SHARED_LIB "Build shared libraries" ON)
option(WITH_STATIC_LIB "Build static libraries" ON)

#NOTE: C++ compiler options are defined in the lib/cpp/CMakeLists.txt

# Visual Studio only options
if(MSVC)
option(WITH_MT "Build unsing MT instead of MT (MSVC only)" OFF)
endif(MSVC)

macro(PRINT_CONFIG_SUMMARY)
message(STATUS "----------------------------------------------------------")
message(STATUS "Thrift version:                       ${thrift_VERSION} (${thrift_VERSION_MAJOR}.${thrift_VERSION_MINOR}.${thrift_VERSION_PATCH})")
message(STATUS "Thrift package version:               ${PACKAGE_VERSION}")
message(STATUS "Build configuration Summary")
message(STATUS "  Build Thrift compiler:              ${BUILD_COMPILER}")
message(STATUS "  Build with unit tests:              ${BUILD_TESTING}")
message(STATUS "  Build examples:                     ${BUILD_EXAMPLES}")
message(STATUS "  Build Thrfit libraries:             ${BUILD_LIBRARIES}")
message(STATUS " Language libraries:")
message(STATUS "  Build C++ library:                  ${WITH_CPP}")
message(STATUS "  Build C (GLib) library:             ${WITH_C_GLIB}")
message(STATUS "  Build Java library:                 ${WITH_JAVA}")
message(STATUS " Library features:")
message(STATUS "  Build shared libraries:             ${WITH_SHARED_LIB}")
message(STATUS "  Build static libraries:             ${WITH_STATIC_LIB}")
message(STATUS "  Build with ZLIB support:            ${WITH_ZLIB}")
message(STATUS "  Build with libevent support:        ${WITH_LIBEVENT}")
message(STATUS "  Build with Qt4 support:             ${WITH_QT4}")
message(STATUS "  Build with Qt5 support:             ${WITH_QT5}")
message(STATUS "  Build with OpenSSL support:         ${WITH_OPENSSL}")
message(STATUS "  Build with Boost thread support:    ${WITH_BOOSTTHREADS}")
message(STATUS "  Build with C++ std::thread support: ${WITH_STDTHREADS}")
message(STATUS "----------------------------------------------------------")
endmacro(PRINT_CONFIG_SUMMARY)
