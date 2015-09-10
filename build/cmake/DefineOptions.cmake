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
option(BUILD_TUTORIALS "Build Thrift tutorials" ON)
option(BUILD_LIBRARIES "Build Thrift libraries" ON)

# Libraries to build

# Each language library can be enabled or disabled using the WITH_<LANG> flag.
# By default CMake checks if the required dependencies for a language are present
# and enables the library if all are found. This means the default is to build as
# much as possible but leaving out libraries if their dependencies are not met.

# C++
option(WITH_CPP "Build C++ Thrift library" ON)
find_package(Boost 1.53 QUIET)
CMAKE_DEPENDENT_OPTION(BUILD_CPP "Build C++ library" ON
                       "BUILD_LIBRARIES;WITH_CPP;Boost_FOUND" OFF)
# NOTE: Currently the following options are C++ specific,
# but in future other libraries might reuse them.
# So they are not dependent on WITH_CPP but setting them without WITH_CPP currently
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
option(WITH_STDTHREADS "Build with C++ std::thread support" OFF)
CMAKE_DEPENDENT_OPTION(WITH_BOOSTTHREADS "Build with Boost threads support" OFF
    "NOT WITH_STDTHREADS;Boost_FOUND" OFF)


# C GLib
option(WITH_C_GLIB "Build C (GLib) Thrift library" ON)
find_package(GLIB QUIET COMPONENTS gobject)
CMAKE_DEPENDENT_OPTION(BUILD_C_GLIB "Build C (GLib) library" ON
                       "BUILD_LIBRARIES;WITH_C_GLIB;GLIB_FOUND" OFF)
# Java
option(WITH_JAVA "Build Java Thrift library" ON)
find_package(Java QUIET)
find_package(Ant QUIET)
CMAKE_DEPENDENT_OPTION(BUILD_JAVA "Build Java library" ON
                       "BUILD_LIBRARIES;WITH_JAVA;JAVA_FOUND;ANT_FOUND" OFF)

# Python
option(WITH_PYTHON "Build Python Thrift library" ON)
find_package(PythonInterp QUIET) # for Python executable
find_package(PythonLibs QUIET) # for Python.h
CMAKE_DEPENDENT_OPTION(BUILD_PYTHON "Build Python library" ON
                       "BUILD_LIBRARIES;WITH_PYTHON;PYTHONLIBS_FOUND" OFF)

# Common library options
option(WITH_SHARED_LIB "Build shared libraries" ON)
option(WITH_STATIC_LIB "Build static libraries" ON)
if (NOT WITH_SHARED_LIB AND NOT WITH_STATIC_LIB)
    message(FATAL_ERROR "Cannot build with both shared and static outputs disabled!")
endif()

#NOTE: C++ compiler options are defined in the lib/cpp/CMakeLists.txt

# Visual Studio only options
if(MSVC)
option(WITH_MT "Build using MT instead of MD (MSVC only)" OFF)
endif(MSVC)

macro(MESSAGE_DEP flag summary)
if(NOT ${flag})
  message(STATUS "   - ${summary}")
endif()
endmacro(MESSAGE_DEP flag summary)

macro(PRINT_CONFIG_SUMMARY)
message(STATUS "----------------------------------------------------------")
message(STATUS "Thrift version:                       ${thrift_VERSION} (${thrift_VERSION_MAJOR}.${thrift_VERSION_MINOR}.${thrift_VERSION_PATCH})")
message(STATUS "Thrift package version:               ${PACKAGE_VERSION}")
message(STATUS "Build configuration Summary")
message(STATUS "  Build Thrift compiler:              ${BUILD_COMPILER}")
message(STATUS "  Build with unit tests:              ${BUILD_TESTING}")
message(STATUS "  Build examples:                     ${BUILD_EXAMPLES}")
message(STATUS "  Build Thrift libraries:             ${BUILD_LIBRARIES}")
message(STATUS " Language libraries:")
message(STATUS "  Build C++ library:                  ${BUILD_CPP}")
MESSAGE_DEP(WITH_CPP "Disabled by via WITH_CCP=OFF")
MESSAGE_DEP(Boost_FOUND "Boost headers missing")
message(STATUS "  Build C (GLib) library:             ${BUILD_C_GLIB}")
MESSAGE_DEP(WITH_C_GLIB "Disabled by via WITH_C_GLIB=OFF")
MESSAGE_DEP(GLIB_FOUND "GLib missing")
message(STATUS "  Build Java library:                 ${BUILD_JAVA}")
MESSAGE_DEP(WITH_JAVA "Disabled by via WITH_JAVA=OFF")
MESSAGE_DEP(JAVA_FOUND "Java Runtime missing")
MESSAGE_DEP(ANT_FOUND "Ant missing")
message(STATUS "  Build Python library:               ${BUILD_PYTHON}")
MESSAGE_DEP(WITH_PYTHON "Disabled by via WITH_PYTHON=OFF")
MESSAGE_DEP(PYTHONLIBS_FOUND "Python libraries missing")
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
