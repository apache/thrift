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

set(THRIFT_COMPILER "" CACHE FILEPATH "External Thrift compiler to use during build")

# Additional components
option(BUILD_COMPILER "Build Thrift compiler" ON)

if(BUILD_COMPILER OR EXISTS ${THRIFT_COMPILER})
    set(HAVE_COMPILER ON)
endif()
CMAKE_DEPENDENT_OPTION(BUILD_TESTING "Build with unit tests" ON "HAVE_COMPILER" OFF)
CMAKE_DEPENDENT_OPTION(BUILD_TUTORIALS "Build Thrift tutorials" ON "HAVE_COMPILER" OFF)
option(BUILD_LIBRARIES "Build Thrift libraries" ON)

# Libraries to build

# Each language library can be enabled or disabled using the WITH_<LANG> flag.
# By default CMake checks if the required dependencies for a language are present
# and enables the library if all are found. This means the default is to build as
# much as possible but leaving out libraries if their dependencies are not met.

if (NOT Boost_USE_STATIC_LIBS)
    add_definitions(-DBOOST_ALL_DYN_LINK)
    add_definitions(-DBOOST_TEST_DYN_LINK)
endif()

# as3
option(WITH_AS3 "Build ActionScript 3 Thrift Library" ON)
if (WITH_AS3)
    set(POSSIBLE_PATHS "${FLEX_HOME}/bin" "$ENV{FLEX_HOME}/bin")
    find_program(HAVE_COMPC NAMES compc HINTS ${POSSIBLE_PATHS})
endif ()
CMAKE_DEPENDENT_OPTION(BUILD_AS3 "Build as3 library" ON
                       "BUILD_LIBRARIES;WITH_AS3;HAVE_COMPC" OFF)

# C++
option(WITH_CPP "Build C++ Thrift library" ON)
if(WITH_CPP)
    # NOTE: Currently the following options are C++ specific,
    # but in future other libraries might reuse them.
    # So they are not dependent on WITH_CPP but setting them without WITH_CPP currently
    # has no effect.
    if(ZLIB_LIBRARY)
        # FindZLIB.cmake does not normalize path so we need to do it ourselves.
        file(TO_CMAKE_PATH ${ZLIB_LIBRARY} ZLIB_LIBRARY)
    endif()
    find_package(ZLIB QUIET)
    CMAKE_DEPENDENT_OPTION(WITH_ZLIB "Build with ZLIB support" ON
                           "ZLIB_FOUND" OFF)
    find_package(Libevent QUIET)
    CMAKE_DEPENDENT_OPTION(WITH_LIBEVENT "Build with libevent support" ON
                           "Libevent_FOUND" OFF)
    find_package(Qt5 QUIET COMPONENTS Core Network)
    CMAKE_DEPENDENT_OPTION(WITH_QT5 "Build with Qt5 support" ON
                           "Qt5_FOUND" OFF)
    find_package(OpenSSL QUIET)
    CMAKE_DEPENDENT_OPTION(WITH_OPENSSL "Build with OpenSSL support" ON
                           "OPENSSL_FOUND" OFF)
endif()
CMAKE_DEPENDENT_OPTION(BUILD_CPP "Build C++ library" ON
                       "BUILD_LIBRARIES;WITH_CPP" OFF)

# C GLib
option(WITH_C_GLIB "Build C (GLib) Thrift library" ON)
if(WITH_C_GLIB)
    find_package(GLIB QUIET COMPONENTS gobject)
endif()
CMAKE_DEPENDENT_OPTION(BUILD_C_GLIB "Build C (GLib) library" ON
                       "BUILD_LIBRARIES;WITH_C_GLIB;GLIB_FOUND" OFF)

# Java
option(WITH_JAVA "Build Java Thrift library" ON)
if(ANDROID)
    find_package(Gradle QUIET)
    CMAKE_DEPENDENT_OPTION(BUILD_JAVA "Build Java library" ON
                           "BUILD_LIBRARIES;WITH_JAVA;GRADLE_FOUND" OFF)
else()
    find_package(Gradlew QUIET)
    find_package(Java QUIET)
    CMAKE_DEPENDENT_OPTION(BUILD_JAVA "Build Java library" ON
                           "BUILD_LIBRARIES;WITH_JAVA;JAVA_FOUND;GRADLEW_FOUND" OFF)
endif()

# Python
option(WITH_PYTHON "Build Python Thrift library" ON)
find_package(PythonInterp QUIET) # for Python executable
find_package(PythonLibs QUIET) # for Python.h
CMAKE_DEPENDENT_OPTION(BUILD_PYTHON "Build Python library" ON
                       "BUILD_LIBRARIES;WITH_PYTHON;PYTHONINTERP_FOUND;PYTHONLIBS_FOUND" OFF)

# Haskell
option(WITH_HASKELL "Build Haskell Thrift library" ON)
find_package(GHC QUIET)
find_package(Cabal QUIET)
CMAKE_DEPENDENT_OPTION(BUILD_HASKELL "Build GHC library" ON
                       "BUILD_LIBRARIES;WITH_HASKELL;GHC_FOUND;CABAL_FOUND" OFF)

# Common library options
# https://cmake.org/cmake/help/latest/variable/BUILD_SHARED_LIBS.html
# Default on Windows is static, shared mode library support needs work...
CMAKE_DEPENDENT_OPTION(BUILD_SHARED_LIBS "Build shared libraries" OFF "WIN32" ON)

if (WITH_SHARED_LIB)
    message(WARNING "WITH_SHARED_LIB is deprecated; use -DBUILD_SHARED_LIBS=ON instead")
    set(BUILD_SHARED_LIBS ON)
elseif (WITH_STATIC_LIB)
    if (WITH_SHARED_LIB)
        message(FATAL_ERROR "Cannot build shared and static together; set BUILD_SHARED_LIBS instead.")
    endif ()
    message(WARNING "WITH_STATIC_LIB is deprecated; use -DBUILD_SHARED_LIBS=OFF instead")
    set(BUILD_SHARED_LIBS OFF)
endif ()

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
message(STATUS "Thrift version:                               ${thrift_VERSION} (${thrift_VERSION_MAJOR}.${thrift_VERSION_MINOR}.${thrift_VERSION_PATCH})")
message(STATUS "Thrift package version:                       ${PACKAGE_VERSION}")
message(STATUS)
message(STATUS "Build configuration summary")
message(STATUS "  Build compiler:                             ${BUILD_COMPILER}")
message(STATUS "  Build libraries:                            ${BUILD_LIBRARIES}")
message(STATUS "  Build tests:                                ${BUILD_TESTING}")
MESSAGE_DEP(HAVE_COMPILER "Disabled because BUILD_THRIFT=OFF and no valid THRIFT_COMPILER is given")
message(STATUS "  Build type:                                 ${CMAKE_BUILD_TYPE}")
message(STATUS)
message(STATUS "Language libraries:")
message(STATUS)
message(STATUS "  Build as3 library:                          ${BUILD_AS3}")
MESSAGE_DEP(WITH_AS3 "Disabled by WITH_AS3=OFF")
MESSAGE_DEP(HAVE_COMPC "Adobe Flex compc was not found - did you set env var FLEX_HOME?")
message(STATUS)
message(STATUS "  Build C++ library:                          ${BUILD_CPP}")
MESSAGE_DEP(WITH_CPP "Disabled by WITH_CPP=OFF")
if (BUILD_CPP)
    message(STATUS "    C++ Language Level:                       ${CXX_LANGUAGE_LEVEL}")
    message(STATUS "    Build shared libraries:                   ${BUILD_SHARED_LIBS}")
    message(STATUS "    Build with libevent support:              ${WITH_LIBEVENT}")
    message(STATUS "    Build with Qt5 support:                   ${WITH_QT5}")
    message(STATUS "    Build with ZLIB support:                  ${WITH_ZLIB}")
endif ()
message(STATUS)
message(STATUS "  Build C (GLib) library:                     ${BUILD_C_GLIB}")
MESSAGE_DEP(WITH_C_GLIB "Disabled by WITH_C_GLIB=OFF")
MESSAGE_DEP(GLIB_FOUND "GLib missing")
message(STATUS)
message(STATUS "  Build Java library:                         ${BUILD_JAVA}")
MESSAGE_DEP(WITH_JAVA "Disabled by WITH_JAVA=OFF")
if(ANDROID)
    MESSAGE_DEP(GRADLE_FOUND "Gradle missing")
else()
    MESSAGE_DEP(JAVA_FOUND "Java Runtime missing")
    MESSAGE_DEP(GRADLEW_FOUND "Gradle Wrapper missing")
endif()
message(STATUS)
message(STATUS "  Build Python library:                       ${BUILD_PYTHON}")
MESSAGE_DEP(WITH_PYTHON "Disabled by WITH_PYTHON=OFF")
MESSAGE_DEP(PYTHONLIBS_FOUND "Python libraries missing")
message(STATUS)
message(STATUS "  Build Haskell library:                      ${BUILD_HASKELL}")
MESSAGE_DEP(WITH_HASKELL "Disabled by WITH_HASKELL=OFF")
MESSAGE_DEP(GHC_FOUND "GHC missing")
MESSAGE_DEP(CABAL_FOUND "Cabal missing")
message(STATUS)
message(STATUS "----------------------------------------------------------")
endmacro(PRINT_CONFIG_SUMMARY)
