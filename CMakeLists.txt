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

cmake_minimum_required(VERSION 3.4)

if(POLICY CMP0048)
  cmake_policy(SET CMP0048 NEW)  # package version behavior added in cmake 3.0
endif()
if(POLICY CMP0074)
  cmake_policy(SET CMP0074 NEW)  # find_package behavior added in cmake 3.12
endif()

# PACKAGE_VERSION is used by cpack scripts currently
# Both thrift_VERSION and PACKAGE_VERSION should be the same for now
set(thrift_VERSION "0.14.1")
set(PACKAGE_VERSION ${thrift_VERSION})

project("thrift" VERSION ${PACKAGE_VERSION})
message(STATUS "Configuring ${CMAKE_PROJECT_NAME} ${thrift_VERSION}")


set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/build/cmake")

# Some default settings
include(DefineCMakeDefaults)

# Build time options are defined here
include(DefineOptions)
include(DefineInstallationPaths)

# Based on the options set some platform specifics
include(DefinePlatformSpecifc)

# Add CMake targets for static code analysis
include(StaticCodeAnalysis)

# Generate the config.h file
include(ConfigureChecks)

# Generate the ThriftConfig.cmake module
include(GenerateConfigModule)

# Packaging
include(CPackConfig)

# Dependencies
include(BoostMacros)
find_package(Threads)
include(CTest)

if(BUILD_TESTING)
  message(STATUS "Building with unittests")

  enable_testing()
  # Define "make check" as alias for "make test"
  add_custom_target(check COMMAND ctest)
else ()
  message(STATUS "Building without tests")
endif ()

if(BUILD_COMPILER)
    if(NOT EXISTS ${THRIFT_COMPILER})
        set(THRIFT_COMPILER $<TARGET_FILE:thrift-compiler>)
    endif()
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/compiler/cpp)
elseif(EXISTS ${THRIFT_COMPILER})
    add_executable(thrift-compiler IMPORTED)
    set_property(TARGET thrift-compiler PROPERTY IMPORTED_LOCATION ${THRIFT_COMPILER})
endif()

if(BUILD_CPP)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/cpp)
    if(BUILD_TUTORIALS)
        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/tutorial/cpp)
    endif()
    if(BUILD_TESTING)
        if(WITH_LIBEVENT AND WITH_ZLIB AND WITH_OPENSSL)
            add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test/cpp)
        else()
            message(WARNING "libevent and/or ZLIB and/or OpenSSL not found or disabled; will not build some tests")
        endif()
    endif()
endif()

if(BUILD_AS3)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/as3)
endif()

if(BUILD_C_GLIB)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/c_glib)
    if(BUILD_TESTING)
        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test/c_glib)
    endif()
endif()

if(BUILD_JAVA)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/java)
endif()

if(BUILD_JAVASCRIPT)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/js)
endif()

if(BUILD_NODEJS)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/nodejs)
endif()

if(BUILD_PYTHON)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/py)
    if(BUILD_TESTING)
        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test/py)
    endif()
endif()

if(BUILD_HASKELL)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/hs)
    if(BUILD_TESTING)
        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test/hs)
    endif()
endif()

# Create the uninstall target
add_custom_target(uninstall "${CMAKE_COMMAND}" -P "${PROJECT_SOURCE_DIR}/build/cmake/uninstall.cmake")

PRINT_CONFIG_SUMMARY()
