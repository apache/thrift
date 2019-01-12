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

# Uncomment this to show some basic cmake variables about platforms
# include (NewPlatformDebug)

# For Debug build types, append a "d" to the library names.
set(CMAKE_DEBUG_POSTFIX "d" CACHE STRING "Set debug library postfix" FORCE)

# Visual Studio specific options
if(MSVC)
    # Allow for shared library builds
    if(BUILD_SHARED_LIBS)
        set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON CACHE TYPE BOOL FORCE)
    endif()

    #For visual studio the library naming is as following:
    # Dynamic libraries:
    #  - thrift.dll  for release library
    #  - thriftd.dll for debug library
    #
    # Static libraries:
    #  - thriftmd.lib for /MD release build
    #  - thriftmt.lib for /MT release build
    #
    #  - thriftmdd.lib for /MD debug build
    #  - thriftmtd.lib for /MT debug build
    #
    # the same holds for other libraries like libthriftz etc.

    # Build using /MT option instead of /MD if the WITH_MT options is set
    if(WITH_MT)
        set(CompilerFlags
                CMAKE_CXX_FLAGS
                CMAKE_CXX_FLAGS_DEBUG
                CMAKE_CXX_FLAGS_RELEASE
                CMAKE_CXX_FLAGS_RELWITHDEBINFO
                CMAKE_C_FLAGS
                CMAKE_C_FLAGS_DEBUG
                CMAKE_C_FLAGS_RELEASE
                CMAKE_C_FLAGS_RELWITHDEBINFO
                )
        foreach(CompilerFlag ${CompilerFlags})
          string(REPLACE "/MD" "/MT" ${CompilerFlag} "${${CompilerFlag}}")
        endforeach()
        set(THRIFT_RUNTIME_POSTFIX "mt" CACHE STRING "Set runtime library postfix" FORCE)
    else(WITH_MT)
        set(THRIFT_RUNTIME_POSTFIX "md" CACHE STRING "Set runtime library postfix" FORCE)
    endif(WITH_MT)

    # Disable boost auto linking pragmas - cmake includes the right files
    add_definitions("-DBOOST_ALL_NO_LIB")

    add_definitions("/MP") # parallel build
    add_definitions("/W3") # warning level 3

    add_definitions("-DUNICODE -D_UNICODE")
elseif(UNIX)
  find_program( MEMORYCHECK_COMMAND valgrind )
  set( MEMORYCHECK_COMMAND_OPTIONS "--gen-suppressions=all --leak-check=full" )
  set( MEMORYCHECK_SUPPRESSIONS_FILE "${PROJECT_SOURCE_DIR}/test/valgrind.suppress" )
endif()

add_definitions("-D__STDC_FORMAT_MACROS")
add_definitions("-D__STDC_LIMIT_MACROS")

# C++ Language Level
set(CXX_LANGUAGE_LEVEL "C++${CMAKE_CXX_STANDARD}")
if (CMAKE_CXX_STANDARD_REQUIRED)
  string(CONCAT CXX_LANGUAGE_LEVEL "${CXX_LANGUAGE_LEVEL} [compiler must support it]")
else()
  string(CONCAT CXX_LANGUAGE_LEVEL "${CXX_LANGUAGE_LEVEL} [fallback to earlier if compiler does not support it]")
endif()
if (CMAKE_CXX_EXTENSIONS)
  string(CONCAT CXX_LANGUAGE_LEVEL "${CXX_LANGUAGE_LEVEL} [with compiler-specific extensions]")
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-register")
endif()

# Building WITH_PLUGIN requires boost memory operations, for now, and gcc >= 4.8
if (WITH_PLUGIN)
  if (CMAKE_CXX_COMPILER_ID MATCHES "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.8")
    message(SEND_ERROR "Thrift compiler plug-in support is not possible with older gcc ( < 4.8 ) compiler")
  endif()
endif()

