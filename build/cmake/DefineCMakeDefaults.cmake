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


# Always include srcdir and builddir in include path
# This saves typing ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY} in
# about every subdir
# since cmake 2.4.0
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Put the include dirs which are in the source or build tree
# before all other include dirs, so the headers in the sources
# are preferred over the already installed ones
# since cmake 2.4.1
set(CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON)

# Use colored output
# since cmake 2.4.0
set(CMAKE_COLOR_MAKEFILE ON)

# Create the compile command database for clang by default
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set the CMAKE_BUILD_TYPE if it is not already defined
include(BuildType)

# Put the libraries and binaries that get built into directories at the
# top of the build tree rather than in hard-to-find leaf
# directories. This simplifies manual testing and the use of the build
# tree rather than installed thrift libraries.
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

#
# "rpath" support.
# See http://www.itk.org/Wiki/index.php?title=CMake_RPATH_handling
#
# On MacOSX, for shared libraries, enable rpath support.
set(CMAKE_MACOSX_RPATH TRUE)
#
# On any OS, for executables, allow linking with shared libraries in non-system
# locations and running the executables without LD_PRELOAD or similar.
# This requires the library to be built with rpath support.
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

#
# C++ Language Level Defaults - this depends on the compiler capabilities
#
if (NOT DEFINED CMAKE_CXX_STANDARD)
  set(CMAKE_CXX_STANDARD 11) # C++11
  message(STATUS "Setting C++11 as the default language level.")
  message(STATUS "To specify a different C++ language level, set CMAKE_CXX_STANDARD")
endif()

if (CMAKE_CXX_STANDARD EQUAL 98)
  message(FATAL_ERROR "only C++11 or above C++ standard is supported")
elseif (CMAKE_CXX_STANDARD EQUAL 11)
  # should not fallback to C++98
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()

if (NOT DEFINED CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)        # use standards compliant language level for portability
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  include(CheckCXXCompilerFlag)
  set(CMAKE_REQUIRED_QUIET ON)
  check_cxx_compiler_flag("/Zc:__cplusplus" res_var)
  if (res_var)
    # Make MSVC reporting correct value for __cplusplus
    # See https://blogs.msdn.microsoft.com/vcblog/2018/04/09/msvc-now-correctly-reports-__cplusplus/
    add_compile_options("/Zc:__cplusplus")
  endif()
endif()
