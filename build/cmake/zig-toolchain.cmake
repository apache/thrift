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

# CMake cross compile toolchain file that builds with zig cc and zig c++.
#
#   cmake -DCMAKE_TOOLCHAIN_FILE=build/cmake/zig-toolchain.cmake \
#         -DZIG_TARGET=x86_64-linux-musl -DBUILD_LIBRARIES=OFF ...
#
# ZIG_TARGET is a Zig target triple, for example x86_64-linux-musl,
# aarch64-macos or x86_64-windows-gnu. zig is looked up on PATH unless ZIG
# names the executable. Use a fresh build directory for each target: CMake
# keeps the compiler it found in the first configure.

if(NOT ZIG_TARGET)
  message(FATAL_ERROR "Set ZIG_TARGET to a Zig target triple, for example x86_64-linux-musl")
endif()

if(NOT ZIG)
  find_program(ZIG zig)
  if(NOT ZIG)
    message(FATAL_ERROR "zig not found on PATH; set ZIG to the zig executable")
  endif()
endif()

# try_compile projects read this file again, without the cache
list(APPEND CMAKE_TRY_COMPILE_PLATFORM_VARIABLES ZIG_TARGET ZIG)

string(REPLACE "-" ";" _zig_target_parts "${ZIG_TARGET}")
list(GET _zig_target_parts 0 CMAKE_SYSTEM_PROCESSOR)
list(GET _zig_target_parts 1 _zig_target_os)
# aarch64-macos.13.0 names a minimum OS version
string(REGEX REPLACE "\\..*" "" _zig_target_os "${_zig_target_os}")
if(_zig_target_os STREQUAL "linux")
  set(CMAKE_SYSTEM_NAME Linux)
elseif(_zig_target_os STREQUAL "macos")
  set(CMAKE_SYSTEM_NAME Darwin)
elseif(_zig_target_os STREQUAL "windows")
  set(CMAKE_SYSTEM_NAME Windows)
else()
  message(FATAL_ERROR "Unsupported operating system '${_zig_target_os}' in ZIG_TARGET ${ZIG_TARGET}")
endif()

set(CMAKE_C_COMPILER "${ZIG}")
set(CMAKE_C_COMPILER_ARG1 "cc -target ${ZIG_TARGET}")
set(CMAKE_CXX_COMPILER "${ZIG}")
set(CMAKE_CXX_COMPILER_ARG1 "c++ -target ${ZIG_TARGET}")

# CMAKE_AR and CMAKE_RANLIB name a program without arguments, and some
# platform rules call CMAKE_AR directly (Windows-GNU archives the objects of an
# executable), so both point at wrapper scripts that call "zig ar" and
# "zig ranlib".
set(_zig_wrapper_dir "${CMAKE_BINARY_DIR}/zig-wrappers")
foreach(tool ar ranlib)
  if(CMAKE_HOST_WIN32)
    set(_zig_wrapper_name "zig-${tool}.cmd")
    file(TO_NATIVE_PATH "${ZIG}" _zig_native)
    set(_zig_wrapper_text "@\"${_zig_native}\" ${tool} %*\r\n")
  else()
    set(_zig_wrapper_name "zig-${tool}")
    set(_zig_wrapper_text "#!/bin/sh\nexec \"${ZIG}\" ${tool} \"$@\"\n")
  endif()
  file(WRITE "${_zig_wrapper_dir}/tmp/${_zig_wrapper_name}" "${_zig_wrapper_text}")
  file(COPY "${_zig_wrapper_dir}/tmp/${_zig_wrapper_name}" DESTINATION "${_zig_wrapper_dir}"
       FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
  string(TOUPPER "${tool}" _zig_tool_upper)
  set(CMAKE_${_zig_tool_upper} "${_zig_wrapper_dir}/${_zig_wrapper_name}" CACHE FILEPATH "zig ${tool}" FORCE)
endforeach()

# Programs such as flex and bison run on the host.
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
