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

set(ANDROID_NDK "/opt/android-ndk" CACHE)
set(ANDROID_PLATFORM "android-15" CACHE)
set(ANDROID_ARCH "arch-arm" CACHE)
set(ANDROID_TOOL_ARCH "android-arm" CACHE)
set(ANDROID_CPU "armeabi-v7a" CACHE)
set(ANDROID_GCC_VERSION 4.9 CACHE)
set(HOST_ARCH linux-x86_64 CACHE)

set(CMAKE_SYSTEM_NAME Android)
set(ANDROID_SYSROOT "${ANDROID_NDK}/platforms/${ANDROID_PLATFORM}/${ANDROID_ARCH}")
set(ANDROID_TRIPLET arm-linux-androideabi)
set(ANDROID_STL "${ANDROID_NDK}/sources/cxx-stl/gnu-libstd++/${ANDROID_GCC_VERSION}")

set(_COMPILER_ROOT ${ANDROID_NDK}/prebuilt/${ANDROID_TRIPLET}-${ANDROID_GCC_VERSION}/prebuilt/${HOST_ARCH})
set(CMAKE_C_COMPILER ${_COMPILER_ROOT}/bin/${ANDROID_TRIPLET}-gcc)
set(CMAKE_CXCX_COMPILER ${_COMPILER_ROOT}/bin/${ANDROID_TRIPLET}-g++)

include_directories(
    ${ANDROID_STL}/include
    ${ANDROID_STL}/libs/${ANDROID_CPU}/include)

set(CMAKE_FIND_ROOT_PATH ${ANDROID_SYSROOT})

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
