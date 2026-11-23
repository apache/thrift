#!/bin/sh

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

set -ev

CMAKE_FLAGS=$*
MAKEPROG=make

if ninja --version  >/dev/null 2>&1; then
  MAKEPROG=ninja
  CMAKE_FLAGS="-GNinja $CMAKE_FLAGS"
fi

mkdir -p cmake_build && cd cmake_build
cmake $CMAKE_FLAGS ..
for LIB in $BUILD_LIBS; do
  if ! grep "^BUILD_${LIB}:BOOL=ON$" CMakeCache.txt ; then
    echo "failed to configure $LIB"
    exit 1
  fi
done
$MAKEPROG -j3
cpack
ctest -VV
