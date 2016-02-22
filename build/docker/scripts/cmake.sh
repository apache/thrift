#!/bin/sh
set -ev

mkdir -p cmake_build && cd cmake_build
cmake $* ..
for LIB in $BUILD_LIBS; do
  if ! grep "^BUILD_${LIB}:BOOL=ON$" CMakeCache.txt ; then
    echo "failed to configure $LIB"
    exit 1
  fi
done
make -j3
cpack
ctest -VV -E "(concurrency_test|processor_test)"
