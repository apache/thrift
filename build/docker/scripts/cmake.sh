#!/bin/sh
set -ev

mkdir -p cmake_build && cd cmake_build
cmake $* ..
make -j3
cpack
ctest -VV -E "(concurrency_test|processor_test)"
