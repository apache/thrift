#!/bin/sh
set -ev

mkdir -p cmake_build && cd cmake_build
cmake $* ..
make -j4
cpack
ctest -VV -E "(concurrency_test|processor_test)"
