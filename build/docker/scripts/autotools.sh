#!/bin/sh
set -ev

# haxe hxcpp > 3.4.188 will enable c++11 by default, and break the
#      build when compiling C files with clang++ by adding -std=c++11
export HXCPP_NO_CPP11=1

./bootstrap.sh
./configure $*
make check -j3
