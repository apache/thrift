#!/bin/sh
set -ev

./bootstrap.sh
./configure $*
make check -j4
