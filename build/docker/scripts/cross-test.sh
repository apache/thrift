#!/bin/sh
set -ev

./bootstrap.sh
./configure --enable-tutorial=no
make -j3 precross

set +e
make cross$1

RET=$?
if [ $RET -ne 0 ]; then
  if [ -f "test/features/log/unexpected_failures.log" ]; then 
    cat "test/features/log/unexpected_failures.log"
  fi
  if [ -f "test/log/unexpected_failures.log" ]; then
    cat "test/log/unexpected_failures.log"
  fi
fi

exit $RET
