#!/bin/bash

rm -rf gen-py
../../../compiler/cpp/thrift --gen py test1.thrift || exit 1
../../../compiler/cpp/thrift --gen py test2.thrift || exit 1
PYTHONPATH=./gen-py python -c 'import foo.bar.baz' || exit 1
PYTHONPATH=./gen-py python -c 'import test2' || exit 1
PYTHONPATH=./gen-py python -c 'import test1' &>/dev/null && exit 1  # Should fail.
cp -r gen-py simple
../../../compiler/cpp/thrift -r --gen py test2.thrift || exit 1
PYTHONPATH=./gen-py python -c 'import test2' || exit 1
diff -ur simple gen-py > thediffs
file thediffs | grep -s -q empty || exit 1
rm -rf simple thediffs
echo 'All tests pass!'
