#!/bin/bash

# Tests the parser, independently of whether any generators
# are correct or useful.
# Currently only tests that valid .thrift files parse cleanly.
# Doesn't test that correct information is extracted from them.

shopt -s extglob

MY_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR=`cd $MY_DIR/../../ && pwd`
TEST_THRIFT_DIR=${ROOT_DIR}/test
THRIFT_FILES=`find ${TEST_THRIFT_DIR} -type f -name *.thrift ! -name BrokenConstants.thrift`

OUTPUT_DIR=`mktemp -d -t test_thrift_parser.XXXXX`

PASS=0
FAIL=0
for f in ${THRIFT_FILES};
do
  echo "Parsing ${f}"
  ${MY_DIR}/thrift -o ${OUTPUT_DIR} -nowarn --allow-64bit-consts --gen cpp ${f}
  EXIT_CODE=$?
  if [ ${EXIT_CODE} -eq 0 ]; then
    let PASS=PASS+1
  else
    let FAIL=FAIL+1
  fi
done
echo
echo "${PASS} files parsed correctly. ${FAIL} files failed to parse."
