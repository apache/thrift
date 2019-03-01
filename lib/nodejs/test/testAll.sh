#! /bin/sh

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

if [ -n "${1}" ]; then
  COVER=${1};
fi

DIR="$( cd "$( dirname "$0" )" && pwd )"

EPISODIC_DIR=${DIR}/episodic-code-generation-test

THRIFT_FILES_DIR=${DIR}/../../../test

THRIFT_COMPILER=${DIR}/../../../compiler/cpp/thrift

ISTANBUL="$DIR/../../../node_modules/istanbul/lib/cli.js"

REPORT_PREFIX="${DIR}/../coverage/report"

COUNT=0

export NODE_PATH="${DIR}:${DIR}/../lib:${NODE_PATH}"

testServer()
{
  echo "  [ECMA $1] Testing $2 Client/Server with protocol $3 and transport $4 $5";
  RET=0
  if [ -n "${COVER}" ]; then
    ${ISTANBUL} cover ${DIR}/server.js --dir ${REPORT_PREFIX}${COUNT} --handle-sigint -- --type $2 -p $3 -t $4 $5 &
    COUNT=$((COUNT+1))
  else
    node ${DIR}/server.js --${1} --type $2 -p $3 -t $4 $5 &
  fi
  SERVERPID=$!
  sleep 0.1
  if [ -n "${COVER}" ]; then
    ${ISTANBUL} cover ${DIR}/client.js --dir ${REPORT_PREFIX}${COUNT} -- --${1} --type $2 -p $3 -t $4 $5 || RET=1
    COUNT=$((COUNT+1))
  else
    node ${DIR}/client.js --${1} --type $2 -p $3 -t $4 $5 || RET=1
  fi
  kill -2 $SERVERPID || RET=1
  wait $SERVERPID
  return $RET
}

testEpisodicCompilation()
{
  RET=0
  if [ -n "${COVER}" ]; then
    ${ISTANBUL} cover ${EPISODIC_DIR}/server.js --dir ${REPORT_PREFIX}${COUNT} --handle-sigint &
    COUNT=$((COUNT+1))
  else
    node ${EPISODIC_DIR}/server.js &
  fi
  SERVERPID=$!
  sleep 0.1
  if [ -n "${COVER}" ]; then
    ${ISTANBUL} cover ${EPISODIC_DIR}/client.js --dir ${REPORT_PREFIX}${COUNT} || RET=1
    COUNT=$((COUNT+1))
  else
    node ${EPISODIC_DIR}/client.js || RET=1
  fi
  kill -2 $SERVERPID || RET=1
  wait $SERVERPID
  return $RET
}


TESTOK=0

# generating Thrift code

${THRIFT_COMPILER} -o ${DIR} --gen js:node ${THRIFT_FILES_DIR}/ThriftTest.thrift
${THRIFT_COMPILER} -o ${DIR} --gen js:node ${THRIFT_FILES_DIR}/JsDeepConstructorTest.thrift
${THRIFT_COMPILER} -o ${DIR} --gen js:node ${THRIFT_FILES_DIR}/Int64Test.thrift
mkdir ${DIR}/gen-nodejs-es6
${THRIFT_COMPILER} -out ${DIR}/gen-nodejs-es6 --gen js:node,es6 ${THRIFT_FILES_DIR}/ThriftTest.thrift
${THRIFT_COMPILER} -out ${DIR}/gen-nodejs-es6 --gen js:node,es6 ${THRIFT_FILES_DIR}/JsDeepConstructorTest.thrift
${THRIFT_COMPILER} -out ${DIR}/gen-nodejs-es6 --gen js:node,es6 ${THRIFT_FILES_DIR}/Int64Test.thrift

# generate episodic compilation test code
TYPES_PACKAGE=${EPISODIC_DIR}/node_modules/types-package

# generate the first episode
mkdir --parents ${EPISODIC_DIR}/gen-1/first-episode
${THRIFT_COMPILER} -o ${EPISODIC_DIR}/gen-1/first-episode --gen js:node,thrift_package_output_directory=first-episode ${THRIFT_FILES_DIR}/Types.thrift

# create a "package" from the first episode and "install" it, the episode file must be at the module root
mkdir --parents ${TYPES_PACKAGE}/first-episode
cp --force ${EPISODIC_DIR}/episodic_compilation.package.json ${TYPES_PACKAGE}/package.json
cp --force ${EPISODIC_DIR}/gen-1/first-episode/gen-nodejs/Types_types.js ${TYPES_PACKAGE}/first-episode/
cp --force ${EPISODIC_DIR}/gen-1/first-episode/gen-nodejs/thrift.js.episode ${TYPES_PACKAGE}

# generate the second episode
mkdir --parents ${EPISODIC_DIR}/gen-2/second-episode
${THRIFT_COMPILER} -o ${EPISODIC_DIR}/gen-2/second-episode --gen js:node,imports=${TYPES_PACKAGE} ${THRIFT_FILES_DIR}/Service.thrift
if [ -f ${EPISODIC_DIR}/gen-2/second-episode/Types_types.js ]; then
  TESTOK=1
fi

# unit tests

node ${DIR}/binary.test.js || TESTOK=1
node ${DIR}/int64.test.js || TESTOK=1
node ${DIR}/deep-constructor.test.js || TESTOK=1

# integration tests

for type in tcp multiplex websocket http
do
  for protocol in compact binary json
  do
    for transport in buffered framed
    do
      for ecma_version in es5 es6
      do
        testServer $ecma_version $type $protocol $transport || TESTOK=1
        testServer $ecma_version $type $protocol $transport --ssl || TESTOK=1
        testServer $ecma_version $type $protocol $transport --callback || TESTOK=1
      done
    done
  done
done

# episodic compilation test
testEpisodicCompilation

if [ -n "${COVER}" ]; then
  ${ISTANBUL} report --dir "${DIR}/../coverage" --include "${DIR}/../coverage/report*/coverage.json" lcov cobertura html
  rm -r ${DIR}/../coverage/report*/*
  rmdir ${DIR}/../coverage/report*
fi

exit $TESTOK
