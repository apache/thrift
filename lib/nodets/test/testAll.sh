#! /bin/sh

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

DIR="$( cd "$( dirname "$0" )" && pwd )"

mkdir -p $DIR/../test-compiled

COMPILEDDIR="$(cd $DIR && cd ../test-compiled && pwd)"
export NODE_PATH="${DIR}:${DIR}/../../nodejs/lib:${NODE_PATH}"

compile()
{
  #generating thrift code
  ${DIR}/../../../compiler/cpp/thrift -o ${DIR} --gen js:node,ts ${DIR}/../../../test/ThriftTest.thrift
  ${DIR}/../../../compiler/cpp/thrift -o ${DIR} --gen js:node,ts ${DIR}/../../../test/Int64Test.thrift
  ${DIR}/../../../compiler/cpp/thrift -o ${COMPILEDDIR} --gen js:node,ts ${DIR}/../../../test/ThriftTest.thrift
  ${DIR}/../../../compiler/cpp/thrift -o ${COMPILEDDIR} --gen js:node,ts ${DIR}/../../../test/Int64Test.thrift

  tsc --outDir $COMPILEDDIR --project $DIR/tsconfig.json
}
compile

testServer()
{
  echo "start server $1"
  RET=0
  node ${COMPILEDDIR}/server.js $1 &
  SERVERPID=$!
  sleep 1
  echo "start client $1"
  node ${COMPILEDDIR}/client.js $1 || RET=1
  kill -2 $SERVERPID || RET=1
  return $RET
}

node ${COMPILEDDIR}/int64.test.js || TESTOK=1

#integration tests

testServer || TESTOK=1
testServer --promise || TESTOK=1

exit $TESTOK
