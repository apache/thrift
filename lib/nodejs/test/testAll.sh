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
#

DIR="$( cd "$( dirname "$0" )" && pwd )"

export NODE_PATH="${DIR}:${DIR}/../lib:${NODE_PATH}"

testClientServer()
{
  echo "   Testing Client/Server with protocol $1 and transport $2 $3";
  RET=0
  node ${DIR}/server.js -p $1 -t $2 $3 &
  SERVERPID=$!
  sleep 1
  node ${DIR}/client.js -p $1 -t $2 $3 || RET=1
  kill -9 $SERVERPID || RET=1
  return $RET
}

testMultiplexedClientServer()
{
  echo "   Testing Multiplexed Client/Server with protocol $1 and transport $2 $3";
  RET=0
  node ${DIR}/multiplex_server.js -p $1 -t $2 $3 &
  SERVERPID=$!
  sleep 1
  node ${DIR}/multiplex_client.js -p $1 -t $2 $3 || RET=1
  kill -9 $SERVERPID || RET=1
  return $RET
}

testHttpClientServer()
{
  echo "   Testing HTTP Client/Server with protocol $1 and transport $2 $3";
  RET=0
  node ${DIR}/http_server.js -p $1 -t $2 $3 &
  SERVERPID=$!
  sleep 1
  node ${DIR}/http_client.js -p $1 -t $2 $3 || RET=1
  kill -9 $SERVERPID || RET=1
  return $RET
}


TESTOK=0

#generating thrift code

${DIR}/../../../compiler/cpp/thrift -o ${DIR} --gen js:node ${DIR}/../../../test/ThriftTest.thrift

#unit tests

node ${DIR}/binary.test.js || TESTOK=1

#integration tests

#TCP connection tests
testClientServer compact buffered || TESTOK=1
testClientServer compact framed || TESTOK=1
testClientServer binary buffered || TESTOK=1
testClientServer json buffered || TESTOK=1
testClientServer binary framed || TESTOK=1
testClientServer json framed || TESTOK=1

#tests for multiplexed services
testMultiplexedClientServer binary buffered || TESTOK=1
testMultiplexedClientServer json buffered || TESTOK=1
testMultiplexedClientServer binary framed || TESTOK=1
testMultiplexedClientServer compact framed || TESTOK=1

#test ssl connection
testClientServer binary framed --ssl || TESTOK=1
testMultiplexedClientServer binary framed --ssl || TESTOK=1

#test promise style
testClientServer binary framed --promise || TESTOK=1
testClientServer compact buffered --promise || TESTOK=1

#HTTP tests
testHttpClientServer compact buffered || TESTOK=1
testHttpClientServer compact framed || TESTOK=1
testHttpClientServer json buffered || TESTOK=1
testHttpClientServer json framed || TESTOK=1
testHttpClientServer binary buffered || TESTOK=1
testHttpClientServer binary framed || TESTOK=1
testHttpClientServer json buffered --promise || TESTOK=1
testHttpClientServer binary framed --ssl || TESTOK=1

exit $TESTOK
