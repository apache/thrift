#! /bin/sh

DIR="$( cd "$( dirname "$0" )" && pwd )"

mkdir -p $DIR/../test-compiled

COMPILEDDIR="$(cd $DIR && cd ../test-compiled && pwd)"
RUNBROWSER="$DIR/../../../node_modules/run-browser/bin/cli.js"
export NODE_PATH="${DIR}:${DIR}/../lib:${NODE_PATH}"

compile()
{
  #generating thrift code
  ${DIR}/../../../compiler/cpp/thrift -o ${DIR} --gen ts:node ${DIR}/../../../test/ThriftTest.thrift
  #generationg js code
  CODES="\
    $DIR/client.ts \
    $DIR/server.ts \
    $DIR/test_driver.ts \
    $DIR/test_handler.ts \
    $DIR/typings/index.d.ts \
    $DIR/gen-nodets/ThriftTest.ts \
    $DIR/gen-nodets/ThriftTest_types.ts"
  tsc -m commonjs -t ES5 --outDir $COMPILEDDIR $CODES
}
compile

testServer()
{
  echo "start server"
  RET=0
  node ${COMPILEDDIR}/server.js $1 &
  SERVERPID=$!
  sleep 1
  echo "start client"
  node ${COMPILEDDIR}/client.js $1 || RET=1
  kill -2 $SERVERPID || RET=1
  return $RET
}

#integration tests

testServer || TESTOK=1
testServer --promise || TESTOK=1

exit $TESTOK
