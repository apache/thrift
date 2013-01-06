#!/bin/sh
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

# Apache Thrift - integration test suite
#
# tests different client-server, protocol and transport combinations

# related issues:
# THRIFT-847 Test Framework harmonization across all languages
# THRIFT-819 add Enumeration for protocol, transport and server types

BASEDIR=$(dirname $0)
cd $BASEDIR

print_header() {
  printf "%-16s %-11s %-17s %-s\n" "client-server:" "protocol:" "transport:" "result:"
}

do_test () {
    client_server=$1
    protocol=$2
    transport=$3
    client_exec=$4
    server_exec=$5
    server_startup_time=$6
    client_delay=$7
    
    testname=${client_server}_${protocol}_${transport}
    server_timeout=$((${server_startup_time}+${client_delay}))
    printf "%-16s %-11s %-17s" ${client_server} ${protocol} ${transport} 
    timeout $server_timeout $server_exec > log/${testname}_server.log 2>&1 &
    sleep $server_startup_time
    $client_exec > log/${testname}_client.log 2>&1
    
    if [ "$?" -eq "0" ]; then
      echo " success"
    else
      echo " failure"
      echo "=================== server message ==================="
      tail log/${testname}_server.log 
      echo "=================== client message ==================="
      tail log/${testname}_client.log
      echo "======================================================"
      echo ""
      print_header
    fi
    sleep ${client_delay}
}

echo "Apache Thrift - integration test suite"
date
echo "======================================================"

rm -rf log
mkdir -p log

print_header

#TODO add enum for parameters
#TODO align program arguments across languages

protocols="binary json"
transports="buffered framed http"
sockets="ip domain"
# we need a test certificate first
#sockets="ip ip-ssl domain"


for proto in $protocols; do
  for trans in $transports; do
    for sock in $sockets; do
      case "$sock" in
       "ip" )     extraparam="";;
       "ip-ssl" ) extraparam="--ssl";;
       "domain" ) extraparam="--domain-socket=/tmp/ThriftTest.thrift";;
      esac
      do_test "cpp-cpp"   "${proto}" "${trans}-${sock}" \
              "cpp/TestClient --protocol=${proto} --transport=${trans} ${extraparam}" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}" \
              "10" "5"
    done;
  done;
done;

# delete Unix Domain Socket used by cpp tests
rm -f /tmp/ThriftTest.thrift

do_test "py-py" "binary" "buffered-ip" \
        "py/TestClient.py --proto=binary --port=9090 --host=localhost --genpydir=py/gen-py" \
        "py/TestServer.py --proto=binary --port=9090 --genpydir=py/gen-py TSimpleServer" \
        "10" "10"
do_test "py-py" "json" "buffered-ip" \
        "py/TestClient.py --proto=json --port=9090 --host=localhost --genpydir=py/gen-py" \
        "py/TestServer.py --proto=json --port=9090 --genpydir=py/gen-py TSimpleServer" \
        "10" "10"
do_test "py-cpp" "binary" "buffered-ip" \
        "py/TestClient.py --proto=binary --port=9090 --host=localhost --genpydir=py/gen-py" \
        "cpp/TestServer" \
        "10" "10"
do_test "py-cpp" "json" "buffered-ip" \
        "py/TestClient.py --proto=json --port=9090 --host=localhost --genpydir=py/gen-py" \
        "cpp/TestServer --protocol=json" \
        "10" "10"
do_test "cpp-py" "binary" "buffered-ip" \
        "cpp/TestClient --protocol=binary --port=9090" \
        "py/TestServer.py --proto=binary --port=9090 --genpydir=py/gen-py TSimpleServer" \
        "10" "10"
do_test "cpp-py" "json" "buffered-ip" \
        "cpp/TestClient --protocol=json --port=9090" \
        "py/TestServer.py --proto=json --port=9090 --genpydir=py/gen-py TSimpleServer" \
        "10" "10"
do_test "py-java"  "binary" "buffered-ip" \
        "py/TestClient.py --proto=binary --port=9090 --host=localhost --genpydir=py/gen-py" \
        "ant -f  ../lib/java/build.xml testserver" \
        "120" "10"
do_test "py-java"  "json"   "buffered-ip" \
        "py/TestClient.py --proto=json --port=9090 --host=localhost --genpydir=py/gen-py" \
        "ant -f  ../lib/java/build.xml testserver" \
        "120" "10"
do_test "java-py"  "binary" "buffered-ip" \
        "ant -f  ../lib/java/build.xml testclient" \
        "py/TestServer.py --proto=binary --port=9090 --genpydir=py/gen-py TSimpleServer" \
        "10" "35"
do_test "java-java" "binary" "buffered-ip" \
        "ant -f  ../lib/java/build.xml testclient" \
        "ant -f  ../lib/java/build.xml testserver" \
        "120" "35"
do_test "cpp-java"  "binary" "buffered-ip" \
        "cpp/TestClient" \
        "ant -f  ../lib/java/build.xml testserver" \
        "100" "10"
do_test "cpp-java"  "json"   "buffered-ip" \
        "cpp/TestClient" \
        "ant -f  ../lib/java/build.xml testserver" \
        "100" "10"
do_test "js-java"   "json "  "http-ip" \
        "" \
        "ant -f  ../lib/js/test/build.xml unittest" \
        "120" "10"
do_test "java-cpp"  "binary" "buffered-ip" \
        "ant -f  ../lib/java/build.xml testclient" \
        "cpp/TestServer" \
        "10" "35"
do_test "perl-cpp"  "binary" "buffered-ip" \
        "perl -I perl/gen-perl/ -I../lib/perl/lib/ perl/TestClient.pl" \
        "cpp/TestServer" \
        "10" "10"
do_test "php-cpp"  "binary" "buffered-ip" \
        "make -C php/ client" \
        "cpp/TestServer" \
        "10" "10"
do_test "nodejs-nodejs" "binary" "framed-ip" \
        "make -C nodejs/ client" \
        "make -C nodejs/ server" \
        "1" "10"
do_test "nodejs-cpp" "binary" "framed-ip" \
        "make -C nodejs/ client" \
        "cpp/TestServer --transport=framed" \
        "1" "10"
do_test "cpp-nodejs" "binary" "framed-ip" \
        "cpp/TestClient --transport=framed" \
        "make -C nodejs/ server" \
        "1" "10"

cd -
