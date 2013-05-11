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

intersection() {
  return_value=""
  for one in $1; do
    for two in $2; do
      if [ ${one} = ${two} ]; then
        return_value=${return_value}" "${one}
      fi
    done;
  done;
  echo ${return_value};
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

cpp_protocols="binary json"
java_protocols="binary json compact"
cpp_transports="buffered framed http"
java_server_transports="buffered framed fastframed"
java_client_transports=${java_server_transports}" http"
# we need a test certificate first
cpp_sockets="ip domain"
java_sockets="ip ip-ssl"
# TODO fastframed java transport is another implementation of framed transport


ant -f ../lib/java/build.xml compile-test 1>/dev/null

######### java client - java server #############
for proto in $java_protocols; do
  for trans in $java_server_transports; do
    for sock in $java_sockets; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-java" "${proto}" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" testclient" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" testserver" \
            "15" "15"
    done
  done
done

######### cpp client - cpp server ###############
for proto in $cpp_protocols; do
  for trans in $cpp_transports; do
    for sock in $cpp_sockets; do
      case "$sock" in
       "ip" )     extraparam="";;
       "ip-ssl" ) extraparam="--ssl";;
       "domain" ) extraparam="--domain-socket=/tmp/ThriftTest.thrift";;
      esac
      do_test "cpp-cpp"   "${proto}" "${trans}-${sock}" \
              "cpp/TestClient --protocol=${proto} --transport=${trans} ${extraparam}" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}" \
              "10" "10"
    done
  done
done

######### java client - cpp server ##############
# warning: ssl over http is not supported in java client!
for proto in $(intersection "${java_protocols}" "${cpp_protocols}"); do
  for trans in $(intersection "${java_client_transports}" "${cpp_transports}"); do
    for sock in $(intersection "${java_sockets}" "${cpp_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-cpp" "${proto}" "${trans}-ip" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" testclient" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}"\
              "10" "15"
    done
  done
done

######### cpp client - java server ##############
for proto in $(intersection "${cpp_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${cpp_transports}" "${java_server_transports}"); do
    for sock in $(intersection "${java_sockets}" "${cpp_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "cpp-java" "${proto}" "${trans}-ip" \
              "cpp/TestClient --protocol=${proto} --transport=${trans}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans}\" testserver" \
              "15" "10"
    done
  done
done

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
        "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" testserver" \
        "15" "15"
do_test "py-java"  "json"   "json-ip" \
        "py/TestClient.py --proto=json --port=9090 --host=localhost --genpydir=py/gen-py" \
        "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=json\" testserver" \
        "15" "10"
do_test "java-py"  "binary" "buffered-ip" \
        "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" testclient" \
        "py/TestServer.py --proto=binary --port=9090 --genpydir=py/gen-py TSimpleServer" \
        "10" "15"
do_test "js-java"   "json "  "http-ip" \
        "" \
        "ant -f  ../lib/js/test/build.xml unittest" \
        "10" "15"
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
        "1" "5"
do_test "nodejs-cpp" "binary" "framed-ip" \
        "make -C nodejs/ client" \
        "cpp/TestServer --transport=framed" \
        "1" "10"
do_test "cpp-nodejs" "binary" "framed-ip" \
        "cpp/TestClient --transport=framed" \
        "make -C nodejs/ server" \
        "1" "5"

cd -
