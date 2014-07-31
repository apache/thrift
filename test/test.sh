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

START_TIME=$SECONDS
cd "$( dirname "$0" )"
BASEDIR=$(pwd)

print_header() {
  printf "%-16s %-13s %-17s %-s\n" "client-server:" "protocol:" "transport:" "result:"
}

STATUS_HTML="status.html"

print_html_header() {
cat << EOF > $STATUS_HTML
<!DOCTYPE HTML>
<html>
<head>
<meta charset="utf-8">
<title>Apache Thrift - integration test suite</title>
<link rel="stylesheet" type="text/css" href="http://cdn.datatables.net/1.10.0/css/jquery.dataTables.css">
<script type="text/javascript" charset="utf-8" src="http://code.jquery.com/jquery-1.10.2.min.js"></script>
<script type="text/javascript" charset="utf-8" src="http://cdn.datatables.net/1.10.0/js/jquery.dataTables.js"></script>
<script>
  var test_data;
  \$(document).ready( function () {
    testTable = \$('#test_results').DataTable( {
      data: test_data
    });
  \$('#test_results_filter label input')
    .focus()
    .val('failure');
  });
</script>
</head>
<body>
<h2>Apache Thrift - integration test suite: Results</h2>
<table id="test_results" class="display">
    <thead>
        <tr>
            <th>Server</th>
            <th>Client</th>
            <th>Protocol</th>
            <th>Transport</th>
            <th>Result (log)</th>
        </tr>
    </thead>
</table>
<script>
test_data = [
EOF
}

print_html_footer() {
duration=$1
cat << EOF >> $STATUS_HTML
]
</script>
<h2>Test Information</h2>
<pre>
Test Date:     `date --iso-8601=seconds`
Revision:      `git rev-parse --short HEAD`
OS:            `uname -a`
Test duration: $duration
</pre>
</body>
</html>
EOF
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
    client_timeout=$6
    server_startup_time=$7
    
    testname=${client_server}_${protocol}_${transport}
    server_timeout=$(echo "(${server_startup_time}+${client_timeout})" | bc)
    printf "%-16s %-13s %-17s" ${client_server} ${protocol} ${transport}
    
    timeout $server_timeout $server_exec > log/${testname}_server.log 2>&1 &
    server_pid=$!
    
    sleep $server_startup_time
    timeout $client_timeout $client_exec > log/${testname}_client.log 2>&1

    if [ "$?" -eq "0" ]; then
      result="success($?)"
      echo " $result"
    else
      result="failure($?)"
      echo " $result"
      # add details to the error.log
      print_header >> log/error.log
      printf "%-16s %-11s %-17s\n" ${client_server} ${protocol} ${transport} >> log/error.log
      echo "=================== server message ===================" >> log/error.log
      tail log/${testname}_server.log  >> log/error.log
      echo "=================== client message ===================" >> log/error.log
      tail log/${testname}_client.log >> log/error.log
      echo "======================================================" >> log/error.log
      echo "" >> log/error.log
    fi

    # split client-server string
    client=${client_server%-*}
    server=${client_server#*-}

    cat << EOF >> $STATUS_HTML
      [
        "${server}",
        "${client}",
        "${protocol}",
        "${transport}",
        "${result} (<a href=\"log/${testname}_client.log\">client</a>, <a href=\"log/${testname}_server.log\">server</a>)"
      ],
EOF

    # silently kill server
    kill ${server_pid} 2>/dev/null && wait ${server_pid} 2>/dev/null
}

echo "Apache Thrift - integration test suite"
date


echo "======================================================"

rm -rf log
mkdir -p log


print_header
print_html_header

ant -f ../lib/java/build.xml compile-test 1>/dev/null


#TODO add enum for parameters
#TODO align program arguments across languages

cpp_protocols="binary compact json"
java_protocols="binary compact json"
cpp_transports="buffered framed http"
java_server_transports="buffered framed fastframed"
java_client_transports=${java_server_transports}" http"
cpp_sockets="ip domain ip-ssl"
java_sockets="ip ip-ssl"
# TODO fastframed java transport is another implementation of framed transport

nodejs_protocols="binary compact json"
nodejs_transports="buffered framed"
nodejs_sockets="ip ip-ssl"

csharp_protocols="binary compact json"
csharp_transports="buffered framed"
csharp_sockets="ip ip-ssl"

py_protocols="binary compact json accel"
py_transports="buffered framed"
py_sockets="ip ip-ssl"

ruby_protocols="binary compact json accel"
ruby_transports="buffered framed"
ruby_sockets="ip"


######### java client - java server #############
for proto in $java_protocols; do
  for trans in $java_server_transports; do
    for sock in $java_sockets; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-java" "${proto}" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testclient" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testserver" \
              "5" "1"
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
              "2" "0.1"
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
      do_test "java-cpp" "${proto}" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testclient" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}"\
              "5" "0.1"
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
      do_test "cpp-java" "${proto}" "${trans}-${sock}" \
              "cpp/TestClient --protocol=${proto} --transport=${trans} ${extraparam}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans}  ${extraparam}\" run-testserver" \
              "5" "1"
    done
  done
done


NODE_TEST_DIR=${BASEDIR}/../lib/nodejs/test
export NODE_PATH=${NODE_TEST_DIR}:${NODE_TEST_DIR}/../lib:${NODE_PATH}
######### nodejs client - nodejs server ##############
for proto in ${nodejs_protocols}; do
  for trans in ${nodejs_transports}; do
    for sock in ${nodejs_sockets}; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-nodejs" "${proto}" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p ${proto} -t ${trans} ${extraparam}" \
              "node ${NODE_TEST_DIR}/server.js -p ${proto} -t ${trans} ${extraparam}" \
              "5" "0.2"
    done
  done
done

######### nodejs client - cpp server ##############
for proto in $(intersection "${nodejs_protocols}" "${cpp_protocols}"); do
  for trans in $(intersection "${nodejs_transports}" "${cpp_transports}"); do
    for sock in $(intersection "${nodejs_sockets}" "${cpp_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-cpp" "${proto}" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p ${proto} -t ${trans} ${extraparam}" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}" \
              "5" "0.2"
    done
  done
done

######### cpp client - nodejs server ##############
for proto in $(intersection "${nodejs_protocols}" "${cpp_protocols}"); do
  for trans in $(intersection "${nodejs_transports}" "${cpp_transports}"); do
    for sock in $(intersection "${nodejs_sockets}" "${cpp_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "cpp-nodejs" "${proto}" "${trans}-${sock}" \
              "cpp/TestClient --protocol=${proto} --transport=${trans} ${extraparam}" \
              "node ${NODE_TEST_DIR}/server.js -p ${proto} -t ${trans} ${extraparam}" \
              "5" "2"
    done
  done
done

######### nodejs client - java server ##############
for proto in $(intersection "${nodejs_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${nodejs_transports}" "${java_server_transports}"); do
    for sock in $(intersection "${nodejs_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-java" "${proto}" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p ${proto} -t ${trans} ${extraparam}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testserver" \
              "5" "1"
    done
  done
done

######### java client - nodejs server ##############
for proto in $(intersection "${nodejs_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${nodejs_transports}" "${java_client_transports}"); do
    for sock in $(intersection "${nodejs_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-nodejs" "${proto}" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testclient" \
              "node ${NODE_TEST_DIR}/server.js -p ${proto} -t ${trans} ${extraparam}" \
              "5" "2"
    done
  done
done

######### py client - py server ##############
for proto in ${py_protocols}; do
  for trans in ${py_transports}; do
    for sock in ${py_sockets}; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-py" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=${proto} --transport={trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "py/TestServer.py --protocol=${proto} --transport={trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
    done
  done
done

for trans in ${py_transports}; do
    for sock in ${py_sockets}; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-py" "accel-binary" "${trans}-${sock}" \
              "py/TestClient.py --protocol=accel --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "py/TestServer.py --protocol=binary --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
      do_test "py-py" "binary-accel" "${trans}-${sock}" \
              "py/TestClient.py --protocol=binary --transport={trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "py/TestServer.py --protocol=accel --transport={trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
    done
  done

######### py client - cpp server ##############
for proto in $(intersection "${cpp_protocols}" "${py_protocols}"); do
  for trans in $(intersection "${cpp_transports}" "${py_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${py_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-cpp" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=${proto} --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}" \
              "10" "2"
    done
  done
done

for trans in $(intersection "${cpp_transports}" "${py_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${py_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-cpp" "accel-binary" "${trans}-${sock}" \
              "py/TestClient.py --protocol=accel --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "cpp/TestServer --protocol=binary --transport=${trans} ${extraparam}" \
              "10" "2"
    done
  done

######### cpp client - py server ##############
for proto in $(intersection "${cpp_protocols}" "${py_protocols}"); do
  for trans in $(intersection "${cpp_transports}" "${py_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${py_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "cpp-py" "${proto}" "${trans}-${sock}" \
              "cpp/TestClient --protocol=${proto} --transport=${trans} ${extraparam}" \
              "py/TestServer.py --protocol=${proto} --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
    done
  done
done

for trans in $(intersection "${cpp_transports}" "${py_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${py_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "cpp-py" "binary-accel" "${trans}-${sock}" \
              "cpp/TestClient --protocol=binary --transport=${trans} ${extraparam}" \
              "py/TestServer.py --protocol=accel --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
    done
  done

######### py client - java server ##############
for proto in $(intersection "${py_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${py_transports}" "${java_server_transports}"); do
    for sock in $(intersection "${py_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-java" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=${proto} --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testserver" \
              "15" "2"
    done
  done
done

for trans in $(intersection "${py_transports}" "${java_server_transports}"); do
    for sock in $(intersection "${py_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-java" "accel-binary" "${trans}-${sock}" \
              "py/TestClient.py --protocol=accel --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=binary --transport=${trans} ${extraparam}\" run-testserver" \
              "15" "2"
    done
  done

######### java client - py server ##############
for proto in $(intersection "${py_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${py_transports}" "${java_client_transports}"); do
    for sock in $(intersection "${py_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-py" "${proto}" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testclient" \
              "py/TestServer.py --protocol=${proto} --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "5"
    done
  done
done

for trans in $(intersection "${py_transports}" "${java_client_transports}"); do
    for sock in $(intersection "${py_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-py" "binary-accel" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=binary --transport=${trans} ${extraparam}\" run-testclient" \
              "py/TestServer.py --protocol=accel --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "5"
    done
  done

######### py client - nodejs server ##############
for proto in $(intersection "${py_protocols}" "${nodejs_protocols}"); do
  for trans in $(intersection "${py_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${py_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-nodejs" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=${proto} --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "node ${NODE_TEST_DIR}/server.js -p ${proto} -t ${trans} ${extraparam}" \
              "15" "2"
    done
  done
done

for trans in $(intersection "${py_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${py_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-nodejs" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=accel --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "node ${NODE_TEST_DIR}/server.js -p binary -t ${trans} ${extraparam}" \
              "15" "2"
    done
  done

######### nodejs client - py server ##############
for proto in $(intersection "${py_protocols}" "${nodejs_protocols}"); do
  for trans in $(intersection "${py_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${py_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-py" "${proto}" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p ${proto} -t ${trans} ${extraparam}" \
              "py/TestServer.py --protocol=${proto} --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
    done
  done
done

for trans in $(intersection "${py_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${py_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-py" "binary-accel" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p binary -t ${trans} ${extraparam}" \
              "py/TestServer.py --protocol=accel --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "10" "2"
    done
  done

######### ruby client - ruby server ##############
for proto in ${ruby_protocols}; do
  for trans in ${ruby_transports}; do
    for sock in ${ruby_sockets}; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-ruby" "${proto}" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=${proto} --transport=${trans} --port=9091" \
              "ruby rb/integration/TestServer.rb --protocol=${proto} --transport=${trans} --port=9091" \
              "5" "5"
    done
  done
done

for trans in ${ruby_transports}; do
    for sock in ${ruby_sockets}; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-ruby" "accel-binary" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=accel --transport=${trans} --port=9091" \
              "ruby rb/integration/TestServer.rb --protocol=binary --transport=${trans} --port=9091" \
              "5" "5"
      do_test "ruby-ruby" "binary-accel" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=binary --transport=${trans} --port=9091" \
              "ruby rb/integration/TestServer.rb --protocol=accel --transport=${trans} --port=9091" \
              "5" "5"
    done
  done

######### ruby client - cpp server ##############
for proto in $(intersection "${cpp_protocols}" "${ruby_protocols}"); do
  for trans in $(intersection "${cpp_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-cpp" "${proto}" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=${proto} --transport=${trans}" \
              "cpp/TestServer --protocol=${proto} --transport=${trans} ${extraparam}" \
              "5" "5"
    done
  done
done

for trans in $(intersection "${cpp_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-cpp" "accel-binary" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=accel --transport=${trans}" \
              "cpp/TestServer --protocol=binary --transport=${trans} ${extraparam}" \
              "5" "5"
    done
  done

######### cpp client - ruby server ##############
for proto in $(intersection "${cpp_protocols}" "${ruby_protocols}"); do
  for trans in $(intersection "${cpp_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "cpp-ruby" "${proto}" "${trans}-${sock}" \
              "cpp/TestClient --protocol=${proto} --transport=${trans} ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=${proto} --transport=${trans}" \
              "5" "5"
    done
  done
done

for trans in $(intersection "${cpp_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${cpp_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "cpp-ruby" "binary-accel" "${trans}-${sock}" \
              "cpp/TestClient --protocol=binary --transport=${trans} ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=accel --transport=${trans}" \
              "5" "5"
    done
  done

######### ruby client - java server ##############
for proto in $(intersection "${ruby_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${ruby_transports}" "${java_server_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-java" "${proto}" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=${proto} --transport=${trans}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testserver" \
              "15" "5"
    done
  done
done

for trans in $(intersection "${ruby_transports}" "${java_server_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-java" "accel-binary" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=accel --transport=${trans}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=binary --transport=${trans} ${extraparam}\" run-testserver" \
              "15" "5"
    done
  done

######### java client - ruby server ##############
for proto in $(intersection "${ruby_protocols}" "${java_protocols}"); do
  for trans in $(intersection "${ruby_transports}" "${java_client_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-ruby" "${proto}" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=${proto} --transport=${trans} ${extraparam}\" run-testclient" \
              "ruby rb/integration/TestServer.rb --protocol=${proto} --transport=${trans}" \
              "10" "5"
    done
  done
done

for trans in $(intersection "${ruby_transports}" "${java_client_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${java_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "java-ruby" "binary-accel" "${trans}-${sock}" \
              "ant -f  ../lib/java/build.xml -Dno-gen-thrift=\"\" -Dtestargs \"--protocol=binary --transport=${trans} ${extraparam}\" run-testclient" \
              "ruby rb/integration/TestServer.rb --protocol=accel --transport=${trans}" \
              "10" "5"
    done
  done

######### ruby client - nodejs server ##############
for proto in $(intersection "${ruby_protocols}" "${nodejs_protocols}"); do
  for trans in $(intersection "${ruby_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-nodejs" "${proto}" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=${proto} --transport=${trans}" \
              "node ${NODE_TEST_DIR}/server.js -p ${proto} -t ${trans} ${extraparam}" \
              "5" "2"
    done
  done
done

for trans in $(intersection "${ruby_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-nodejs" "${proto}" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=accel --transport=${trans}" \
              "node ${NODE_TEST_DIR}/server.js -p binary -t ${trans} ${extraparam}" \
              "5" "2"
    done
  done

######### nodejs client - ruby server ##############
for proto in $(intersection "${ruby_protocols}" "${nodejs_protocols}"); do
  for trans in $(intersection "${ruby_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-ruby" "${proto}" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p ${proto} -t ${trans} ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=${proto} --transport=${trans}" \
              "10" "5"
    done
  done
done

for trans in $(intersection "${ruby_transports}" "${nodejs_transports}"); do
    for sock in $(intersection "${ruby_sockets}" "${nodejs_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "nodejs-ruby" "binary-accel" "${trans}-${sock}" \
              "node ${NODE_TEST_DIR}/client.js -p binary -t ${trans} ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=accel --transport=${trans}" \
              "10" "2"
    done
  done

  ######### py client - ruby server ##############
for proto in $(intersection "${py_protocols}" "${ruby_protocols}"); do
  for trans in $(intersection "${py_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${py_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-ruby" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=${proto} --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=${proto} --transport=${trans}" \
              "15" "5"
    done
  done
done

for trans in $(intersection "${py_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${py_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "py-ruby" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=accel --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=binary --transport=${trans}" \
              "15" "5"
      do_test "py-ruby" "${proto}" "${trans}-${sock}" \
              "py/TestClient.py --protocol=binary --transport=${trans} --port=9090 --host=localhost --genpydir=py/gen-py ${extraparam}" \
              "ruby rb/integration/TestServer.rb --protocol=accel --transport=${trans}" \
              "15" "5"
    done
  done

######### ruby client - py server ##############
for proto in $(intersection "${py_protocols}" "${ruby_protocols}"); do
  for trans in $(intersection "${py_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${py_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-py" "${proto}" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=${proto} --transport=${trans}" \
              "py/TestServer.py --protocol=${proto} --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "5" "2"
    done
  done
done

for trans in $(intersection "${py_transports}" "${ruby_transports}"); do
    for sock in $(intersection "${py_sockets}" "${ruby_sockets}"); do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "ruby-py" "binary-accel" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=binary --transport=${trans}" \
              "py/TestServer.py --protocol=accel --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "5" "2"
      do_test "ruby-py" "accel-binary" "${trans}-${sock}" \
              "ruby rb/integration/TestClient.rb --protocol=accel --transport=${trans}" \
              "py/TestServer.py --protocol=binary --transport=${trans} --port=9090 --genpydir=py/gen-py TSimpleServer ${extraparam}" \
              "5" "2"
    done
  done


# delete Unix Domain Socket used by cpp tests
rm -f /tmp/ThriftTest.thrift

######### csharp client - csharp server #############
export MONO_PATH=../lib/csharp
for proto in $csharp_protocols; do
  for trans in $csharp_transports; do
    for sock in $csharp_sockets; do
      case "$sock" in
        "ip" ) extraparam="";;
        "ip-ssl" ) extraparam="--ssl";;
      esac
      do_test "csharp-csharp"   "${proto}" "${trans}-${sock}" \
              "../lib/csharp/test/ThriftTest/TestClientServer.exe client --protocol=${proto} --transport=${trans} ${extraparam}" \
              "../lib/csharp/test/ThriftTest/TestClientServer.exe server --protocol=${proto} --transport=${trans} ${extraparam}" \
              "5" "1"
    done
  done
done


do_test "js-java"   "json"  "http-ip" \
        "" \
        "ant -f  ../lib/js/test/build.xml unittest" \
        "2" "2"
do_test "perl-cpp"  "binary" "buffered-ip" \
        "perl -I perl/gen-perl/ -I../lib/perl/lib/ perl/TestClient.pl" \
        "cpp/TestServer" \
        "10" "2"
do_test "php-cpp"  "binary" "buffered-ip" \
        "make -C php/ client" \
        "cpp/TestServer" \
        "10" "2"

echo " failed tests are logged to test/log/error.log"
echo " full log is here test/log/client_server_protocol_transport_client.log"
echo " full log is here test/log/client_server_protocol_transport_server.log"
echo " or look at file://$BASEDIR/$STATUS_HTML"

ELAPSED_TIME=$(echo "(${SECONDS} - ${START_TIME})" | bc)
DURATION="${ELAPSED_TIME} seconds"
echo "test an took" $DURATION
print_html_footer "$DURATION"

date
cd -
