#!/bin/bash 

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

LOG4J="../../java/build/ivy/lib/slf4j-api-1.5.8.jar:../../java/build/ivy/lib/log4j-1.2.15.jar:../../java/build/ivy/lib/slf4j-simple-1.5.8.jar"
HTTPCORE="./httpcore-4.0.1.jar"

if [ -f ${HTTPCORE} ]
then
    echo "compiling test..."
else
    echo "Missing required file ${HTTPCORE}"
    echo "You can download this from http://archive.apache.org/dist/httpcomponents/httpcore/binary/httpcomponents-core-4.0.1-bin.tar.gz"
    echo "Place the jar in this directory and try again."
    exit
fi

../../../compiler/cpp/thrift --gen java ../../../test/ThriftTest.thrift
../../../compiler/cpp/thrift --gen js ../../../test/ThriftTest.thrift

javac -cp ${LOG4J}:../../java/libthrift.jar gen-java/thrift/test/*.java
javac -cp ${LOG4J}:${HTTPCORE}:../../java/libthrift.jar:gen-java/ src/test/*.java 
java -cp  ${LOG4J}:${HTTPCORE}:../../java/libthrift.jar:gen-java:src test.Httpd ../
