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

THRIFT_DIR =../..
INCS_DIRS  =-I${THRIFT_DIR}/lib/cpp/src -I.
THRIFT_LIB =${THRIFT_DIR}/lib/cpp/.libs/libthrift.a
BOOST_LIB  =
CPP_DEFS   =
CPP_OPTS   =-Wall -O2

GEN_SRC    = gen-cpp/SampleService.cpp  \
             gen-cpp/Sample_types.cpp   \
             gen-cpp/SampleCallback.cpp \
             gen-cpp/Sample_constants.cpp
GEN_INC    = -Igen-cpp

default: server client
.PHONY: server client

server:
	g++ ${CPP_OPTS} ${CPP_DEFS} -o server/server ${GEN_INC} ${INCS_DIRS} server/server.cpp ${GEN_SRC} ${THRIFT_LIB} ${BOOST_LIB}

client:
	g++ ${CPP_OPTS} ${CPP_DEFS} -o client/client ${GEN_INC} ${INCS_DIRS} client/client.cpp ${GEN_SRC} ${THRIFT_LIB} ${BOOST_LIB}

clean:
	$(RM) -r client/client server/server

