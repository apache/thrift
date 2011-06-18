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

GENDIR=gen
GEN_INCLUDEDIR=$(GENDIR)/include
GEN_SRCDIR=$(GENDIR)/src
GEN_TARGETDIR=$(GENDIR)/ebin

INCLUDEDIR=include
TARGETDIR=ebin
SRCDIR=src

ALL_INCLUDEDIR=$(GEN_INCLUDEDIR) $(INCLUDEDIR) ../../lib/erl/include
INCLUDEFLAGS=$(patsubst %,-I%, ${ALL_INCLUDEDIR})

MODULES = stress_server test_server test_client test_disklog test_membuffer test_thrift_1151

INCLUDES = 
TARGETS = $(patsubst %,${TARGETDIR}/%.beam,${MODULES})
HEADERS = $(patsubst %,${INCLUDEDIR}/%.hrl,${INCLUDES})

all: ${GEN_TARGETDIR}/ ${TARGETS}

TEST_RPCFILE = ../ThriftTest.thrift
STRESS_RPCFILE = ../StressTest.thrift
THRIFT_1151_FILE = src/Thrift1151.thrift
THRIFT = ../../compiler/cpp/thrift

${GENDIR}/: ${RPCFILE}
	rm -rf ${GENDIR}
	${THRIFT} --gen erl ${TEST_RPCFILE}
	${THRIFT} --gen erl ${STRESS_RPCFILE}
	${THRIFT} --gen erl ${THRIFT_1151_FILE}
	mkdir -p ${GEN_INCLUDEDIR}
	mkdir -p ${GEN_SRCDIR}
	mkdir -p ${GEN_TARGETDIR}
	mv gen-erl/*.hrl ${GEN_INCLUDEDIR}
	mv gen-erl/*.erl ${GEN_SRCDIR}
	rm -rf gen-erl

${GEN_TARGETDIR}/: ${GENDIR}/
	rm -rf ${GEN_TARGETDIR}
	mkdir -p ${GEN_TARGETDIR}
	erlc ${ERLC_FLAGS} ${INCLUDEFLAGS} -o ${GEN_TARGETDIR} ${GEN_SRCDIR}/*.erl

$(TARGETS): ${TARGETDIR}/%.beam: ${SRCDIR}/%.erl ${GEN_INCLUDEDIR}/ ${HEADERS}
	mkdir -p ${TARGETDIR}
	erlc ${ERLC_FLAGS} ${INCLUDEFLAGS} -o ${TARGETDIR} $<

clean:
	rm -f ${TARGETDIR}/*.beam
	rm -rf ${GENDIR}
