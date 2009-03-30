GENDIR=gen
GEN_INCLUDEDIR=$(GENDIR)/include
GEN_SRCDIR=$(GENDIR)/src
GEN_TARGETDIR=$(GENDIR)/ebin

INCLUDEDIR=include
TARGETDIR=ebin
SRCDIR=src

ALL_INCLUDEDIR=$(GEN_INCLUDEDIR) $(INCLUDEDIR) ../../lib/erl/include
INCLUDEFLAGS=$(patsubst %,-I%, ${ALL_INCLUDEDIR})

MODULES = stress_server test_server test_disklog test_membuffer

INCLUDES = 
TARGETS = $(patsubst %,${TARGETDIR}/%.beam,${MODULES})
HEADERS = $(patsubst %,${INCLUDEDIR}/%.hrl,${INCLUDES})

all: ${GEN_TARGETDIR}/ ${TARGETS}

TEST_RPCFILE = ../ThriftTest.thrift
STRESS_RPCFILE = ../StressTest.thrift
THRIFT = ../../compiler/cpp/thrift

${GENDIR}/: ${RPCFILE}
	rm -rf ${GENDIR}
	${THRIFT} --gen erl ${TEST_RPCFILE}
	${THRIFT} --gen erl ${STRESS_RPCFILE}
	mkdir -p ${GEN_INCLUDEDIR}
	mkdir -p ${GEN_SRCDIR}
	mkdir -p ${GEN_TARGETDIR}
	mv -t ${GEN_INCLUDEDIR} gen-erl/*.hrl
	mv -t ${GEN_SRCDIR} gen-erl/*.erl
	rm -rf gen-erl

${GEN_TARGETDIR}/: ${GENDIR}/
	rm -rf ${GEN_TARGETDIR}
	mkdir -p ${GEN_TARGETDIR}
	erlc ${INCLUDEFLAGS} -o ${GEN_TARGETDIR} ${GEN_SRCDIR}/*.erl

$(TARGETS): ${TARGETDIR}/%.beam: ${SRCDIR}/%.erl ${GEN_INCLUDEDIR}/ ${HEADERS}
	mkdir -p ${TARGETDIR}
	erlc ${INCLUDEFLAGS} -o ${TARGETDIR} $<

clean:
	rm -f ${TARGETDIR}/*.beam
	rm -rf ${GENDIR}
