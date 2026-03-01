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

BUILT_SOURCES = gopath

GOBUILDEXTRA = -buildvcs=false

THRIFTCMD = $(THRIFT) -out src/gen --gen go:thrift_import=github.com/apache/thrift/lib/go/thrift,package_prefix=github.com/apache/thrift/test/go/src/gen/$(COMPILER_EXTRAFLAG)
THRIFTTEST = $(top_srcdir)/test/ThriftTest.thrift

precross: bin/testclient bin/testserver

ThriftTest.thrift: $(THRIFTTEST)
	grep -v list.*map.*list.*map $(THRIFTTEST) > ThriftTest.thrift

.PHONY: gopath

# Thrift for GO has problems with complex map keys: THRIFT-2063
gopath: $(THRIFT) ThriftTest.thrift
	mkdir -p src/gen
	$(THRIFTCMD) ThriftTest.thrift
	$(THRIFTCMD) ../StressTest.thrift
	$(THRIFTCMD) ../ConstantsDemo.thrift
	touch gopath

bin/testclient: gopath
	$(GO) build $(GOBUILDEXTRA) -o bin/testclient ./src/bin/testclient

bin/testserver: gopath
	$(GO) build $(GOBUILDEXTRA) -o bin/testserver ./src/bin/testserver

bin/stress: gopath
	$(GO) build $(GOBUILDEXTRA) -o bin/stress ./src/bin/stress

clean-local:
	$(RM) -r src/gen src/github.com/golang src/thrift bin pkg gopath ThriftTest.thrift

check_PROGRAMS: bin/testclient bin/testserver bin/stress

check: gopath genmock
	$(GO) test -v ./src/common/...
	$(GO) test -v ./src/gen/...

genmock: gopath
	sh genmock.sh

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	src/bin \
	src/common \
	go.mod \
	go.sum \
	genmock.sh
