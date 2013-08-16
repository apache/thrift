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

THRIFT = $(top_srcdir)/compiler/cpp/thrift
THRIFTTEST = $(top_srcdir)/test/ThriftTest.thrift

# Thrift for GO has problems with complex map keys: THRIFT-2063
gopath: $(THRIFT) $(THRIFTTEST) IncludesTest.thrift NamespacedTest.thrift
	mkdir -p gopath/src
	grep -v list.*map.*list.*map $(THRIFTTEST) > ThriftTest.thrift
	$(THRIFT) --gen go:thrift_import=thrift -r IncludesTest.thrift
	ln -nfs ../../gen-go/ThriftTest gopath/src/ThriftTest
	ln -nfs ../../gen-go/IncludesTest gopath/src/IncludesTest
	ln -nfs ../../gen-go/lib gopath/src/lib
	ln -nfs ../../../thrift gopath/src/thrift
	touch gopath

check: gopath
	GOPATH=`pwd`/gopath $(GO) build IncludesTest

clean-local:
	$(RM) -r gen-go gopath

client: stubs
	$(GO) run TestClient.go

EXTRA_DIST = \
	IncludesTest.thrift \
	NamespacedTest.thrift
