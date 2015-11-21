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

THRIFT = $(top_builddir)/compiler/cpp/thrift

gensrcs = \
	$(top_srcdir)/test/ThriftTest.thrift \
	$(srcdir)/src/tests/if/hs_test.thrift \
	$(srcdir)/src/tests/if/IncludeTest.thrift \
	$(srcdir)/src/tests/if/IncludedTest.thrift

stubs = \
	Hs_test_Consts.hs \
	Hs_test_Types.hs \
	IncludeTest_Consts.hs \
	IncludeTest_Types.hs \
	ThriftTest.hs \
	ThriftTest_Client.hs \
	ThriftTest_Consts.hs \
	ThriftTest_Iface.hs \
	ThriftTest_Types.hs

$(stubs): $(gensrcs) $(THRIFT)
	$(THRIFT) --gen hs $(top_srcdir)/test/ThriftTest.thrift
	$(THRIFT) --gen hs $(srcdir)/src/tests/if/hs_test.thrift
	$(THRIFT) --gen hs $(srcdir)/src/tests/if/IncludeTest.thrift
	$(THRIFT) --gen hs $(srcdir)/src/tests/if/IncludedTest.thrift

EXTRA_DIST = \
	coding_standards.md \
	LICENSE \
	README.md \
	Setup.lhs \
	TODO \
	Thrift.cabal \
	src

all-local:
	$(CABAL) update
	$(CABAL) install

install-exec-hook:
	$(CABAL) install

# Make sure this doesn't fail if Haskell is not configured.
clean-local:
	$(CABAL) clean

maintainer-clean-local:
	$(CABAL) clean

check-local: $(stubs)
	$(CABAL) check
	$(srcdir)/run-test.sh BinaryTests
	$(srcdir)/run-test.sh CompactTests
	$(srcdir)/run-test.sh IncludeTest
	$(srcdir)/run-test.sh JSONTests
	$(srcdir)/run-test.sh PrettyJSONTests
	$(srcdir)/run-test.sh SimpleJSONTests
