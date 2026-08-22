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
THRIFT_TEST = $(top_srcdir)/test/ThriftTest.thrift
OPTIONAL_REQUIRED_TEST = $(top_srcdir)/test/OptionalRequiredTest.thrift
FUZZ_TEST = $(top_srcdir)/test/v0.16/FuzzTestNoUuid.thrift
CONSTANTS_DEMO_TEST = $(top_srcdir)/test/ConstantsDemo.thrift

stubs: $(THRIFT) $(THRIFT_TEST) $(OPTIONAL_REQUIRED_TEST) $(FUZZ_TEST) $(CONSTANTS_DEMO_TEST)
	$(MKDIR) -p gen-zig
	$(THRIFT) -out gen-zig --gen zig $(THRIFT_TEST)
	$(THRIFT) -out gen-zig --gen zig $(OPTIONAL_REQUIRED_TEST)
	$(THRIFT) -out gen-zig --gen zig $(FUZZ_TEST)
	$(THRIFT) -out gen-zig --gen zig $(CONSTANTS_DEMO_TEST)

style-local:
	$(ZIG) fmt --check src
	$(ZIG) fmt --check build.zig

check-local: stubs
	$(ZIG) build test --summary all

clean-local:
	$(RM) -r .zig-cache
	$(RM) -r gen-zig/*

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	build.zig \
	src
