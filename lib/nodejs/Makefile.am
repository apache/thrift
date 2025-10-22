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

# We call npm twice to work around npm issues

stubs: $(top_srcdir)/test/v0.16/ThriftTest.thrift $(top_srcdir)/test/v0.16/FuzzTestNoUuid.thrift
	$(THRIFT) --gen js:node -o test/ $(top_srcdir)/test/v0.16/ThriftTest.thrift
	$(THRIFT) --gen js:node -o test/fuzz/ $(top_srcdir)/test/v0.16/FuzzTestNoUuid.thrift
	sed -i "s/require('thrift')/require(\"..\/..\/..\/lib\/thrift\")/" test/fuzz/gen-nodejs/FuzzTestNoUuid_types.js


deps-root: $(top_srcdir)/package.json
	$(NPM) install $(top_srcdir)/ || $(NPM) install $(top_srcdir)/

deps-test: test/package.json test/package-lock.json
	cd test/ && $(NPM) install && cd ..

deps: deps-root deps-test

all-local: deps

precross: deps stubs

# TODO: Lint nodejs lib and gen-code as part of build
check: deps
	cd $(top_srcdir) && $(NPM) test && $(NPM) run lint-tests && cd lib/nodejs

clean-local:
	$(RM) -r test/gen-*
	$(RM) -r test/fuzz/gen-*
	$(RM) -r $(top_srcdir)/node_modules
	$(RM) -r test/episodic-code-generation-test/gen*
	$(RM) -r test/episodic-code-generation-test/node_modules

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	examples \
	lib \
	test \
	coding_standards.md \
	CMakeLists.txt \
	README.md
