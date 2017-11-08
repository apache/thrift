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

all-local:
	sh ensure-externals.sh

test-build:
	$(SBCL) --script test/make-test-binary.lisp

check-local: test-build
	./run-tests

clean-local:
	$(RM) run-tests quicklisp.lisp quicklisp externals

EXTRA_DIST = \
	README.md \
	READMES \
	binary-protocol.lisp \
	classes.lisp \
	client.lisp \
	conditions.lisp \
	definition-operators.lisp \
	framed-transport.lisp \
	load-locally.lisp \
	package.lisp \
	parameters.lisp \
	protocol.lisp \
	server.lisp \
	symbols.lisp \
	thrift.asd \
	transport.lisp \
	types.lisp \
	vector-protocol.lisp \
	test
