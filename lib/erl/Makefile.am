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

THRIFT = ../../compiler/cpp/thrift
THRIFT_FILES = $(wildcard test/*.thrift) \
		  ../../test/ThriftTest.thrift

if ERLANG_OTP16
.generated: $(THRIFT) $(THRIFT_FILES)
	for f in $(THRIFT_FILES) ; do \
		$(THRIFT) --gen erl:otp16 -o test $$f ; \
	done ;
else
.generated: $(THRIFT) $(THRIFT_FILES)
	for f in $(THRIFT_FILES) ; do \
		$(THRIFT) --gen erl -o test $$f ; \
	done ; \
	$(THRIFT) --gen erl:maps -o test test/Thrift3214.thrift ; \
	touch .generated
endif

all: .generated
	./rebar get-deps
	./rebar compile

check: .generated
	./rebar skip_deps=true eunit

install: all
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift) ; \
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/ebin ; \
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/include ; \
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/src ; \
	for p in ebin/*.app* ebin/*.beam include/*.hrl src/*.erl ; \
		do $(INSTALL) $$p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/$$p ; \
	done

uninstall:
	rm -rf $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)

clean:
	rm -f .generated
	rm -rf test/gen-erl/
	./rebar clean

maintainer-clean-local:
	rm -rf ebin

EXTRA_DIST = \
	include \
	src \
	coding_standards.md \
	rebar \
	rebar.config \
	test \
	README.md

MAINTAINERCLEANFILES = Makefile.in
