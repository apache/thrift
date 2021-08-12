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
		  ../../test/NameConflictTest.thrift \
		  ../../test/DoubleConstantsTest.thrift \
		  ../../test/ThriftTest.thrift \
		  ../../test/Recursive.thrift

ERL_FLAG = erlang

.generated: $(THRIFT) $(THRIFT_FILES)
	for f in $(THRIFT_FILES) ; do \
		$(THRIFT) --gen $(ERL_FLAG) -o test $$f ; \
	done
	touch .generated

all: .generated
	$(REBAR) compile

check: .generated
	$(REBAR) eunit

install: all
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift) ; \
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/ebin ; \
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/include ; \
	mkdir -p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/src ; \
	for p in ebin/*.app* ebin/*.beam include/*.hrl src/*.erl ; \
		do $(INSTALL) $$p $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)/$$p ; \
	done

uninstall:
	$(RM) -rf $(DESTDIR)$(ERLANG_INSTALL_LIB_DIR_thrift)

clean-local:
	$(REBAR) clean
	$(RM) -r _build/
	$(RM)    .generated
	$(RM) -r test/gen-erlang/

maintainer-clean-local:
	$(RM) -r ebin/

dist-hook:
	$(RM)    $(distdir)/.generated
	$(RM) -r $(distdir)/.rebar/
	$(RM) -r $(distdir)/_build/
	$(RM) -r $(distdir)/test/gen-erl/
	$(RM)    $(distdir)/$(THRIFT_OMIT_FILE)


EXTRA_DIST = \
	include \
	src \
	coding_standards.md \
	rebar.config \
	test \
	README.md

MAINTAINERCLEANFILES = Makefile.in
