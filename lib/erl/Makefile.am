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

# directory where erlang library code should live
ERL_LIB_ROOT_DIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)

MODULES = \
	src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

ERLLIBDIR=$(DESTDIR)$(ERL_LIB_ROOT_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)

install: all
	mkdir -p $(ERLLIBDIR) ; \
	mkdir -p $(ERLLIBDIR)/ebin ; \
	mkdir -p $(ERLLIBDIR)/include ; \
	mkdir -p $(ERLLIBDIR)/src ; \
	for i in ebin/*.app*   ; do $(INSTALL) $$i $(ERLLIBDIR)/$$i ; done ; \
	for i in ebin/*.beam   ; do $(INSTALL) $$i $(ERLLIBDIR)/$$i ; done ; \
	for i in include/*.hrl ; do $(INSTALL) $$i $(ERLLIBDIR)/$$i ; done ; \
	for i in src/*.erl     ; do $(INSTALL) $$i $(ERLLIBDIR)/$$i ; done

uninstall:
	rm -rf $(ERLLIBDIR)

check: all

distclean: clean

# Hack to make "make dist" work.
# This should not work, but it appears to.
distdir:

MAINTAINERCLEANFILES = Makefile.in

maintainer-clean:
	test ! -d ebin || rm -rf ebin
