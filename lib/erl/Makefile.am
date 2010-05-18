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

MODULES = \
	src

all clean docs:
	for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
	done

install: all
	mkdir -p $(ERLANG_INSTALL_LIB_DIR_thrift) ; \
	mkdir -p $(ERLANG_INSTALL_LIB_DIR_thrift)/ebin ; \
	mkdir -p $(ERLANG_INSTALL_LIB_DIR_thrift)/include ; \
	mkdir -p $(ERLANG_INSTALL_LIB_DIR_thrift)/src ; \
	for i in ebin/*.app* ebin/*.beam include/*.hrl src/*.erl ; \
		do $(INSTALL) $$p $(ERLANG_INSTALL_LIB_DIR_thrift)/$$p ; \
	done

uninstall:
	rm -rf $(ERLANG_INSTALL_LIB_DIR_thrift)


maintainer-clean-local:
	rm -rf ebin

MAINTAINERCLEANFILES = Makefile.in
