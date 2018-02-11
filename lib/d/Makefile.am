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

AUTOMAKE_OPTIONS = serial-tests

SUBDIRS = .

if WITH_TESTS
SUBDIRS += test
endif

#
# Enumeration of all the public and private modules.
#
# We unconditionally install all of them, even if libevent or OpenSSL are
# not available, but build the respective libraries only if the Deimos headers
# could be found.
#
d_thriftmodules = $(addprefix thrift/, base)
d_thriftdir = $(D_IMPORT_PREFIX)/thrift
d_thrift_DATA = $(addprefix src/, $(addsuffix .d, $(d_thriftmodules)))

d_asyncmodules = $(addprefix thrift/async/, base libevent socket ssl)
d_asyncdir = $(d_thriftdir)/async
d_async_DATA = $(addprefix src/, $(addsuffix .d, $(d_asyncmodules)))

d_codegenmodules = $(addprefix thrift/codegen/, async_client \
	async_client_pool base client client_pool processor)
#d_codegenmodules = $(addprefix thrift/codegen/, async_client \
#	async_client_pool base client client_pool idlgen processor)

d_codegendir = $(d_thriftdir)/codegen
d_codegen_DATA = $(addprefix src/, $(addsuffix .d, $(d_codegenmodules)))

d_protocolmodules = $(addprefix thrift/protocol/, base binary compact json \
	processor)
d_protocoldir = $(d_thriftdir)/protocol
d_protocol_DATA = $(addprefix src/, $(addsuffix .d, $(d_protocolmodules)))

d_servermodules = $(addprefix thrift/server/, base simple nonblocking \
	taskpool threaded)
d_serverdir = $(d_thriftdir)/server
d_server_DATA = $(addprefix src/, $(addsuffix .d, $(d_servermodules)))

d_servertransportmodules = $(addprefix thrift/server/transport/, base socket ssl)
d_servertransportdir = $(d_thriftdir)/server/transport
d_servertransport_DATA = $(addprefix src/, $(addsuffix .d, \
	$(d_servertransportmodules)))

d_transportmodules = $(addprefix thrift/transport/, base buffered file \
	framed http memory piped range socket ssl zlib)
d_transportdir = $(d_thriftdir)/transport
d_transport_DATA = $(addprefix src/, $(addsuffix .d, $(d_transportmodules)))

d_utilmodules = $(addprefix thrift/util/, awaitable cancellation future \
	hashset)
d_utildir = $(d_thriftdir)/util
d_util_DATA = $(addprefix src/, $(addsuffix .d, $(d_utilmodules)))

d_internalmodules = $(addprefix thrift/internal/, algorithm codegen ctfe \
	endian resource_pool socket ssl ssl_bio traits)
d_internaldir = $(d_thriftdir)/internal
d_internal_DATA = $(addprefix src/, $(addsuffix .d, $(d_internalmodules)))

d_testmodules = $(addprefix thrift/internal/test/, protocol server)
d_testdir = $(d_internaldir)/test
d_test_DATA = $(addprefix src/, $(addsuffix .d, $(d_testmodules)))

d_publicmodules = $(d_thriftmodules) $(d_asyncmodules) \
	$(d_codegenmodules) $(d_protocolmodules) $(d_servermodules) \
	$(d_servertransportmodules) $(d_transportmodules) $(d_utilmodules)
d_publicsources = $(addprefix src/, $(addsuffix .d, $(d_publicmodules)))

d_modules = $(d_publicmodules) $(d_internalmodules) $(d_testmodules)

# List modules with external dependencies and remove them from the main list
d_libevent_dependent_modules = thrift/async/libevent thrift/server/nonblocking
d_openssl_dependent_modules = thrift/async/ssl thrift/internal/ssl \
	thrift/internal/ssl_bio thrift/transport/ssl thrift/server/transport/ssl
d_main_modules = $(filter-out $(d_libevent_dependent_modules) \
	$(d_openssl_dependent_modules),$(d_modules))


all_targets = $(D_LIB_NAME)


#
# Documentation target (requires Dil).
#
docs: $(d_publicsources) src/thrift/index.d
	dil ddoc docs -hl --kandil $^


#
# Hook custom library targets into the automake all/install targets.
#
all-local:
	dub build --config=library --arch=x86_64 --build=debug --compiler=dmd

install-exec-local:
	$(INSTALL_PROGRAM) $(all_targets) $(DESTDIR)$(libdir)

clean-local:
	dub clean
	$(RM) lib*.a

#
# Unit tests (built both in debug and release mode).
#
check-local:
	dub test --config=library --arch=x86_64 --build=unittest --compiler=dmd


precross: all-local
	$(MAKE) -C test precross

EXTRA_DIST = \
	src \
	test \
	README.md
