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

gen-zig/tutorial.zig gen-zig/shared.zig: $(top_srcdir)/tutorial/tutorial.thrift $(THRIFT)
	mkdir -p gen-zig
	$(THRIFT) -out gen-zig --gen zig -r $<

style-local:
	$(ZIG) fmt --check src
	$(ZIG) fmt --check build.zig

fetch-lib: ../../lib/zig/
	cp -r $(top_srcdir)/lib/zig/. lib/

all-local: fetch-lib gen-zig/tutorial.zig
	zig build -freference-trace

check: all

tutorialserver: all
	zig-out/bin/tutorial_server

tutorialclient: all
	zig-out/bin/tutorial_client

clean-local:
	$(RM) -r .zig-cache
	$(RM) -r zig-out
	$(RM) -r gen-zig/*
	$(RM) -r lib

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	build.zig \
	build.zig.zon \
	src/tutorial_server.zig \
	src/tutorial_client.zig

