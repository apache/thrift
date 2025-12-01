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

SUBDIRS = .

all-local:
	swift build --configuration release

install-exec-hook:
	swift install

clean-local:
	swift package clean
	rm -rf .build
	rm -rf FuzzTesting/.build
	rm -rf FuzzTesting/Sources/Fuzz/*

precross:
	swift build

check-local:
	swift test

FuzzTesting/Sources/Fuzz:
	mkdir -p FuzzTesting/Sources/Fuzz

fuzz-gen: FuzzTesting/Sources/Fuzz
	$(top_builddir)/compiler/cpp/thrift --gen swift -r -out FuzzTesting/Sources/Fuzz $(top_srcdir)/test/FuzzTest.thrift

fuzz-local: fuzz-gen
	cd FuzzTesting && swift build --configuration release ${SWIFTFLAGS}

fuzz: all-local fuzz-local
	@echo "Built fuzzers successfully"

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	Package.swift \
	Sources \
	Tests \
	FuzzTesting \
	README.md

MAINTAINERCLEANFILES = \
	Makefile \
	Makefile.in
