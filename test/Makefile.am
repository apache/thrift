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

SUBDIRS = features
PRECROSS_TARGET =

if WITH_C_GLIB
SUBDIRS += c_glib
PRECROSS_TARGET += precross-c_glib
endif

if WITH_CL
SUBDIRS += cl
PRECROSS_TARGET += precross-cl
endif

if WITH_CPP
SUBDIRS += cpp
PRECROSS_TARGET += precross-cpp
endif

if WITH_PERL
SUBDIRS += perl
PRECROSS_TARGET += precross-perl
endif

if WITH_PHP
SUBDIRS += php
PRECROSS_TARGET += precross-php
endif

if WITH_DART
SUBDIRS += dart
PRECROSS_TARGET += precross-dart
endif

if WITH_PYTHON
SUBDIRS += py
PRECROSS_TARGET += precross-py
SUBDIRS += py.tornado
if WITH_TWISTED_TEST
SUBDIRS += py.twisted
endif
endif

if WITH_RUBY
SUBDIRS += rb
PRECROSS_TARGET += precross-rb
endif

if WITH_HAXE
SUBDIRS += haxe
endif

if WITH_DOTNET
SUBDIRS += netstd
endif

if WITH_GO
SUBDIRS += go
PRECROSS_TARGET += precross-go
endif

if WITH_ERLANG
SUBDIRS += erl
PRECROSS_TARGET += precross-erl
endif

if WITH_LUA
SUBDIRS += lua
PRECROSS_TARGET += precross-lua
endif

if WITH_RS
SUBDIRS += rs
PRECROSS_TARGET += precross-rs
endif

if WITH_SWIFT
SUBDIRS += swift
PRECROSS_TARGET += precross-swift
endif

#
# generate html for ThriftTest.thrift AND validate it!
#
if WITH_NODEJS
check-local:
	$(top_builddir)/compiler/cpp/thrift --gen html -r $(top_srcdir)/test/ThriftTest.thrift
	$(top_builddir)/node_modules/.bin/html-validator --file=gen-html/index.html --verbose
	$(top_builddir)/node_modules/.bin/html-validator --file=gen-html/ThriftTest.html --verbose
else
check-local:
	$(top_builddir)/compiler/cpp/thrift --gen html -r $(top_srcdir)/test/ThriftTest.thrift
endif

clean-local:
	$(RM) -r $(top_srcdir)/test/gen-html/
	find . -type d -name "__pycache__" | xargs rm -rf
	find . -type f -name "*.pyc" | xargs rm -f

dist-hook:
	$(RM) -r $(distdir)/gen-html/
	find $(distdir) -type d -name "__pycache__" | xargs rm -rf
	find $(distdir) -type f -name "*.pyc" | xargs rm -f

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	audit \
	c_glib \
	cl \
	cpp \
	crossrunner \
	dart \
	delphi \
	erl \
	keys \
	lua \
	ocaml \
	perl \
	php \
	py \
	py.tornado \
	py.twisted \
	rb \
	rs \
	swift \
	threads \
	partial \
	AnnotationTest.thrift \
	BrokenConstants.thrift \
	ConstantsDemo.thrift \
	DebugProtoTest.thrift \
	DenseLinkingTest.thrift \
	DocTest.thrift \
	DoubleConstantsTest.thrift \
	EnumContainersTest.thrift \
	EnumTest.thrift \
	ExceptionStruct.thrift \
	FullCamelTest.thrift \
	FuzzTest.thrift \
	Identifiers.thrift \
	Include.thrift \
	index.html \
	Int64Test.thrift \
	JsDeepConstructorTest.thrift \
	keys/keygen/.gitignore \
	known_failures_Linux.json \
	ManyOptionals.thrift \
	ManyTypedefs.thrift \
	NameConflictTest.thrift \
	OptionalRequiredTest.thrift \
	partial/thrift_test_schema.thrift \
	README.md \
	rebuild_known_failures.sh \
	Recursive.thrift \
	result.js \
	ReuseObjects.thrift \
	Service.thrift \
	SmallTest.thrift \
	SpecificNameTest.thrift \
	StressTest.thrift \
	test.py \
	tests.json \
	ThriftTest.thrift \
	TypedefTest.thrift \
	Types.thrift \
	UnsafeTypes.thrift \
	v0.16/ConstantsDemo.thrift \
	v0.16/DebugProtoTest.thrift \
	v0.16/FuzzTestNoUuid.thrift \
	v0.16/NameConflictTest.thrift \
	v0.16/ThriftTest.thrift \
	VoidMethExceptionsTest.thrift \
	valgrind.suppress

precross-%:
	$(MAKE) -C $* precross
precross: $(PRECROSS_TARGET)
