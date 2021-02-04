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

if WITH_HASKELL
SUBDIRS += hs
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

EXTRA_DIST = \
	audit \
	c_glib \
	cl \
	cpp \
	crossrunner \
	dart \
	erl \
	hs \
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
	threads \
	AnnotationTest.thrift \
	BrokenConstants.thrift \
	ConstantsDemo.thrift \
	DebugProtoTest.thrift \
	DenseLinkingTest.thrift \
	DocTest.thrift \
	DoubleConstantsTest.thrift \
	EnumContainersTest.thrift \
	EnumTest.thrift \
	FullCamelTest.thrift \
	Include.thrift \
	Identifiers.thrift \
	Int64Test.thrift \
	JavaBeansTest.thrift \
	JavaBinaryDefault.thrift \
	JavaDeepCopyTest.thrift \
	JavaTypes.thrift \
	JsDeepConstructorTest.thrift \
	ManyOptionals.thrift \
	ManyTypedefs.thrift \
	NameConflictTest.thrift \
	OptionalRequiredTest.thrift \
	Recursive.thrift \
	ReuseObjects.thrift \
	SmallTest.thrift \
	StressTest.thrift \
	ThriftTest.thrift \
	TypedefTest.thrift \
	Types.thrift \
	UnsafeTypes.thrift \
	Service.thrift \
	SpecificNameTest.thrift \
	known_failures_Linux.json \
	test.py \
	tests.json \
	rebuild_known_failures.sh \
	result.js \
	index.html \
	README.md \
	valgrind.suppress

precross-%:
	$(MAKE) -C $* precross
precross: $(PRECROSS_TARGET)
