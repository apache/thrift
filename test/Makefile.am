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

SUBDIRS =

if WITH_C_GLIB
SUBDIRS += c_glib
endif

if WITH_CPP
SUBDIRS += cpp
endif

if WITH_PERL
SUBDIRS += perl
endif

if WITH_PHP
SUBDIRS += php
endif

if WITH_PYTHON
SUBDIRS += py
SUBDIRS += py.twisted
SUBDIRS += py.tornado
endif

if WITH_RUBY
SUBDIRS += rb
endif

if WITH_HASKELL
SUBDIRS += hs
endif

if WITH_GO
SUBDIRS += go
endif

if WITH_ERLANG
SUBDIRS += erl
endif

#
# generate html for ThriftTest.thrift
#
check-local:
	$(top_builddir)/compiler/cpp/thrift --gen html -r $(top_srcdir)/test/ThriftTest.thrift

EXTRA_DIST = \
	keys \
	test.sh \
	test.py \
	tests.json \
	c_glib \
	cpp \
	erl \
	hs \
	lua \
	ocaml \
	perl \
	php \
	py \
	py.twisted \
	py.tornado \
	rb \
	threads \
	AnnotationTest.thrift \
	BrokenConstants.thrift \
	ConstantsDemo.thrift \
	DebugProtoTest.thrift \
	DenseLinkingTest.thrift \
	DocTest.thrift \
	EnumTest.thrift \
	Include.thrift \
	JavaBeansTest.thrift \
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
	FastbinaryTest.py \
	result.html \
	README.md
