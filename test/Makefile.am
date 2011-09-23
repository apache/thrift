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

if WITH_CPP
SUBDIRS += cpp
endif

if WITH_PYTHON
SUBDIRS += py
SUBDIRS += py.twisted
endif

if WITH_RUBY
SUBDIRS += rb
endif

if WITH_HASKELL
SUBDIRS += hs
endif

EXTRA_DIST = \
	cpp \
	threads \
	csharp \
	py \
	py.twisted \
	rb \
	perl \
	php \
	hs \
	ocaml \
	AnnotationTest.thrift \
	BrokenConstants.thrift \
	ConstantsDemo.thrift \
	DebugProtoTest.thrift \
	DenseLinkingTest.thrift \
	DocTest.thrift \
	JavaBeansTest.thrift \
	ManyTypedefs.thrift \
	OptionalRequiredTest.thrift \
	SmallTest.thrift \
	StressTest.thrift \
	ThriftTest.thrift \
	FastbinaryTest.py
