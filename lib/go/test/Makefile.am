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

GOBUILDEXTRA = -buildvcs=false

THRIFT_GO_ARGS_BASE = thrift_import=github.com/apache/thrift/lib/go/thrift,package_prefix=github.com/apache/thrift/lib/go/test/gopath/src/

THRIFTARGS = -out gopath/src/ --gen go:$(THRIFT_GO_ARGS_BASE)$(COMPILER_EXTRAFLAG)
THRIFTTEST = $(top_srcdir)/test/ThriftTest.thrift

THRIFTARGS_SKIP_REMOTE = -out gopath/src/ --gen go:skip_remote,$(THRIFT_GO_ARGS_BASE)$(COMPILER_EXTRAFLAG)

# Thrift for GO has problems with complex map keys: THRIFT-2063
gopath: $(THRIFT) $(THRIFTTEST) \
				IncludesTest.thrift \
				NamespacedTest.thrift \
				MultiplexedProtocolTest.thrift \
				OnewayTest.thrift \
				OptionalFieldsTest.thrift \
				RequiredFieldTest.thrift \
				ServicesTest.thrift \
				GoTagTest.thrift \
				TypedefFieldTest.thrift \
				RefAnnotationFieldsTest.thrift \
				UnionDefaultValueTest.thrift \
				UnionBinaryTest.thrift \
				ErrorTest.thrift \
				NamesTest.thrift \
				InitialismsTest.thrift \
				DontExportRWTest.thrift \
				dontexportrwtest/compile_test.go \
				IgnoreInitialismsTest.thrift \
				ConflictNamespaceTestA.thrift \
				ConflictNamespaceTestB.thrift \
				ConflictNamespaceTestC.thrift \
				ConflictNamespaceTestD.thrift \
				ConflictNamespaceTestE.thrift \
				ConflictNamespaceTestF.thrift \
				ConflictNamespaceTestSuperThing.thrift \
				ConflictNamespaceServiceTest.thrift \
				DuplicateImportsTest.thrift \
				EqualsTest.thrift \
				ConflictArgNamesTest.thrift \
				ConstOptionalFieldImport.thrift \
				ConstOptionalField.thrift \
				ProcessorMiddlewareTest.thrift \
				ClientMiddlewareExceptionTest.thrift \
				ValidateTest.thrift \
				ForwardType.thrift \
				StringParseAllocationTest.thrift
	mkdir -p gopath/src
	grep -v list.*map.*list.*map $(THRIFTTEST) | grep -v 'set<Insanity>' > ThriftTest.thrift
	$(THRIFT) $(THRIFTARGS) -r IncludesTest.thrift
	$(THRIFT) $(THRIFTARGS) BinaryKeyTest.thrift
	$(THRIFT) $(THRIFTARGS) MultiplexedProtocolTest.thrift
	$(THRIFT) $(THRIFTARGS) OnewayTest.thrift
	$(THRIFT) $(THRIFTARGS) OptionalFieldsTest.thrift
	$(THRIFT) $(THRIFTARGS) RequiredFieldTest.thrift
	$(THRIFT) $(THRIFTARGS) ServicesTest.thrift
	$(THRIFT) $(THRIFTARGS) GoTagTest.thrift
	$(THRIFT) $(THRIFTARGS) TypedefFieldTest.thrift
	$(THRIFT) $(THRIFTARGS) RefAnnotationFieldsTest.thrift
	$(THRIFT) $(THRIFTARGS) UnionDefaultValueTest.thrift
	$(THRIFT) $(THRIFTARGS) UnionBinaryTest.thrift
	$(THRIFT) $(THRIFTARGS) ErrorTest.thrift
	$(THRIFT) $(THRIFTARGS) NamesTest.thrift
	$(THRIFT) $(THRIFTARGS) InitialismsTest.thrift
	$(THRIFT) $(THRIFTARGS),read_write_private DontExportRWTest.thrift
	$(THRIFT) $(THRIFTARGS),ignore_initialisms IgnoreInitialismsTest.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestA.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestB.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestC.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestD.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestE.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestF.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceTestSuperThing.thrift
	$(THRIFT) $(THRIFTARGS) ConflictNamespaceServiceTest.thrift
	$(THRIFT) $(THRIFTARGS) -r DuplicateImportsTest.thrift
	$(THRIFT) $(THRIFTARGS) EqualsTest.thrift
	$(THRIFT) $(THRIFTARGS) ConflictArgNamesTest.thrift
	$(THRIFT) $(THRIFTARGS) -r ConstOptionalField.thrift
	$(THRIFT) $(THRIFTARGS_SKIP_REMOTE) ProcessorMiddlewareTest.thrift
	$(THRIFT) $(THRIFTARGS) ClientMiddlewareExceptionTest.thrift
	$(THRIFT) $(THRIFTARGS) ValidateTest.thrift
	$(THRIFT) $(THRIFTARGS) ForwardType.thrift
	$(THRIFT) $(THRIFTARGS) StringParseAllocationTest.thrift
	ln -nfs ../../tests gopath/src/tests
	cp -r ./dontexportrwtest gopath/src
	touch gopath

check: gopath
	$(GO) build $(GOBUILDEXTRA) \
				./gopath/src/includestest \
				./gopath/src/binarykeytest \
				./gopath/src/servicestest \
				./gopath/src/typedeffieldtest \
				./gopath/src/refannotationfieldstest \
				./gopath/src/errortest	\
				./gopath/src/namestest \
				./gopath/src/initialismstest \
				./gopath/src/dontexportrwtest \
				./gopath/src/ignoreinitialismstest \
				./gopath/src/unionbinarytest \
				./gopath/src/conflict/super \
				./gopath/src/conflict/context/conflict_service-remote \
				./gopath/src/servicestest/container_test-remote \
				./gopath/src/duplicateimportstest \
				./gopath/src/equalstest \
				./gopath/src/conflictargnamestest \
				./gopath/src/processormiddlewaretest \
				./gopath/src/clientmiddlewareexceptiontest \
				./gopath/src/validatetest \
				./gopath/src/forwardtypetest
	$(GO) test github.com/apache/thrift/lib/go/thrift
	$(GO) test ./gopath/src/tests ./gopath/src/dontexportrwtest

clean-local:
	$(RM) -r gopath ThriftTest.thrift gen-go

client: stubs
	$(GO) run TestClient.go

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	dontexportrwtest \
	tests \
	common \
	go.mod \
	go.sum \
	BinaryKeyTest.thrift \
	ClientMiddlewareExceptionTest.thrift \
	ConflictArgNamesTest.thrift \
	ConflictNamespaceServiceTest.thrift \
	ConflictNamespaceTestA.thrift \
	ConflictNamespaceTestB.thrift \
	ConflictNamespaceTestC.thrift \
	ConflictNamespaceTestD.thrift \
	ConflictNamespaceTestE.thrift \
	ConflictNamespaceTestF.thrift \
	ConflictNamespaceTestSuperThing.thrift \
	ConstOptionalField.thrift \
	ConstOptionalFieldImport.thrift \
	DontExportRWTest.thrift \
	DuplicateImportsTest.thrift \
	ErrorTest.thrift \
	EqualsTest.thrift \
	ForwardType.thrift \
	GoTagTest.thrift \
	IgnoreInitialismsTest.thrift \
	IncludesTest.thrift \
	InitialismsTest.thrift \
	MultiplexedProtocolTest.thrift \
	NamespacedTest.thrift \
	NamesTest.thrift \
	OnewayTest.thrift \
	OptionalFieldsTest.thrift \
	ProcessorMiddlewareTest.thrift \
	RefAnnotationFieldsTest.thrift \
	RequiredFieldTest.thrift \
	ServicesTest.thrift \
	StringParseAllocationTest.thrift \
	TypedefFieldTest.thrift \
	UnionBinaryTest.thrift \
	UnionDefaultValueTest.thrift \
	ValidateTest.thrift
