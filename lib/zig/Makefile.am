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

if WITH_TESTS
SUBDIRS += tests
endif

GENERATED_APPLICATION_EXCEPTION = src/lib/generated/t_application_exception.zig

.PHONY: generate verify-generated

generate: generate-local
verify-generated: verify-generated-local

install:
	@echo '##############################################################'
	@echo '##############################################################'
	@echo 'The Zig client library should be installed via build.zig.zon'
	@echo '##############################################################'
	@echo '##############################################################'

style-local:
	$(ZIG) fmt --check src
	$(ZIG) fmt --check build.zig

check-local:
	$(ZIG) build test --summary all

verify-generated-local: $(THRIFT) idl/TApplicationException.thrift scripts/generate_application_exception.sh
	$(MKDIR_P) .gen
	$(THRIFT) -out .gen --gen zig idl/TApplicationException.thrift
	./scripts/generate_application_exception.sh .gen/t_application_exception.zig .gen/t_application_exception.zig.expected
	diff -u $(GENERATED_APPLICATION_EXCEPTION) .gen/t_application_exception.zig.expected

generate-local: $(THRIFT) idl/TApplicationException.thrift scripts/generate_application_exception.sh
	$(MKDIR_P) .gen src/lib/generated
	$(THRIFT) -out .gen --gen zig idl/TApplicationException.thrift
	./scripts/generate_application_exception.sh .gen/t_application_exception.zig $(GENERATED_APPLICATION_EXCEPTION)

all-local:
	$(ZIG) build -freference-trace

clean-local:
	$(RM) -r .zig-cache/ .gen/

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	src \
	build.zig \
	build.zig.zon \
	idl/TApplicationException.thrift \
	scripts/generate_application_exception.sh
