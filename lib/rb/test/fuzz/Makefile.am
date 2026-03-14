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
FUZZ_RB_ROOT = $(top_srcdir)/lib/rb
FUZZ_GEN_DIR = $(FUZZ_RB_ROOT)/test/fuzz
FUZZ_STUBS = \
	$(FUZZ_GEN_DIR)/gen-rb/fuzz_test_constants.rb \
	$(FUZZ_GEN_DIR)/gen-rb/fuzz_test_types.rb

FUZZ_RUBY_FILES = \
	$(srcdir)/fuzz_tracer.rb \
	$(srcdir)/fuzz_common.rb \
	$(srcdir)/fuzz_parse_binary_protocol.rb \
	$(srcdir)/fuzz_parse_binary_protocol_harness.rb \
	$(srcdir)/fuzz_parse_binary_protocol_accelerated.rb \
	$(srcdir)/fuzz_parse_binary_protocol_accelerated_harness.rb \
	$(srcdir)/fuzz_parse_compact_protocol.rb \
	$(srcdir)/fuzz_parse_compact_protocol_harness.rb \
	$(srcdir)/fuzz_parse_json_protocol.rb \
	$(srcdir)/fuzz_parse_json_protocol_harness.rb \
	$(srcdir)/fuzz_roundtrip_binary_protocol.rb \
	$(srcdir)/fuzz_roundtrip_binary_protocol_harness.rb \
	$(srcdir)/fuzz_roundtrip_binary_protocol_accelerated.rb \
	$(srcdir)/fuzz_roundtrip_binary_protocol_accelerated_harness.rb \
	$(srcdir)/fuzz_roundtrip_compact_protocol.rb \
	$(srcdir)/fuzz_roundtrip_compact_protocol_harness.rb \
	$(srcdir)/fuzz_roundtrip_json_protocol.rb \
	$(srcdir)/fuzz_roundtrip_json_protocol_harness.rb

FUZZ_ASAN_OPTIONS ?= allocator_may_return_null=1:detect_leaks=0:use_sigaltstack=0
FUZZ_CC ?= clang
FUZZ_CXX ?= clang++
FUZZ_LDSHARED ?= $(FUZZ_CC) -shared
FUZZ_LDSHAREDXX ?= $(FUZZ_CXX) -shared
FUZZ_CFLAGS ?= -fsanitize=address,fuzzer-no-link -fno-omit-frame-pointer -fno-common -fPIC -g
FUZZ_CXXFLAGS ?= -fsanitize=address,fuzzer-no-link -fno-omit-frame-pointer -fno-common -fPIC -g
FUZZ_MAKE ?= make
FUZZ_MAKEFLAGS ?= --environment-overrides V=1

check: fuzz-prepare

fuzz-prepare: $(FUZZ_STUBS)
	@for script in $(FUZZ_RUBY_FILES); do \
		$(RUBY) -c $$script; \
	done
	cd $(FUZZ_RB_ROOT) && \
		RUBYLIB="lib:test/fuzz/gen-rb" $(RUBY) -e 'require "thrift"; require "fuzz_test_types"; abort("Fuzz::FuzzTest is missing") unless defined?(Fuzz::FuzzTest)'

$(THRIFT):
	$(MAKE) -C $(top_builddir)/compiler/cpp all

$(FUZZ_STUBS): $(top_srcdir)/test/FuzzTest.thrift $(THRIFT)
	$(MKDIR_P) $(FUZZ_GEN_DIR)
	$(THRIFT) --gen rb -o $(FUZZ_GEN_DIR) $(top_srcdir)/test/FuzzTest.thrift

fuzz-build-ext:
	cd $(FUZZ_RB_ROOT)/ext && \
		env MAKE="$(FUZZ_MAKE) $(FUZZ_MAKEFLAGS)" \
			CC="$(FUZZ_CC)" \
			CXX="$(FUZZ_CXX)" \
			LDSHARED="$(FUZZ_LDSHARED)" \
			LDSHAREDXX="$(FUZZ_LDSHAREDXX)" \
			CFLAGS="$(FUZZ_CFLAGS)" \
			CXXFLAGS="$(FUZZ_CXXFLAGS)" \
			$(RUBY) extconf.rb && \
		env MAKE="$(FUZZ_MAKE) $(FUZZ_MAKEFLAGS)" \
			CC="$(FUZZ_CC)" \
			CXX="$(FUZZ_CXX)" \
			LDSHARED="$(FUZZ_LDSHARED)" \
			LDSHAREDXX="$(FUZZ_LDSHAREDXX)" \
			CFLAGS="$(FUZZ_CFLAGS)" \
			CXXFLAGS="$(FUZZ_CXXFLAGS)" \
			$(FUZZ_MAKE) $(FUZZ_MAKEFLAGS) clean all

fuzz-run: fuzz-prepare
	@test -n "$(TARGET)" || { echo 'Set TARGET=<fuzz script name>'; exit 1; }
	cd $(FUZZ_RB_ROOT) && \
		ASAN_OPTIONS="$(FUZZ_ASAN_OPTIONS)" \
		LD_PRELOAD=$$($(RUBY) -e 'require "ruzzy"; print Ruzzy::ASAN_PATH') \
		$(RUBY) test/fuzz/$(TARGET) $(CORPUS) $(FUZZ_ARGS)

fuzz-parse-binary:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_parse_binary_protocol.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-parse-binary-accelerated: fuzz-build-ext
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_parse_binary_protocol_accelerated.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-parse-compact:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_parse_compact_protocol.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-parse-json:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_parse_json_protocol.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-roundtrip-binary:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_roundtrip_binary_protocol.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-roundtrip-binary-accelerated: fuzz-build-ext
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_roundtrip_binary_protocol_accelerated.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-roundtrip-compact:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_roundtrip_compact_protocol.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-roundtrip-json:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-run TARGET=fuzz_roundtrip_json_protocol.rb CORPUS="$(CORPUS)" FUZZ_ARGS="$(FUZZ_ARGS)"

fuzz-smoke-parse-binary:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-parse-binary CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-parse-binary-accelerated:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-parse-binary-accelerated CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-parse-compact:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-parse-compact CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-parse-json:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-parse-json CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-roundtrip-binary:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-roundtrip-binary CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-roundtrip-binary-accelerated:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-roundtrip-binary-accelerated CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-roundtrip-compact:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-roundtrip-compact CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

fuzz-smoke-roundtrip-json:
	$(MAKE) $(AM_MAKEFLAGS) fuzz-roundtrip-json CORPUS="$(CORPUS)" FUZZ_ARGS="-runs=100 $(FUZZ_ARGS)"

clean-local:
	-$(RM) -r $(FUZZ_GEN_DIR)/gen-rb

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	.gitignore \
	README.md \
	fuzz_common.rb \
	fuzz_tracer.rb \
	fuzz_parse_binary_protocol.rb \
	fuzz_parse_binary_protocol_harness.rb \
	fuzz_parse_binary_protocol_accelerated.rb \
	fuzz_parse_binary_protocol_accelerated_harness.rb \
	fuzz_parse_compact_protocol.rb \
	fuzz_parse_compact_protocol_harness.rb \
	fuzz_parse_json_protocol.rb \
	fuzz_parse_json_protocol_harness.rb \
	fuzz_roundtrip_binary_protocol.rb \
	fuzz_roundtrip_binary_protocol_harness.rb \
	fuzz_roundtrip_binary_protocol_accelerated.rb \
	fuzz_roundtrip_binary_protocol_accelerated_harness.rb \
	fuzz_roundtrip_compact_protocol.rb \
	fuzz_roundtrip_compact_protocol_harness.rb \
	fuzz_roundtrip_json_protocol.rb \
	fuzz_roundtrip_json_protocol_harness.rb
