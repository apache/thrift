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

# These sed commands are needed to work around the fact that the generator doesn't
# support the arbitrary crate yet.
stubs: $(top_builddir)/test/FuzzTest.thrift $(THRIFT)
	$(THRIFT) -out lib/ --gen rs $(top_builddir)/test/FuzzTest.thrift
	sed -i 's/thrift::OrderedFloat/ordered_float::OrderedFloat/g' lib/fuzz_test.rs
	sed -i 's/derive(/derive(arbitrary::Arbitrary, /g' lib/fuzz_test.rs

check: stubs
	$(CARGO) fmt --all -- --check
	$(CARGO) clippy --all -- -D warnings
	$(CARGO) build

clean-local:
	$(CARGO) clean
	-$(RM) Cargo.lock
	-$(RM) -r target/
	-$(RM) -r corpus/
	-$(RM) -r artifacts/
	-$(RM) -r coverage/
	-$(RM) lib/fuzz_test.rs

distdir:
	$(MAKE) $(AM_MAKEFLAGS) distdir-am

EXTRA_DIST = \
	Cargo.toml \
	lib/mod.rs \
	fuzz_targets/parse_compact.rs \
	fuzz_targets/parse_binary.rs \
	fuzz_targets/roundtrip_binary.rs \
	fuzz_targets/roundtrip_compact.rs \
	fuzz_targets/structured_roundtrip_compact.rs \
	fuzz_targets/structured_roundtrip_binary.rs \
	bin/corpus_generator.rs
