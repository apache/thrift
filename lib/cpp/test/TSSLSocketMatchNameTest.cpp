/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#define BOOST_TEST_MODULE TSSLSocketMatchNameTest
#include <boost/test/unit_test.hpp>
#include <thrift/transport/TSSLSocket.h>

using apache::thrift::transport::AccessManager;
using apache::thrift::transport::DefaultClientAccessManager;

BOOST_AUTO_TEST_SUITE(TSSLSocketMatchNameTest)

// Helper: ALLOW means match, SKIP means no match.
static bool allows(const std::string& host, const std::string& pattern) {
  DefaultClientAccessManager mgr;
  return mgr.verify(host, pattern.c_str(), static_cast<int>(pattern.size()))
         == AccessManager::ALLOW;
}

BOOST_AUTO_TEST_CASE(standard_wildcard_matches) {
  BOOST_CHECK(allows("foo.example.com", "*.example.com"));
  BOOST_CHECK(allows("FOO.EXAMPLE.COM", "*.example.com"));  // case-insensitive
  BOOST_CHECK(allows("a.b.c.example.com", "*.b.c.example.com"));  // leftmost wildcard
}

BOOST_AUTO_TEST_CASE(exact_match) {
  BOOST_CHECK(allows("example.com", "example.com"));
  BOOST_CHECK(allows("foo.example.com", "foo.example.com"));
}

BOOST_AUTO_TEST_CASE(wildcard_must_not_span_labels) {
  BOOST_CHECK(!allows("foo.bar.example.com", "*.example.com"));
}

BOOST_AUTO_TEST_CASE(wildcard_must_be_in_leftmost_label) {
  // RFC 6125 §6.4.3: wildcard must not appear outside the leftmost label.
  BOOST_CHECK(!allows("example.foo.com",  "example.*.com"));
  BOOST_CHECK(!allows("a.evil.com",       "a.ev*.com"));
}

BOOST_AUTO_TEST_CASE(no_suffix_bypass) {
  BOOST_CHECK(!allows("evil.com.attacker.com", "evil.com"));
}

BOOST_AUTO_TEST_SUITE_END()
