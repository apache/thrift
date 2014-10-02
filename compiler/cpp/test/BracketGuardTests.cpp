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

#include <sstream>

#include <boost/test/unit_test.hpp>

#include <generate/BracketGuard.h>

using namespace apache::thrift::compiler;

struct BracketGuardTestsFixture {
  std::ostringstream stream;

  std::string text() const { return stream.str(); }
};

BOOST_FIXTURE_TEST_SUITE( BracketGuardTests, BracketGuardTestsFixture )

BOOST_AUTO_TEST_CASE( BracketGuard_outputs_open_bracket_on_construction ) {
  BracketGuard guard(stream, "<", ">");

  BOOST_CHECK_EQUAL(text(), "<");
}

BOOST_AUTO_TEST_CASE( BracketGuard_outputs_close_bracket_on_destruction ) {
  {
    BracketGuard guard(stream, "[", "]");
  }

  BOOST_CHECK_EQUAL(text(), "[]");
}

BOOST_AUTO_TEST_SUITE_END()
