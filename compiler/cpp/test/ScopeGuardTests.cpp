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

#include <generate/ScopeGuard.h>
#include <generate/IndentKeeper.h>
#include <generate/OutputGenerator.h>


using namespace apache::thrift::compiler;

struct ScopeGuardTestsFixture {
  std::ostringstream stream;
  OutputGenerator generator;

  ScopeGuardTestsFixture()
    : generator(stream)
  {}

  std::string text() const { return stream.str(); }
};

BOOST_FIXTURE_TEST_SUITE( ScopeGuardTests, ScopeGuardTestsFixture )

BOOST_AUTO_TEST_CASE( ScopeGuard_open_bracket_and_increses_indent_on_construction ) {
  const int start_indent = generator.get_indent();

  ScopeGuard guard(generator);

  BOOST_CHECK_EQUAL(text(), "{\n");
  BOOST_CHECK_EQUAL(generator.get_indent(), start_indent + 1);
}

BOOST_AUTO_TEST_CASE( ScopeGuard_closes_bracket_and_restores_indent_on_destruction ) {
  const int start_indent = generator.get_indent();

  {
    ScopeGuard guard(generator);
  }

  BOOST_CHECK_EQUAL(text(), "{\n}\n");
  BOOST_CHECK_EQUAL(generator.get_indent(), start_indent);
}

BOOST_AUTO_TEST_SUITE_END()
