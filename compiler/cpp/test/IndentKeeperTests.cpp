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

#include <generate/IndentKeeper.h>

using namespace apache::thrift::compiler;

struct IndentKeeperTestsFixture {
  IndentKeeper keeper;
};

BOOST_FIXTURE_TEST_SUITE( IndentKeeperTests, IndentKeeperTestsFixture )

BOOST_AUTO_TEST_CASE( empty_IndentKeeper_gives_empty_indent ) {
  BOOST_CHECK(keeper.indent().empty());
}

BOOST_AUTO_TEST_CASE( IndentKeeper_gives_proper_indent_after_set ) {
  const int test_indent1 = 6;
  keeper.set_indent(test_indent1);
  BOOST_CHECK_EQUAL(keeper.indent().size(), test_indent1 * IndentKeeper::INDENT_SIZE);

  const int test_indent2 = 16;
  keeper.set_indent(test_indent2);
  BOOST_CHECK_EQUAL(keeper.indent().size(), test_indent2 * IndentKeeper::INDENT_SIZE);
}

BOOST_AUTO_TEST_CASE( IndentKeeper_retunrs_only_spaces_in_indentation ) {
  const int test_indent = 160;
  keeper.set_indent(test_indent);
  const std::string ind = keeper.indent();

  for (int i = 0; i < test_indent; ++i)
    BOOST_CHECK_EQUAL(ind[i], ' ');
}

BOOST_AUTO_TEST_CASE( IndentKeeper_increments_and_decrements_indent ) {
  const int start_indent = keeper.get_indent();

  keeper.indent_up();
  BOOST_CHECK_EQUAL(keeper.get_indent(), start_indent + 1);
  keeper.indent_down();
  BOOST_CHECK_EQUAL(keeper.get_indent(), start_indent);
}

BOOST_AUTO_TEST_CASE( IndentKeeper_indents_stream ) {
  const std::string test_text = "abcd";

  std::ostringstream s;
  keeper.indent_up();
  keeper.indent(s) << test_text;

  BOOST_CHECK_EQUAL(s.str().size(), test_text.size() + IndentKeeper::INDENT_SIZE);
}

BOOST_AUTO_TEST_SUITE_END()
