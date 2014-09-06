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

#include <generate/IndentGuard.h>

using namespace apache::thrift::compiler;

struct IndentGuardTestsFixture {
  IndentKeeper keeper;
};

BOOST_FIXTURE_TEST_SUITE( IndentGuardTests, IndentGuardTestsFixture )

BOOST_AUTO_TEST_CASE( IndentGuard_increses_indent_on_construction ) {
  const int start_indent = keeper.get_indent();
  IndentGuard guard(keeper);
  BOOST_CHECK_EQUAL(keeper.get_indent(), start_indent + 1);
}

BOOST_AUTO_TEST_CASE( IndentGuard_restores_original_indent_on_destruction ) {
  const int start_indent = keeper.get_indent();
  {
    IndentGuard guard(keeper);
  }
  BOOST_CHECK_EQUAL(keeper.get_indent(), start_indent);
}

BOOST_AUTO_TEST_SUITE_END()
