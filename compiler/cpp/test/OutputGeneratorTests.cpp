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

#include <generate/OutputGenerator.h>

using namespace apache::thrift::compiler;

struct OutputGeneratorTestsFixture {
  std::ostringstream stream;

  std::string generated() { return stream.str(); }
};

BOOST_FIXTURE_TEST_SUITE(OutputGeneratorTests, OutputGeneratorTestsFixture);

BOOST_AUTO_TEST_CASE( OutputGenerator_embeds_stream ) {
  const std::string test_str = "test";

  OutputGenerator gen(stream);

  gen.out() << test_str;
  BOOST_CHECK_EQUAL(generated(), test_str);
}

BOOST_AUTO_TEST_CASE( OutputGenerator_can_inherit_stream ) {
  OutputGenerator parent(stream);
  parent.out() << "a";

  OutputGenerator child(parent);
  child.out() << "b";

  BOOST_CHECK_EQUAL(generated(), "ab");
}

BOOST_AUTO_TEST_CASE( OutputGenerator_can_inherit_indent ) {
  const int test_indent = 10;
  OutputGenerator parent(stream);
  parent.set_indent(test_indent);

  {
    OutputGenerator child(parent);
    child.indent_up();
    BOOST_CHECK_EQUAL(child.get_indent(), test_indent + 1);
  }

  BOOST_CHECK_EQUAL(parent.get_indent(), test_indent);
}

BOOST_AUTO_TEST_CASE( OutputGenerator_can_give_indented_output ) {
  OutputGenerator gen(stream);
  gen.indent_up();

  gen.indented() << "text";

  BOOST_CHECK_EQUAL(generated(), "  text");
}

BOOST_AUTO_TEST_SUITE_END();
