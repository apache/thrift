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

#include <boost/test/unit_test.hpp>
#include <iostream>
#include <sstream>
#include "gen-cpp/Thrift5272_types.h"

BOOST_AUTO_TEST_SUITE(Thrift5272Test)

namespace utf = boost::unit_test;

// Define this env var to enable some logging (in case you need to debug)
#undef ENABLE_STDERR_LOGGING

using namespace thrift5272;


BOOST_AUTO_TEST_CASE( printTo )
{
  std::stringstream ss;
  std::string text;
  Meta a = Meta();

  a.printTo(ss);
  text = ss.str();
  BOOST_TEST(text == "Meta(byte_type=0, i8_type=0, i16_type=0, i32_type=0, i64_type=0)");

  ss.clear();
  ss.str("");
  a.byte_type = 50;
  a.i8_type = 50;
  a.i16_type = 50;
  a.i32_type = 50;
  a.i64_type = 50;
  a.printTo(ss);
  text = ss.str();
  BOOST_TEST(text == "Meta(byte_type=50, i8_type=50, i16_type=50, i32_type=50, i64_type=50)");

  ss.clear();
  ss.str("");
  a.byte_type = 127;
  a.i8_type = 127;
  a.i16_type = 127;
  a.i32_type = 127;
  a.i64_type = 127;
  a.printTo(ss);
  text = ss.str();
  BOOST_TEST(text == "Meta(byte_type=127, i8_type=127, i16_type=127, i32_type=127, i64_type=127)");
}

BOOST_AUTO_TEST_CASE( ostream_handle_int8_to_str )
{
  int8_t t = 65;
  std::ostringstream o;
  o << t;
  BOOST_TEST(o.str() != "65", "ostingstream handles int8 correctly. let's drop specialization for Thrift5272 from TToString.h.");
}

BOOST_AUTO_TEST_SUITE_END()
