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

#define BOOST_TEST_MODULE FloatTest
#include <boost/test/unit_test.hpp>
#include "gen-cpp/FloatTest_types.h"

BOOST_AUTO_TEST_SUITE( FloatTest )

BOOST_AUTO_TEST_CASE( test_float ) {
  foo f;
  f.bar = 5;
  BOOST_CHECK(5 == f.bar);
  BOOST_CHECK(1 == f.baz);
  BOOST_CHECK((float)12.345 == f.baz1);
  f.baz = (float)12.345;
  BOOST_CHECK((float)12.345 == f.baz);
}

BOOST_AUTO_TEST_SUITE_END()
