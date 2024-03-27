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

#include <thrift/TUuid.h>

using apache::thrift::TUuid;

BOOST_AUTO_TEST_SUITE(TUuidTest)

BOOST_AUTO_TEST_CASE(construction) {
    BOOST_TEST(TUuid().is_nil());
}

BOOST_AUTO_TEST_CASE(construction_string_valid) {
  const std::string expected_1{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};

  BOOST_TEST(to_string(TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c")) == expected_1);
  BOOST_TEST(to_string(TUuid("{5e2ab188-1726-4e75-a04f-1ed9a6a89c4c}")) == expected_1);
  BOOST_TEST(to_string(TUuid("{5e2ab18817264e75a04f1ed9a6a89c4c}")) == expected_1);
  BOOST_TEST(to_string(TUuid("5e2ab18817264e75a04f1ed9a6a89c4c")) == expected_1);
}

BOOST_AUTO_TEST_CASE(construction_string_invalid) {
  // This test also ensures that the constructor does not throw
  const std::string expected{"00000000-0000-0000-0000-000000000000"};

  BOOST_TEST(to_string(TUuid("5e2ab188-1726-4e75-a04f")) == expected);
  BOOST_TEST(to_string(TUuid("{}")) == expected);
  BOOST_TEST(to_string(TUuid("{5e2ab18817264e75a04f1ed9a6a89c4c")) == expected);
  BOOST_TEST(to_string(TUuid("5e2ab18817264e75a04f1ed9a689c4c")) == expected);
}

BOOST_AUTO_TEST_CASE(compare) {
  BOOST_TEST(TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c")
             == TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"));
  BOOST_TEST(TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c")
             != TUuid("00000000-1726-4e75-a04f-1ed9a6a89c4c"));
  BOOST_TEST(TUuid("{5e2ab188-1726-4e75-a04f-1ed9a6a89c4c}")
             == TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"));

  // This comparison is expected to fail if strcmp is used
  TUuid uuid_1{};
  TUuid uuid_2{};
  uuid_2.data[15] = 0x64;
  BOOST_TEST(uuid_1 != uuid_2);
}

BOOST_AUTO_TEST_CASE(assign_valid) {
  TUuid uuid_1{};
  BOOST_TEST(uuid_1.is_nil());
  uuid_1 = "5e2ab188-1726-4e75-a04f-1ed9a6a89c4c";
  BOOST_TEST(!uuid_1.is_nil());

  BOOST_TEST(uuid_1 == TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"));

  uuid_1 = "{12345678-1726-4e75-a04f-1ed9a6a89c4c}";
  BOOST_TEST(uuid_1 != TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"));
  BOOST_TEST(uuid_1 == TUuid("{12345678-1726-4e75-a04f-1ed9a6a89c4c}"));
}

BOOST_AUTO_TEST_CASE(assign_invalid) {
  TUuid uuid_1{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  BOOST_TEST(!uuid_1.is_nil());

  BOOST_CHECK_THROW(uuid_1 = "123", std::runtime_error);
  BOOST_TEST(uuid_1.is_nil());
  BOOST_TEST(to_string(uuid_1) == std::string{"00000000-0000-0000-0000-000000000000"});
}

BOOST_AUTO_TEST_SUITE_END()
