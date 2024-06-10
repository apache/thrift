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
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>

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
  uuid_2.data()[15] = 0x64;
  BOOST_TEST(uuid_1 != uuid_2);
}

BOOST_AUTO_TEST_CASE(assign_valid) {
  TUuid uuid_1{};
  BOOST_TEST(uuid_1.is_nil());
  uuid_1 = TUuid{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  BOOST_TEST(!uuid_1.is_nil());

  BOOST_TEST(uuid_1 == TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"));

  uuid_1 = TUuid{"{12345678-1726-4e75-a04f-1ed9a6a89c4c}"};
  BOOST_TEST(uuid_1 != TUuid("5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"));
  BOOST_TEST(uuid_1 == TUuid("{12345678-1726-4e75-a04f-1ed9a6a89c4c}"));
}

BOOST_AUTO_TEST_CASE(assign_invalid) {
  TUuid uuid_1{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  BOOST_TEST(!uuid_1.is_nil());

  BOOST_CHECK_NO_THROW(uuid_1 = TUuid{"123"});
  BOOST_TEST(uuid_1.is_nil());
  BOOST_TEST(to_string(uuid_1) == std::string{"00000000-0000-0000-0000-000000000000"});
}

BOOST_AUTO_TEST_CASE(swap) {
  TUuid uuid_1{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  TUuid uuid_2{};
  BOOST_TEST(!uuid_1.is_nil());
  BOOST_TEST(uuid_2.is_nil());

  using std::swap;
  swap(uuid_1, uuid_2);

  BOOST_TEST(uuid_1.is_nil());
  BOOST_TEST(!uuid_2.is_nil());

  BOOST_TEST(to_string(uuid_1) == std::string{"00000000-0000-0000-0000-000000000000"});
  BOOST_TEST(to_string(uuid_2) == std::string{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"});
}

BOOST_AUTO_TEST_CASE(begin_end) {
  TUuid uuid_1{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  BOOST_TEST(std::distance(std::begin(uuid_1), std::end(uuid_1)) == uuid_1.size());
}

BOOST_AUTO_TEST_CASE(into_boost_uuid) {
  TUuid uuid{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  boost::uuids::uuid boost_uuid{};
  BOOST_TEST(boost_uuid.is_nil());
  std::copy(std::begin(uuid), std::end(uuid), boost_uuid.begin());
  BOOST_TEST(!boost_uuid.is_nil());
  BOOST_TEST(boost::uuids::to_string(boost_uuid) == "5e2ab188-1726-4e75-a04f-1ed9a6a89c4c");
  BOOST_TEST(boost::uuids::to_string(boost_uuid) == to_string(uuid));
}

BOOST_AUTO_TEST_CASE(from_boost_uuid) {
  static boost::uuids::string_generator gen;
  boost::uuids::uuid boost_uuid{gen("1f610073-db33-4d21-adf2-75460d4955cc")};
  BOOST_TEST(!boost_uuid.is_nil());
  TUuid uuid;
  BOOST_TEST(uuid.is_nil());

  std::copy(std::begin(boost_uuid), std::end(boost_uuid), uuid.begin());
  BOOST_TEST(!uuid.is_nil());

  BOOST_TEST(to_string(boost_uuid) == to_string(uuid));
}

BOOST_AUTO_TEST_CASE(test_byte_order_variant) {
  TUuid uuid{"5e2ab188-1726-4e75-a04f-1ed9a6a89c4c"};
  boost::uuids::uuid boost_uuid{};
  BOOST_TEST(boost_uuid.is_nil());
  std::copy(std::begin(uuid), std::end(uuid), boost_uuid.begin());
  BOOST_TEST(!boost_uuid.is_nil());
  BOOST_TEST(boost_uuid.variant() == boost::uuids::uuid::variant_rfc_4122);
}

BOOST_AUTO_TEST_CASE(test_byte_order_verify_network) {
  const TUuid uuid{"{00112233-4455-6677-8899-aabbccddeeff}"};

  for (uint8_t idx = 0; idx < uuid.size(); ++idx) {
    const uint8_t expected = idx * 0x11;
    BOOST_TEST(*(std::begin(uuid) + idx) == expected);
  }

  const uint8_t test[16] = {0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
                            0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff};

  TUuid new_uuid;
  std::copy(std::begin(test), std::end(test), std::begin(new_uuid));

  BOOST_TEST(!new_uuid.is_nil());
  BOOST_TEST(to_string(new_uuid) == std::string{"00112233-4455-6677-8899-aabbccddeeff"});

  BOOST_TEST(new_uuid == uuid);
}

BOOST_AUTO_TEST_CASE(test_character_buffer) {

  const uint8_t test[16] = {0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
                            0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff};

  const TUuid uuid{test};

  BOOST_TEST(to_string(uuid) == std::string{"00112233-4455-6677-8899-aabbccddeeff"});
}

BOOST_AUTO_TEST_CASE(test_boost_buffer) {

  static boost::uuids::string_generator gen;
  boost::uuids::uuid boost_uuid{gen("1f610073-db33-4d21-adf2-75460d4955cc")};
  BOOST_TEST(!boost_uuid.is_nil());

  const TUuid uuid{boost_uuid.data};

  BOOST_TEST(to_string(boost_uuid) == to_string(uuid));
}

BOOST_AUTO_TEST_SUITE_END()
