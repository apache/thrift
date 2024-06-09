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

BOOST_AUTO_TEST_SUITE(TUuidBoostTestNoDirective)

BOOST_AUTO_TEST_CASE(compiler_directive_not_set) {
    // Test if the macro is set as expected
    #ifdef THRIFT_TUUID_SUPPORT_BOOST_UUID
    BOOST_TEST(false, "The 'THRIFT_TUUID_SUPPORT_BOOST_UUID' preprocessor must NOT be set for these tests");
    #else
    BOOST_TEST(true);
    #endif // THRIFT_TUUID_SUPPORT_BOOST_UUID
}
BOOST_AUTO_TEST_SUITE_END()

// This inclusion order is unconventional: This test specifcially tests that
// the THRIFT_TUUID_SUPPORT_BOOST_UUID directive can be set before including the header
// to enable boost::uuid support without causing linking or other errors with
// the compiled thrift library.

#define THRIFT_TUUID_SUPPORT_BOOST_UUID
#define THRIFT_TUUID_BOOST_CONSTRUCTOR_EXPLICIT

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>

#include <thrift/TUuid.h>
using apache::thrift::TUuid;

BOOST_AUTO_TEST_SUITE(TUuidBoostTestNoDirective)

BOOST_AUTO_TEST_CASE(compiler_directive_set) {
    // Test if the macro is set as expected
    #ifdef THRIFT_TUUID_SUPPORT_BOOST_UUID
    BOOST_TEST(true);
    #else
    BOOST_TEST(false, "The 'THRIFT_TUUID_SUPPORT_BOOST_UUID' preprocessor must now be set for these tests");
    #endif // THRIFT_TUUID_SUPPORT_BOOST_UUID
}

BOOST_AUTO_TEST_CASE(from_boost_uuid_constructor) {
  static boost::uuids::string_generator gen;
  boost::uuids::uuid boost_uuid{gen("5cb719a4-cd15-4476-8bcc-f1834b2527ee")};
  BOOST_TEST(!boost_uuid.is_nil());
  const TUuid uuid{boost_uuid};
  BOOST_TEST(!uuid.is_nil());

  BOOST_TEST(to_string(boost_uuid) == to_string(uuid));
  BOOST_TEST(to_string(uuid) == std::string{"5cb719a4-cd15-4476-8bcc-f1834b2527ee"});
}

BOOST_AUTO_TEST_CASE(from_boost_uuid_assignment) {
  static boost::uuids::string_generator gen;
  boost::uuids::uuid boost_uuid{gen("1f610073-db33-4d21-adf2-75460d4955cc")};
  BOOST_TEST(!boost_uuid.is_nil());
  TUuid uuid{};
  BOOST_TEST(uuid.is_nil());

  uuid = TUuid{boost_uuid};

  BOOST_TEST(to_string(boost_uuid) == to_string(uuid));
  BOOST_TEST(to_string(uuid) == std::string{"1f610073-db33-4d21-adf2-75460d4955cc"});
}
BOOST_AUTO_TEST_SUITE_END()