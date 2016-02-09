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

#define _USE_MATH_DEFINES
#include <cmath>
#include <iomanip>
#include <sstream>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TJSONProtocol.h>
#include "gen-cpp/DebugProtoTest_types.h"

#define BOOST_TEST_MODULE JSONProtoTest
#include <boost/test/unit_test.hpp>

using namespace thrift::test::debug;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::protocol::TJSONProtocol;

static std::auto_ptr<OneOfEach> ooe;

void testCaseSetup_1() {
  ooe.reset(new OneOfEach);
  ooe->im_true = true;
  ooe->im_false = false;
  ooe->a_bite = 0x7f;
  ooe->integer16 = 27000;
  ooe->integer32 = 1 << 24;
  ooe->integer64 = (uint64_t)6000 * 1000 * 1000;
  ooe->double_precision = M_PI;
  ooe->some_characters = "JSON THIS! \"\1";
  ooe->zomg_unicode = "\xd7\n\a\t";
  ooe->base64 = "\1\2\3\255";
  ooe->float_precision = (float)M_PI;
  ooe->rank_map[567419810] = (float)0.211184;
  ooe->rank_map[507959914] = (float)0.080382;
}

BOOST_AUTO_TEST_CASE(test_json_proto_1) {
  testCaseSetup_1();

  const std::string expected_result(
  "{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":127},\"4\":{\"i16\":27000},"
  "\"5\":{\"i32\":16777216},\"6\":{\"i64\":6000000000},\"7\":{\"dbl\":3.1415926"
  "535897931},\"8\":{\"str\":\"JSON THIS! \\\"\\u0001\"},\"9\":{\"str\":\"\xd7"
  "\\n\\u0007\\t\"},\"10\":{\"tf\":0},\"11\":{\"str\":\"AQIDrQ\"},\"12\":{\"lst"
  "\":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\":{\"lst\":[\"i"
  "64\",3,1,2,3]},\"17\":{\"flt\":3.14159274},\"18\":{\"map\":[\"i64\",\"flt\","
  "2,{\"507959914\":0.0803819969,\"567419810\":0.211183995}]}}"
  );

  const std::string result(apache::thrift::ThriftJSONString(*ooe));

  BOOST_CHECK_MESSAGE(!expected_result.compare(result),
    "Expected:\n" << expected_result << "\nGotten:\n" << result);
}

static std::auto_ptr<Nesting> n;

void testCaseSetup_2() {
  testCaseSetup_1();

  n.reset(new Nesting);
  n->my_ooe = *ooe;
  n->my_ooe.integer16 = 16;
  n->my_ooe.integer32 = 32;
  n->my_ooe.integer64 = 64;
  n->my_ooe.double_precision = (std::sqrt(5.0) + 1) / 2;
  n->my_ooe.float_precision = (std::sqrt(5.0f) + 1) / 2;
  n->my_ooe.some_characters = ":R (me going \"rrrr\")";
  n->my_ooe.zomg_unicode     = "\xd3\x80\xe2\x85\xae\xce\x9d\x20\xd0\x9d\xce"
                               "\xbf\xe2\x85\xbf\xd0\xbe\xc9\xa1\xd0\xb3\xd0"
                               "\xb0\xcf\x81\xe2\x84\x8e\x20\xce\x91\x74\x74"
                               "\xce\xb1\xe2\x85\xbd\xce\xba\xc7\x83\xe2\x80"
                               "\xbc";
  n->my_bonk.type = 31337;
  n->my_bonk.message = "I am a bonk... xor!";
}

BOOST_AUTO_TEST_CASE(test_json_proto_2) {
  testCaseSetup_2();

  const std::string expected_result(
  "{\"1\":{\"rec\":{\"1\":{\"i32\":31337},\"2\":{\"str\":\"I am a bonk... xor!"
  "\"}}},\"2\":{\"rec\":{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":127},"
  "\"4\":{\"i16\":16},\"5\":{\"i32\":32},\"6\":{\"i64\":64},\"7\":{\"dbl\":1.61"
  "80339887498949},\"8\":{\"str\":\":R (me going \\\"rrrr\\\")\"},\"9\":{\"str"
  "\":\"ӀⅮΝ Нοⅿоɡгаρℎ Αttαⅽκǃ‼\"},\"10\":{\"tf\":0},\"11\":{\"str\":\"AQIDrQ\"}"
  ",\"12\":{\"lst\":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\""
  ":{\"lst\":[\"i64\",3,1,2,3]},\"17\":{\"flt\":1.61803401},\"18\":{\"map\":[\""
  "i64\",\"flt\",2,{\"507959914\":0.0803819969,\"567419810\":0.211183995}]}}}}"
  );

  const std::string result(apache::thrift::ThriftJSONString(*n));

  BOOST_CHECK_MESSAGE(!expected_result.compare(result),
    "Expected:\n" << expected_result << "\nGotten:\n" << result);
}

static std::auto_ptr<HolyMoley> hm;

void testCaseSetup_3() {
  testCaseSetup_2();

  hm.reset(new HolyMoley);

  hm->big.push_back(*ooe);
  hm->big.push_back(n->my_ooe);
  hm->big[0].a_bite = 0x22;
  hm->big[1].a_bite = 0x33;

  std::vector<std::string> stage1;
  stage1.push_back("and a one");
  stage1.push_back("and a two");
  hm->contain.insert(stage1);
  stage1.clear();
  stage1.push_back("then a one, two");
  stage1.push_back("three!");
  stage1.push_back("FOUR!!");
  hm->contain.insert(stage1);
  stage1.clear();
  hm->contain.insert(stage1);

  std::vector<Bonk> stage2;
  hm->bonks["nothing"] = stage2;
  stage2.resize(stage2.size() + 1);
  stage2.back().type = 1;
  stage2.back().message = "Wait.";
  stage2.resize(stage2.size() + 1);
  stage2.back().type = 2;
  stage2.back().message = "What?";
  hm->bonks["something"] = stage2;
  stage2.clear();
  stage2.resize(stage2.size() + 1);
  stage2.back().type = 3;
  stage2.back().message = "quoth";
  stage2.resize(stage2.size() + 1);
  stage2.back().type = 4;
  stage2.back().message = "the raven";
  stage2.resize(stage2.size() + 1);
  stage2.back().type = 5;
  stage2.back().message = "nevermore";
  hm->bonks["poe"] = stage2;
}

BOOST_AUTO_TEST_CASE(test_json_proto_3) {
  testCaseSetup_3();

  const std::string expected_result(
  "{\"1\":{\"lst\":[\"rec\",2,{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":"
  "34},\"4\":{\"i16\":27000},\"5\":{\"i32\":16777216},\"6\":{\"i64\":6000000000"
  "},\"7\":{\"dbl\":3.1415926535897931},\"8\":{\"str\":\"JSON THIS! \\\"\\u0001"
  "\"},\"9\":{\"str\":\"\xd7\\n\\u0007\\t\"},\"10\":{\"tf\":0},\"11\":{\"str\":"
  "\"AQIDrQ\"},\"12\":{\"lst\":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2"
  ",3]},\"14\":{\"lst\":[\"i64\",3,1,2,3]},\"17\":{\"flt\":3.14159274},\"18\":{"
  "\"map\":[\"i64\",\"flt\",2,{\"507959914\":0.0803819969,\"567419810\":0.21118"
  "3995}]}},{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":51},\"4\":{\"i16\""
  ":16},\"5\":{\"i32\":32},\"6\":{\"i64\":64},\"7\":{\"dbl\":1.6180339887498949"
  "},\"8\":{\"str\":\":R (me going \\\"rrrr\\\")\"},\"9\":{\"str\":\"ӀⅮΝ Нοⅿоɡг"
  "аρℎ Αttαⅽκǃ‼\"},\"10\":{\"tf\":0},\"11\":{\"str\":\"AQIDrQ\"},\"12\":{\"lst"
  "\":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\":{\"lst\":[\"i"
  "64\",3,1,2,3]},\"17\":{\"flt\":1.61803401},\"18\":{\"map\":[\"i64\",\"flt\","
  "2,{\"507959914\":0.0803819969,\"567419810\":0.211183995}]}}]},\"2\":{\"set\""
  ":[\"lst\",3,[\"str\",0],[\"str\",2,\"and a one\",\"and a two\"],[\"str\",3,"
  "\"then a one, two\",\"three!\",\"FOUR!!\"]]},\"3\":{\"map\":[\"str\",\"lst\""
  ",3,{\"nothing\":[\"rec\",0],\"poe\":[\"rec\",3,{\"1\":{\"i32\":3},\"2\":{\"s"
  "tr\":\"quoth\"}},{\"1\":{\"i32\":4},\"2\":{\"str\":\"the raven\"}},{\"1\":{"
  "\"i32\":5},\"2\":{\"str\":\"nevermore\"}}],\"something\":[\"rec\",2,{\"1\":{"
  "\"i32\":1},\"2\":{\"str\":\"Wait.\"}},{\"1\":{\"i32\":2},\"2\":{\"str\":\"Wh"
  "at?\"}}]}]}}"
  );

  const std::string result(apache::thrift::ThriftJSONString(*hm));

  BOOST_CHECK_MESSAGE(!expected_result.compare(result),
    "Expected:\n" << expected_result << "\nGotten:\n" << result);
}

BOOST_AUTO_TEST_CASE(test_json_proto_4) {
  testCaseSetup_1();

  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  ooe->write(proto.get());
  OneOfEach ooe2;
  ooe2.read(proto.get());

  BOOST_CHECK(*ooe == ooe2);
}

BOOST_AUTO_TEST_CASE(test_json_proto_5) {
  testCaseSetup_3();

  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  hm->write(proto.get());
  HolyMoley hm2;
  hm2.read(proto.get());

  BOOST_CHECK(*hm == hm2);

  hm2.big[0].a_bite = 0x00;

  BOOST_CHECK(*hm != hm2);
}

BOOST_AUTO_TEST_CASE(test_json_proto_6_doubles) {
  Doubles dub;
  dub.nan = HUGE_VAL / HUGE_VAL;
  dub.inf = HUGE_VAL;
  dub.neginf = -HUGE_VAL;
  dub.repeating = 10.0 / 3.0;
  dub.big = 1E+305;
  dub.tiny = 1E-305;
  dub.zero = 0.0;
  dub.negzero = -0.0;

  const std::string expected_result(
  "{\"1\":{\"dbl\":\"NaN\"},\"2\":{\"dbl\":\"Infinity\"},\"3\":{\"dbl\":\"-Infi"
  "nity\"},\"4\":{\"dbl\":3.3333333333333335},\"5\":{\"dbl\":9.9999999999999994"
  "e+304},\"6\":{\"dbl\":1e-305},\"7\":{\"dbl\":0},\"8\":{\"dbl\":-0}}"
  );

  const std::string result(apache::thrift::ThriftJSONString(dub));

  BOOST_CHECK_MESSAGE(!expected_result.compare(result),
    "Expected:\n" << expected_result << "\nGotten:\n" << result);
}

BOOST_AUTO_TEST_CASE(test_json_proto_6_floats) {
  Floats flt;
  flt.nan = HUGE_VALF / HUGE_VALF;
  flt.inf = HUGE_VALF;
  flt.neginf = -HUGE_VALF;
  flt.repeating = 10.0 / 3.0;
  flt.big = 1E+30;
  flt.tiny = 1E-30;
  flt.zero = 0.0;
  flt.negzero = -0.0;

  const std::string expected_result(
  "{\"1\":{\"flt\":\"NaN\"},\"2\":{\"flt\":\"Infinity\"},\"3\":{\"flt\":\"-Infi"
  "nity\"},\"4\":{\"flt\":3.33333325},\"5\":{\"flt\":1.00000002e+30},"
  "\"6\":{\"flt\":1e-30},\"7\":{\"flt\":0},\"8\":{\"flt\":-0}}"
  );

  const std::string result(apache::thrift::ThriftJSONString(flt));

  BOOST_CHECK_MESSAGE(!expected_result.compare(result),
    "Expected:\n" << expected_result << "\nGotten:\n" << result);
}

BOOST_AUTO_TEST_CASE(test_json_proto_7) {
  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  Base64 base;
  base.a = 123;
  base.b1 = "1";
  base.b2 = "12";
  base.b3 = "123";
  base.b4 = "1234";
  base.b5 = "12345";
  base.b6 = "123456";

  base.write(proto.get());
  Base64 base2;
  base2.read(proto.get());

  BOOST_CHECK(base == base2);
}

BOOST_AUTO_TEST_CASE(test_json_proto_8) {
  const char* json_string =
  "{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":127},\"4\":{\"i16\":27000},"
  "\"5\":{\"i32\":16.77216},\"6\":{\"i64\":6000000000},\"7\":{\"dbl\":3.1415926"
  "535897931},\"8\":{\"str\":\"JSON THIS! \\\"\\u0001\"},\"9\":{\"str\":\"\xd7"
  "\\n\\u0007\\t\"},\"10\":{\"tf\":0},\"11\":{\"str\":\"AQIDrQ\"},\"12\":{\"lst"
  "\":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\":{\"lst\":[\"i"
  "64\",3,1,2,3]}}";

  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(
    (uint8_t*)(json_string), strlen(json_string)*sizeof(char)));
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  OneOfEach ooe2;

  BOOST_CHECK_THROW(ooe2.read(proto.get()),
    apache::thrift::protocol::TProtocolException);
}

static std::string toHexSequence(const std::string& str) {
  std::stringstream ss;
  ss << std::hex << std::setfill('0');
  for (std::size_t i = 0; i < str.size(); i++) {
    ss << "\\x" << int(uint8_t(str[i]));
  }
  return ss.str();
}

BOOST_AUTO_TEST_CASE(test_json_unicode_escaped) {
  const char json_string[] =
  "{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":127},\"4\":{\"i16\":27000},"
  "\"5\":{\"i32\":16},\"6\":{\"i64\":6000000000},\"7\":{\"dbl\":3.1415926"
  "535897931},\"8\":{\"str\":\"JSON THIS!\"},\"9\":{\"str\":\"\\u0e01 \\ud835\\udd3e\"},"
  "\"10\":{\"tf\":0},\"11\":{\"str\":\"000000\"},\"12\":{\"lst\""
  ":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\":{\"lst\":[\"i64"
  "\",3,1,2,3]}}";
  const char* expected_zomg_unicode = "\xe0\xb8\x81 \xf0\x9d\x94\xbe";

  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(
    (uint8_t*)(json_string), sizeof(json_string)));
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  OneOfEach ooe2;
  ooe2.read(proto.get());
  BOOST_CHECK_MESSAGE(!ooe2.zomg_unicode.compare(expected_zomg_unicode),
    "Expected:\n" << toHexSequence(expected_zomg_unicode) << "\nGotten:\n"
                  << toHexSequence(ooe2.zomg_unicode));

}

BOOST_AUTO_TEST_CASE(test_json_unicode_escaped_missing_low_surrogate) {
  const char json_string[] =
  "{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":127},\"4\":{\"i16\":27000},"
  "\"5\":{\"i32\":16},\"6\":{\"i64\":6000000000},\"7\":{\"dbl\":3.1415926"
  "535897931},\"8\":{\"str\":\"JSON THIS!\"},\"9\":{\"str\":\"\\ud835\"},"
  "\"10\":{\"tf\":0},\"11\":{\"str\":\"000000\"},\"12\":{\"lst\""
  ":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\":{\"lst\":[\"i64"
  "\",3,1,2,3]}}";

  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(
    (uint8_t*)(json_string), sizeof(json_string)));
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  OneOfEach ooe2;
  BOOST_CHECK_THROW(ooe2.read(proto.get()),
    apache::thrift::protocol::TProtocolException);
}

BOOST_AUTO_TEST_CASE(test_json_unicode_escaped_missing_hi_surrogate) {
  const char json_string[] =
  "{\"1\":{\"tf\":1},\"2\":{\"tf\":0},\"3\":{\"i8\":127},\"4\":{\"i16\":27000},"
  "\"5\":{\"i32\":16},\"6\":{\"i64\":6000000000},\"7\":{\"dbl\":3.1415926"
  "535897931},\"8\":{\"str\":\"JSON THIS!\"},\"9\":{\"str\":\"\\udd3e\"},"
  "\"10\":{\"tf\":0},\"11\":{\"str\":\"000000\"},\"12\":{\"lst\""
  ":[\"i8\",3,1,2,3]},\"13\":{\"lst\":[\"i16\",3,1,2,3]},\"14\":{\"lst\":[\"i64"
  "\",3,1,2,3]}}";

  boost::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(
    (uint8_t*)(json_string), sizeof(json_string)));
  boost::shared_ptr<TJSONProtocol> proto(new TJSONProtocol(buffer));

  OneOfEach ooe2;
  BOOST_CHECK_THROW(ooe2.read(proto.get()),
    apache::thrift::protocol::TProtocolException);
}
