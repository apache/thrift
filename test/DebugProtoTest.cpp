/*
thrift -cpp DebugProtoTest.thrift
g++ -Wall -I../lib/cpp/src -I/usr/local/include/boost-1_33_1 \
  DebugProtoTest.cpp gen-cpp/DebugProtoTest_types.cpp \
  ../lib/cpp/.libs/libthrift.a -o DebugProtoTest
./DebugProtoTest
*/

#include <iostream>
#include <cmath>
#include "gen-cpp/DebugProtoTest_types.h"
#include "../lib/cpp/src/protocol/TDebugProtocol.h"

int main() {
  using std::cout;
  using std::endl;
  using namespace thrift::test;


  OneOfEach ooe;
  ooe.im_true   = true;
  ooe.im_false  = false;
  ooe.a_bite    = 0xd6;
  ooe.integer16 = 27000;
  ooe.integer32 = 1<<24;
  ooe.integer64 = 6000000000;
  ooe.double_precision = M_PI;
  ooe.some_characters  = "Debug THIS!";
  ooe.zomg_unicode     = "\xd7\n\a\t";

  cout << facebook::thrift::ThriftDebugString(ooe) << endl << endl;


  Nesting n;
  n.my_ooe = ooe;
  n.my_ooe.integer16 = 16;
  n.my_ooe.integer32 = 32;
  n.my_ooe.integer64 = 64;
  n.my_ooe.double_precision = (std::sqrt(5)+1)/2;
  n.my_ooe.some_characters  = ":R (me going \"rrrr\")";
  n.my_ooe.zomg_unicode     = "IDN Homograph Attack";
  n.my_bonk.type    = 31337;
  n.my_bonk.message = "I am a bonk... xor!";

  cout << facebook::thrift::ThriftDebugString(n) << endl << endl;


  HolyMoley hm;

  hm.big.push_back(ooe);
  hm.big.push_back(n.my_ooe);
  hm.big[0].a_bite = 0x22;
  hm.big[1].a_bite = 0x33;

  std::vector<std::string> stage1;
  stage1.push_back("and a one");
  stage1.push_back("and a two");
  hm.contain.insert(stage1);
  stage1.clear();
  stage1.push_back("then a one, two");
  stage1.push_back("three!");
  stage1.push_back("FOUR!!");
  hm.contain.insert(stage1);
  stage1.clear();
  hm.contain.insert(stage1);

  std::vector<Bonk> stage2;
  hm.bonks["nothing"] = stage2;
  stage2.resize(stage2.size()+1);
  stage2.back().type = 1;
  stage2.back().message = "Wait.";
  stage2.resize(stage2.size()+1);
  stage2.back().type = 2;
  stage2.back().message = "What?";
  hm.bonks["something"] = stage2;
  stage2.clear();
  stage2.resize(stage2.size()+1);
  stage2.back().type = 3;
  stage2.back().message = "quoth";
  stage2.resize(stage2.size()+1);
  stage2.back().type = 4;
  stage2.back().message = "the raven";
  stage2.resize(stage2.size()+1);
  stage2.back().type = 5;
  stage2.back().message = "nevermore";
  hm.bonks["poe"] = stage2;

  cout << facebook::thrift::ThriftDebugString(hm) << endl << endl;


  return 0;
}
