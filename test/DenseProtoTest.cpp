/*
../compiler/cpp/thrift -cpp -dense DebugProtoTest.thrift
g++ -Wall -g -I../lib/cpp/src -I/usr/local/include/boost-1_33_1 \
  DenseProtoTest.cpp gen-cpp/DebugProtoTest_types.cpp \
  ../lib/cpp/.libs/libthrift.a -o DenseProtoTest
./DenseProtoTest
*/

// I do this to reach into the guts of TDenseProtocol.  Sorry.
#define private public
#define inline

#undef NDEBUG
#include <cassert>
#include <iostream>
#include <cmath>
#include "gen-cpp/DebugProtoTest_types.h"
#include <protocol/TDenseProtocol.h>
#include <transport/TTransportUtils.h>


// Can't use memcmp here.  GCC is too smart.
bool my_memeq(const char* str1, const char* str2, int len) {
  for (int i = 0; i < len; i++) {
    if (str1[i] != str2[i]) {
      return false;
    }
  }
  return true;
}


int main() {
  using std::cout;
  using std::endl;
  using boost::shared_ptr;
  using namespace thrift::test;
  using namespace facebook::thrift::transport;
  using namespace facebook::thrift::protocol;


  OneOfEach ooe;
  ooe.im_true   = true;
  ooe.im_false  = false;
  ooe.a_bite    = 0xd6;
  ooe.integer16 = 27000;
  ooe.integer32 = 1<<24;
  ooe.integer64 = (uint64_t)6000 * 1000 * 1000;
  ooe.double_precision = M_PI;
  ooe.some_characters  = "Debug THIS!";
  ooe.zomg_unicode     = "\xd7\n\a\t";

  //cout << facebook::thrift::ThriftDebugString(ooe) << endl << endl;


  Nesting n;
  n.my_ooe = ooe;
  n.my_ooe.integer16 = 16;
  n.my_ooe.integer32 = 32;
  n.my_ooe.integer64 = 64;
  n.my_ooe.double_precision = (std::sqrt(5)+1)/2;
  n.my_ooe.some_characters  = ":R (me going \"rrrr\")";
  n.my_ooe.zomg_unicode     = "\xd3\x80\xe2\x85\xae\xce\x9d\x20"
                              "\xd0\x9d\xce\xbf\xe2\x85\xbf\xd0\xbe\xc9\xa1\xd0\xb3\xd0\xb0\xcf\x81\xe2\x84\x8e"
                              "\x20\xce\x91\x74\x74\xce\xb1\xe2\x85\xbd\xce\xba\xc7\x83\xe2\x80\xbc";
  n.my_bonk.type    = 31337;
  n.my_bonk.message = "I am a bonk... xor!";

  //cout << facebook::thrift::ThriftDebugString(n) << endl << endl;


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

  //cout << facebook::thrift::ThriftDebugString(hm) << endl << endl;

  shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  shared_ptr<TDenseProtocol> proto(new TDenseProtocol(buffer));
  proto->setTypeSpec(HolyMoley::local_reflection);

  hm.write(proto.get());
  HolyMoley hm2;
  hm2.read(proto.get());

  assert(hm == hm2);


  // Let's test out the variable-length ints, shall we?
  uint64_t vli;
  #define checkout(i, c) { \
    buffer->resetBuffer(); \
    proto->vliWrite(i); \
    assert(my_memeq(buffer->getBufferAsString().data(), c, sizeof(c)-1)); \
    proto->vliRead(vli); \
    assert(vli == i); \
  }

  checkout(0x00000000, "\x00");
  checkout(0x00000040, "\x40");
  checkout(0x0000007F, "\x7F");
  checkout(0x00000080, "\x81\x00");
  checkout(0x00002000, "\xC0\x00");
  checkout(0x00003FFF, "\xFF\x7F");
  checkout(0x00004000, "\x81\x80\x00");
  checkout(0x00100000, "\xC0\x80\x00");
  checkout(0x001FFFFF, "\xFF\xFF\x7F");
  checkout(0x00200000, "\x81\x80\x80\x00");
  checkout(0x08000000, "\xC0\x80\x80\x00");
  checkout(0x0FFFFFFF, "\xFF\xFF\xFF\x7F");
  checkout(0x10000000, "\x81\x80\x80\x80\x00");
  checkout(0x20000000, "\x82\x80\x80\x80\x00");
  checkout(0x1FFFFFFF, "\x81\xFF\xFF\xFF\x7F");
  checkout(0xFFFFFFFF, "\x8F\xFF\xFF\xFF\x7F");

  checkout(0x0000000100000000ull, "\x90\x80\x80\x80\x00");
  checkout(0x0000000200000000ull, "\xA0\x80\x80\x80\x00");
  checkout(0x0000000300000000ull, "\xB0\x80\x80\x80\x00");
  checkout(0x0000000700000000ull, "\xF0\x80\x80\x80\x00");
  checkout(0x00000007F0000000ull, "\xFF\x80\x80\x80\x00");
  checkout(0x00000007FFFFFFFFull, "\xFF\xFF\xFF\xFF\x7F");
  checkout(0x0000000800000000ull, "\x81\x80\x80\x80\x80\x00");
  checkout(0x1FFFFFFFFFFFFFFFull, "\x9F\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x7F");
  checkout(0x7FFFFFFFFFFFFFFFull, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x7F");
  checkout(0xFFFFFFFFFFFFFFFFull, "\x81\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x7F");


  return 0;
}
