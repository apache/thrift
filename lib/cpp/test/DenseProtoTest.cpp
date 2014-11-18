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

/*
../compiler/cpp/thrift --gen cpp:dense DebugProtoTest.thrift
../compiler/cpp/thrift --gen cpp:dense OptionalRequiredTest.thrift
g++ -Wall -g -I../lib/cpp/src -I/usr/local/include/boost-1_33_1 \
  gen-cpp/OptionalRequiredTest_types.cpp \
  gen-cpp/DebugProtoTest_types.cpp \
  DenseProtoTest.cpp ../lib/cpp/.libs/libthrift.a -o DenseProtoTest
./DenseProtoTest
*/

// I do this to reach into the guts of TDenseProtocol.  Sorry.
#define private public
#define inline

#undef NDEBUG
#include <cstdlib>
#include <cassert>
#include <iostream>
#include <cmath>
#include <string>
#include "gen-cpp/DebugProtoTest_types.h"
#include "gen-cpp/OptionalRequiredTest_types.h"
#include <thrift/protocol/TDenseProtocol.h>
#include <thrift/transport/TBufferTransports.h>


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
  using std::string;
  using std::cout;
  using std::endl;
  using boost::shared_ptr;
  using namespace thrift::test::debug;
  using namespace apache::thrift::transport;
  using namespace apache::thrift::protocol;


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

  //cout << apache::thrift::ThriftDebugString(ooe) << endl << endl;


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

  //cout << apache::thrift::ThriftDebugString(n) << endl << endl;


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

  //cout << apache::thrift::ThriftDebugString(hm) << endl << endl;

  shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  shared_ptr<TDenseProtocol> proto(new TDenseProtocol(buffer));
  proto->setTypeSpec(HolyMoley::local_reflection);

  hm.write(proto.get());
  HolyMoley hm2;
  hm2.read(proto.get());

  assert(hm == hm2);


  // Let's test out the variable-length ints, shall we?
  uint64_t vlq;
  #define checkout(i, c) { \
    buffer->resetBuffer(); \
    proto->vlqWrite(i); \
    proto->getTransport()->flush(); \
    assert(my_memeq(buffer->getBufferAsString().data(), c, sizeof(c)-1)); \
    proto->vlqRead(vlq); \
    assert(vlq == i); \
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

  // Test out the slow path with a TBufferedTransport.
  shared_ptr<TBufferedTransport> buff_trans(new TBufferedTransport(buffer, 3));
  proto.reset(new TDenseProtocol(buff_trans));
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

  // Test optional stuff.
  proto.reset(new TDenseProtocol(buffer));
  proto->setTypeSpec(ManyOpt::local_reflection);
  ManyOpt mo1, mo2, mo3, mo4, mo5, mo6;
  mo1.opt1 = 923759347;
  mo1.opt2 = 392749274;
  mo1.opt3 = 395739402;
  mo1.def4 = 294730928;
  mo1.opt5 = 394309218;
  mo1.opt6 = 832194723;
  mo1.__isset.opt1 = true;
  mo1.__isset.opt2 = true;
  mo1.__isset.opt3 = true;
  mo1.__isset.def4 = true;
  mo1.__isset.opt5 = true;
  mo1.__isset.opt6 = true;

  mo1.write(proto.get());
  mo2.read(proto.get());

  assert(mo2.__isset.opt1 == true);
  assert(mo2.__isset.opt2 == true);
  assert(mo2.__isset.opt3 == true);
  assert(mo2.__isset.def4 == true);
  assert(mo2.__isset.opt5 == true);
  assert(mo2.__isset.opt6 == true);

  assert(mo1 == mo2);

  mo1.__isset.opt1 = false;
  mo1.__isset.opt3 = false;
  mo1.__isset.opt5 = false;

  mo1.write(proto.get());
  mo3.read(proto.get());

  assert(mo3.__isset.opt1 == false);
  assert(mo3.__isset.opt2 == true);
  assert(mo3.__isset.opt3 == false);
  assert(mo3.__isset.def4 == true);
  assert(mo3.__isset.opt5 == false);
  assert(mo3.__isset.opt6 == true);

  assert(mo1 == mo3);

  mo1.__isset.opt1 = true;
  mo1.__isset.opt3 = true;
  mo1.__isset.opt5 = true;
  mo1.__isset.opt2 = false;
  mo1.__isset.opt6 = false;

  mo1.write(proto.get());
  mo4.read(proto.get());

  assert(mo4.__isset.opt1 == true);
  assert(mo4.__isset.opt2 == false);
  assert(mo4.__isset.opt3 == true);
  assert(mo4.__isset.def4 == true);
  assert(mo4.__isset.opt5 == true);
  assert(mo4.__isset.opt6 == false);

  assert(mo1 == mo4);

  mo1.__isset.opt1 = false;
  mo1.__isset.opt5 = false;

  mo1.write(proto.get());
  mo5.read(proto.get());

  assert(mo5.__isset.opt1 == false);
  assert(mo5.__isset.opt2 == false);
  assert(mo5.__isset.opt3 == true);
  assert(mo5.__isset.def4 == true);
  assert(mo5.__isset.opt5 == false);
  assert(mo5.__isset.opt6 == false);

  assert(mo1 == mo5);

  mo1.__isset.opt3 = false;

  mo1.write(proto.get());
  mo6.read(proto.get());

  assert(mo6.__isset.opt1 == false);
  assert(mo6.__isset.opt2 == false);
  assert(mo6.__isset.opt3 == false);
  assert(mo6.__isset.def4 == true);
  assert(mo6.__isset.opt5 == false);
  assert(mo6.__isset.opt6 == false);

  assert(mo1 == mo6);


  // Test fingerprint checking stuff.

  {
    // Default and required have the same fingerprint.
    Tricky1 t1;
    Tricky3 t3;
    assert(string(Tricky1::ascii_fingerprint) == Tricky3::ascii_fingerprint);
    proto->setTypeSpec(Tricky1::local_reflection);
    t1.im_default = 227;
    t1.write(proto.get());
    proto->setTypeSpec(Tricky3::local_reflection);
    t3.read(proto.get());
    assert(t3.im_required == 227);
  }

  {
    // Optional changes things.
    Tricky1 t1;
    Tricky2 t2;
    assert(string(Tricky1::ascii_fingerprint) != Tricky2::ascii_fingerprint);
    proto->setTypeSpec(Tricky1::local_reflection);
    t1.im_default = 227;
    t1.write(proto.get());
    try {
      proto->setTypeSpec(Tricky2::local_reflection);
      t2.read(proto.get());
      assert(false);
    } catch (TProtocolException& ex) {
      buffer->resetBuffer();
    }
  }

  {
    // Holy cow.  We can use the Tricky1 typespec with the Tricky2 structure.
    Tricky1 t1;
    Tricky2 t2;
    proto->setTypeSpec(Tricky1::local_reflection);
    t1.im_default = 227;
    t1.write(proto.get());
    t2.read(proto.get());
    assert(t2.__isset.im_optional == true);
    assert(t2.im_optional == 227);
  }

  {
    // And totally off the wall.
    Tricky1 t1;
    OneOfEach ooe2;
    assert(string(Tricky1::ascii_fingerprint) != OneOfEach::ascii_fingerprint);
    proto->setTypeSpec(Tricky1::local_reflection);
    t1.im_default = 227;
    t1.write(proto.get());
    try {
      proto->setTypeSpec(OneOfEach::local_reflection);
      ooe2.read(proto.get());
      assert(false);
    } catch (TProtocolException& ex) {
      buffer->resetBuffer();
    }
  }

  // Okay, this is really off the wall.
  // Just don't crash.
  cout << "Starting fuzz test.  This takes a while.  (20 dots.)" << endl;
  std::srand(12345);
  for (int i = 0; i < 2000; i++) {
    if (i % 100 == 0) {
      cout << ".";
      cout.flush();
    }
    buffer->resetBuffer();
    // Make sure the fingerprint prefix is right.
    buffer->write(Nesting::binary_fingerprint, 4);
    for (int j = 0; j < 1024*1024; j++) {
      uint8_t r = std::rand();
      buffer->write(&r, 1);
    }
    Nesting n;
    proto->setTypeSpec(OneOfEach::local_reflection);
    try {
      n.read(proto.get());
    } catch (TProtocolException& ex) {
    } catch (TTransportException& ex) {
    }
  }
  cout << endl;

  return 0;
}
