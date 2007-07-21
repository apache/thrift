/*
thrift -cpp DebugProtoTest.thrift
g++ -Wall -I../lib/cpp/src -I/usr/local/include/boost-1_33_1 \
  DebugProtoTest.cpp gen-cpp/DebugProtoTest_types.cpp \
  ../lib/cpp/.libs/libthrift.a -o DebugProtoTest
./DebugProtoTest
*/

cpp_namespace thrift.test

struct OneOfEach {
  1: bool im_true,
  2: bool im_false,
  3: byte a_bite,
  4: i16 integer16,
  5: i32 integer32,
  6: i64 integer64,
  7: double double_precision,
  8: string some_characters,
  9: string zomg_unicode,
  10: bool what_who,
}

struct Bonk {
  1: i32 type,
  2: string message,
}

struct Nesting {
  1: Bonk my_bonk,
  2: OneOfEach my_ooe,
}

struct HolyMoley {
  1: list<OneOfEach> big,
  2: set<list<string>> contain,
  3: map<string,list<Bonk>> bonks,
}
