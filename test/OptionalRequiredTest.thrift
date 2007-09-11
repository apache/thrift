/*
../compiler/cpp/thrift -cpp OptionalRequiredTest.thrift
g++ -Wall -g -I../lib/cpp/src -I/usr/local/include/boost-1_33_1 \
  OptionalRequiredTest.cpp gen-cpp/OptionalRequiredTest_types.cpp \
  ../lib/cpp/.libs/libthrift.a -o OptionalRequiredTest
./OptionalRequiredTest
*/

cpp_namespace thrift.test

struct OldSchool {
  1: i16    im_int;
  2: string im_str;
  3: list<map<i32,string>> im_big;
}

struct Simple {
  1: /* :) */ i16 im_default;
  2: required i16 im_required;
  3: optional i16 im_optional;
}

struct Tricky1 {
  1: /* :) */ i16 im_default;
}

struct Tricky2 {
  1: optional i16 im_optional;
}

struct Tricky3 {
  1: required i16 im_required;
}

struct Complex {
  1:          i16 cp_default;
  2: required i16 cp_required;
  3: optional i16 cp_optional;
  4:          map<i16,Simple> the_map;
  5: required Simple req_simp;
  6: optional Simple opt_simp;
}

struct ManyOpt {
  1: optional i32 opt1;
  2: optional i32 opt2;
  3: optional i32 opt3;
  4:          i32 def4;
  5: optional i32 opt5;
  6: optional i32 opt6;
}
