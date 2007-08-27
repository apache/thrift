/*
../compiler/cpp/thrift -cpp DebugProtoTest.thrift
../compiler/cpp/thrift -cpp StressTest.thrift
g++ -Wall -I../lib/cpp/src -I/usr/local/include/boost-1_33_1 \
  ReflectionTest.cpp \
  gen-cpp/StressTest_types.cpp gen-cpp/DebugProtoTest_types.cpp \
  gen-cpp/Service.cpp gen-cpp/PartiallyReflectable.cpp \
  ../lib/cpp/.libs/libthrift.a -o ReflectionTest
./ReflectionTest
*/

#include <iostream>
#include "gen-cpp/PartiallyReflectable.h"
#include "gen-cpp/Service.h"
#include "../lib/cpp/src/protocol/TDebugProtocol.h"

int main() {
  using std::cout;
  using std::endl;

  facebook::thrift::reflection::limited::Service srv1;
  thrift::test::PartiallyReflectableIf::getStaticLimitedReflection(srv1);
  cout << facebook::thrift::ThriftDebugString(srv1) << endl << endl;

  facebook::thrift::reflection::limited::Service srv2;
  test::stress::ServiceIf::getStaticLimitedReflection(srv2);
  cout << facebook::thrift::ThriftDebugString(srv2) << endl << endl;

  return 0;
}
