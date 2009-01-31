#include <iostream>
#include "gen-cpp/PartiallyReflectable.h"
#include "gen-cpp/Service.h"
#include "../lib/cpp/src/protocol/TDebugProtocol.h"

int main() {
  using std::cout;
  using std::endl;

  apache::thrift::reflection::limited::Service srv1;
  thrift::test::debug::PartiallyReflectableIf::getStaticLimitedReflection(srv1);
  cout << apache::thrift::ThriftDebugString(srv1) << endl << endl;

  apache::thrift::reflection::limited::Service srv2;
  test::stress::ServiceIf::getStaticLimitedReflection(srv2);
  cout << apache::thrift::ThriftDebugString(srv2) << endl << endl;

  return 0;
}
