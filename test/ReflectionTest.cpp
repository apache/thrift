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
