// Extra functions required for ThriftTest_types to work

#include <protocol/TDebugProtocol.h>
#include "gen-cpp/ThriftTest_types.h"


namespace thrift { namespace test {

bool Insanity::operator<(thrift::test::Insanity const& other) const {
  using apache::thrift::ThriftDebugString;
  return ThriftDebugString(*this) < ThriftDebugString(other);
}

}}
