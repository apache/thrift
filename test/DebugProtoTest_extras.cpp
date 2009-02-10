// Extra functions required for DebugProtoTest_types to work

#include "gen-cpp/DebugProtoTest_types.h"


namespace thrift { namespace test { namespace debug {

bool Empty::operator<(Empty const& other) const {
  // It is empty, so all are equal.
  return false;
}

}}}
