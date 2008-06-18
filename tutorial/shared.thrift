#!/usr/local/bin/thrift --gen cpp --gen java --gen py --php --xsd --perl

/**
 * This Thrift file can be included by other Thrift files that want to share
 * these definitions.
 */

namespace cpp shared
namespace java shared
namespace perl shared

struct SharedStruct {
  1: i32 key
  2: string value
}

service SharedService {
  SharedStruct getStruct(1: i32 key)
}
