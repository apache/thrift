#!/usr/local/bin/thrift -cpp -java -py -php -xsd -perl

/**
 * This Thrift file can be included by other Thrift files that want to share
 * these definitions.
 */

namespace cpp shared
namespace java shared
perl_package shared

struct SharedStruct {
  1: i32 key
  2: string value
}

service SharedService {
  SharedStruct getStruct(1: i32 key)
}
