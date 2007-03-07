#!/usr/local/bin/thrift -cpp -java -py -php -xsd

/**
 * This Thrift file can be included by other Thrift files that want to share
 * these definitions.
 */

cpp_namespace shared
java_package shared

struct SharedStruct {
  1: i32 key
  2: string value
}

service SharedService {
  SharedStruct getStruct(1: i32 key)
}
