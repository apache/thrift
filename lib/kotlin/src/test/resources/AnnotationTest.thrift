namespace java org.apache.thrift.kotlin.annotation.test

struct Person {
  1: required i64 id (min="1", max="100000")
  2: required string name
  3: optional string phoneNumber
}
