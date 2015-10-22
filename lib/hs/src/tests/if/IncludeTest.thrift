include "IncludedTest.thrift"

struct Bar {
  1: IncludedTest.Foo baz = { "Baz": 1 };
}
