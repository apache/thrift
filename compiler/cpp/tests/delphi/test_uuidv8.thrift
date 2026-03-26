namespace delphi Test.GuidV8

struct SimpleStruct {
  1: string name,
  2: i32 value
}

struct ComplexStruct {
  1: string id,
  2: list<string> tags,
  3: map<string, i32> counts,
  4: SimpleStruct nested
}

exception TestException {
  1: string message
}

service SimpleService {
  string echo(1: string input)
}

service ComplexService extends SimpleService {
  i32 calculate(1: i32 a, 2: i32 b),
  oneway void notify(1: string event)
}
