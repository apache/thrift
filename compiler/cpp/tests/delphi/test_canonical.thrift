namespace delphi test.canonical

struct SimpleStruct {
  1: i64 id,
  2: string name,
  3: i32 value
}

struct ComplexStruct {
  1: string id,
  2: list<string> tags,
  3: map<string, i32> counts,
  4: SimpleStruct nested
}

exception ApplicationError {
  1: i32 code,
  2: string message
}

service BaseService {
  string ping()
}

service DerivedService extends BaseService {
  i32 calculate(1: i32 a, 2: i32 b)
}

service Notifier {
  oneway void notify(1: string event),
  string getStatus()
}
