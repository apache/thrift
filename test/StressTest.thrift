namespace cpp test.stress

service Service {

  void echoVoid(),
  byte echoByte(1: byte arg),
  i32 echoI32(1: i32 arg),
  i64 echoI64(1: i64 arg),
  string echoString(1: string arg),
  list<byte>  echoList(1: list<byte> arg),
  set<byte>  echoSet(1: set<byte> arg),
  map<byte, byte>  echoMap(1: map<byte, byte> arg),
}

