namespace test.stress

service Service {

  void echoVoid(),
  byte echoByte(byte arg),
  i32 echoI32(i32 arg),
  i64 echoI64(i64 arg),
  string echoString(string arg),
  list<byte>  echoList(list<byte> arg),
  set<byte>  echoSet(set<byte> arg),
  map<byte, byte>  echoMap(map<byte, byte> arg),
}

