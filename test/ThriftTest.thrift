enum Numberz
{
  ONE = 1,
  TWO,
  THREE,
  FIVE = 5,
  SIX,
  EIGHT = 8
}

typedef i64 UserId

struct Xtruct
{
  string string_thing = 0,
  byte   byte_thing = 1,
  i32    i32_thing = 3,
  i64    i64_thing = 5
}

struct Xtruct2
{
  byte   byte_thing,
  Xtruct struct_thing,
  i32    i32_thing
}

struct Insanity
{
  map<Numberz, UserId> userMap = 0,
  list<Xtruct> xtructList = 1
}

service ThriftTest
{
  void         testVoid()
  string       testString(string thing = 0)
  byte         testByte(byte thing = 0)
  i32          testI32(i32 thing = 0)
  i64          testI64(i64 thing = 0)
  Xtruct       testStruct(Xtruct thing = 0)
  Xtruct2      testNest(Xtruct2 thing = 0)
  map<i32,i32> testMap(map<i32,i32> thing = 0)
  set<i32>     testSet(set<i32> thing = 0)
  list<i32>    testList(list<i32> thing = 0)
  Numberz      testEnum(Numberz thing = 0)
  UserId       testTypedef(UserId thing = 0)

  map<i32,map<i32,i32>> testMapMap(i32 hello = 0)

  /* So you think you've got this all worked, out eh? */
  map<UserId, map<Numberz,Insanity>> testInsanity(Insanity argument = 0)
  
  Xtruct	testMulti(byte arg0, i32 arg1, u64 arg2, map<i16, string> arg3, Numberz arg4, UserId arg5)
}
