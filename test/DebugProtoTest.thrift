cpp_namespace thrift.test

struct OneOfEach {
  1: bool im_true,
  2: bool im_false,
  3: byte a_bite,
  4: i16 integer16,
  5: i32 integer32,
  6: i64 integer64,
  7: double double_precision,
  8: string some_characters,
  9: string zomg_unicode,
  10: bool what_who,
}

struct Bonk {
  1: i32 type,
  2: string message,
}

struct Nesting {
  1: Bonk my_bonk,
  2: OneOfEach my_ooe,
}

struct HolyMoley {
  1: list<OneOfEach> big,
  2: set<list<string>> contain,
  3: map<string,list<Bonk>> bonks,
}

struct Backwards {
  2: i32 first_tag2,
  1: i32 second_tag1,
}

struct Empty {
}

struct Wrapper {
  1: Empty foo
}

struct RandomStuff {
  1: i32 a,
  2: i32 b,
  3: i32 c,
  4: i32 d,
  5: list<i32> myintlist,
  6: map<i32,Wrapper> maps,
  7: i64 bigint,
  8: double triple,
}

service Srv {
  i32 Janky(i32 arg)
}

service PartiallyReflectable {
  map<i32,map<i32,i32>> returnNotReflectable(1: i32 hello),
  void argNotReflectable(1: list<set<i32>> arg),
  void arg2NotReflectable(1: i32 arg1, 2: list<set<i32>> argNotReflectable),
  void withMap(1: map<i32, string> amap),

  OneOfEach refl1(1: list<Bonk> arg1),
  OneOfEach refl2(2: list<string> arg1, 1: Bonk arg2);
}

// The only purpose of this thing is to increase the size of the generated code
// so that ZlibTest has more highly compressible data to play with.
struct BlowUp {
  1: map<list<i32>,set<map<i32,string>>> b1;
  2: map<list<i32>,set<map<i32,string>>> b2;
  3: map<list<i32>,set<map<i32,string>>> b3;
  4: map<list<i32>,set<map<i32,string>>> b4;
}
