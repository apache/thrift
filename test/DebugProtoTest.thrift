namespace cpp thrift.test.debug
namespace java thrift.test

struct Doubles {
 1: double nan,
 2: double inf,
 3: double neginf,
 4: double repeating,
 5: double big,
 6: double small,
 7: double zero,
 8: double negzero,
}

struct OneOfEach {
  1: bool im_true,
  2: bool im_false,
  3: byte a_bite = 200,
  4: i16 integer16 = 33000,
  5: i32 integer32,
  6: i64 integer64 = 10000000000,
  7: double double_precision,
  8: string some_characters,
  9: string zomg_unicode,
  10: bool what_who,
  11: binary base64,
  12: list<byte> byte_list = [1, 2, 3],
  13: list<i16> i16_list = [1,2,3],
  14: list<i64> i64_list = [1,2,3]
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

struct Base64 {
  1: i32 a,
  2: binary b1,
  3: binary b2,
  4: binary b3,
  5: binary b4,
  6: binary b5,
  7: binary b6,
}

service Srv {
  i32 Janky(i32 arg)
}

service Inherited extends Srv {
  i32 identity(i32 arg)
}

service EmptyService {}

// The only purpose of this thing is to increase the size of the generated code
// so that ZlibTest has more highly compressible data to play with.
struct BlowUp {
  1: map<list<i32>,set<map<i32,string>>> b1;
  2: map<list<i32>,set<map<i32,string>>> b2;
  3: map<list<i32>,set<map<i32,string>>> b3;
  4: map<list<i32>,set<map<i32,string>>> b4;
}
