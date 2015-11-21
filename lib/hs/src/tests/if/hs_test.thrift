struct Foo {
  5: i32 bar,
  1: i32 baz,
}

struct TestStruct {
  1: required bool f_bool,
  2: byte f_byte,
  3: double f_double,
  4: i16 f_i16 = 5,
  5: i32 f_i32,
  6: i64 f_i64,
  # 7: float f_float,
  8: list<i16> f_list,
  9: map<i16,i32> f_map = {1:2},
  10: string f_string,
  11: set<byte> f_set,
  12: optional i32 o_i32,
  99: Foo foo = {"bar":1,"baz":2},
}
