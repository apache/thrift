struct StructA
{
  1: string a,
  2: binary b,
  3: optional string c,
  4: optional binary d,
  5: required string e,
  6: required binary f,
  7: string g = "foo",
  8: i32 h,
  9: optional i32 i,
  10: required i32 j,
  11: required i32 k = 5,
  12: double l,
  13: optional double m,
  14: required double n,
  15: double o = 3.14159,
  16: list<string> string_list,
  17: list<byte> byte_list = [1, 2, 3],
  18: set<string> string_set,
  19: map<string, string> string_map
}
