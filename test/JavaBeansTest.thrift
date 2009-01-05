namespace java thrift.test

struct OneOfEachBeans {
  1: bool boolean_field,
  2: byte a_bite,
  3: i16 integer16,
  4: i32 integer32,
  5: i64 integer64,
  6: double double_precision,
  7: string some_characters,
  8: binary base64,
  9: list<byte> byte_list,
  10: list<i16> i16_list,
  11: list<i64> i64_list
}
