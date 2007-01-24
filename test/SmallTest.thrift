struct Hello {
  1: i32 simple,
  2: map<i32,i32> complex,
  3: map<i32, map<i32,i32>> complexer,
}

exception Goodbye {
  1: i32 simple,
  2: map<i32,i32> complex,
  3: map<i32, map<i32,i32>> complexer,  
}

service SmallService {
  Hello testMe(1:i32 hello, 2: Hello wonk) throws (1: Goodbye g),
  void testVoid() throws (1: Goodbye g),
  i32 testI32(1:i32 boo)
}
