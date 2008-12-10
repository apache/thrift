typedef list<i32> ( cpp.template = "std::list" ) int_linked_list

struct foo {
  1: i32 bar;
  2: i32 baz;
  3: i32 qux;
  4: i32 bop;
} (
  cpp.type = "DenseFoo",
  python.type = "DenseFoo",
  java.final = "",
)
