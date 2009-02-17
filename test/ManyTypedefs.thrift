// This is to make sure you don't mess something up when you change typedef code.
// Generate it with the old and new thrift and make sure they are the same.
/*
rm -rf gen-* orig-*
mkdir old new
thrift --gen cpp --gen java --gen php --gen phpi --gen py --gen rb --gen xsd --gen perl --gen ocaml --gen erl --gen hs --strict ManyTypedefs.thrift
mv gen-* old
../compiler/cpp/thrift --gen cpp --gen java --gen php --gen phpi --gen py --gen rb --gen xsd --gen perl --gen ocaml --gen erl --gen hs --strict ManyTypedefs.thrift
mv gen-* new
diff -ur old new
rm -rf old new
# There should be no output.
*/

typedef i32 int32
typedef list<map<int32, string>> biglist

struct struct1 {
  1: int32 myint;
  2: biglist mylist;
}

exception exception1 {
  1: biglist alist;
  2: struct1 mystruct;
}

service AService {
  struct1 method1(1: int32 myint) throws (1: exception1 exn);
  biglist method2();
}
