
struct structA {
 1: required i64 sa_i
}

struct all_optional {
 1: optional string s = "DEFAULT",
 2: optional i64 i = 42,
 3: optional bool b = false,
 4: optional string s2,
 5: optional i64 i2,
 6: optional bool b2,
 7: optional structA aa,
 9: optional list<i64> l,
 10: optional list<i64> l2 = [1, 2],
 11: optional map<i64, i64> m,
 12: optional map<i64, i64> m2 = {1:2, 3:4},
 13: optional binary bin,
 14: optional binary bin2 = "asdf",
}

struct structB {
 1: required structA required_struct_thing
 2: optional structA optional_struct_thing
}
