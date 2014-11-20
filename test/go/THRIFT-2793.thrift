
/*
 * Test case for THRIFT-2793
 *
 * Without the patch for THRIFT-2793 the thrift compiler
 * for Go produces invalid code for the structs below.
 */
struct THRIFT_2793_A { 1: set<THRIFT_2793_B> b }
struct THRIFT_2793_B { 1: i64 id }
struct THRIFT_2793_C { 1: list<THRIFT_2793_D> d }
struct THRIFT_2793_D { 1: i64 id }

