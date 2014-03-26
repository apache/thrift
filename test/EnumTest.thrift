enum MyEnum1 {
  ME1_0 = 0,
  ME1_1 = 1,
  ME1_2,
  ME1_3,
  ME1_5 = 5,
  ME1_6,
}

enum MyEnum2 {
  ME2_0,
  ME2_1,
  ME2_2,
}

enum MyEnum3 {
  ME3_0,
  ME3_1,
  ME3_N2 = -2,
  ME3_N1,
  ME3_D0,
  ME3_D1,
  ME3_9 = 9,
  ME3_10,
}

enum MyEnum4 {
  ME4_A = 0x7ffffffd
  ME4_B
  ME4_C
  // attempting to define another enum value here fails
  // with an overflow error, as we overflow values that can be
  // represented with an i32.
}

enum MyEnum5 {
  // attempting to explicitly use values out of the i32 range will also fail
  // ME5_A = 0x80000000,
  // ME5_B = 0x100000000,
}

struct MyStruct {
  1: MyEnum2 me2_2 = MyEnum1.ME2_2
  2: MyEnum3 me3_n2 = MyEnum3.ME3_N2
  3: MyEnum3 me3_d1 = MyEnum3.ME3_D1
}
