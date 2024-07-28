struct StructA { 1: i16 x; }
struct StructB { 1: i32 x; }
struct StructC { 1: StructA x; }
union UnionA { 1: StructA a; 2: StructB b; 3: StructC c; }
