// Define Child, then Parent. Parent is a forward declaration and was problematic for our Java compiler before
// fixing THRIFT-4086: Java compiler generates different meta data depending on order of structures in file
struct Child {
    1: required string Name
    2: required Parent Parent
}

struct Parent {
    1: required string Name
}
