// Define Parent, then Child. No forward declarations.
struct Parent {
    1: required string Name
}

struct Child {
    1: required string Name
    2: required Parent Parent
}
