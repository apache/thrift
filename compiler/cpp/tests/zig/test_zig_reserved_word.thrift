enum KWEnum {
  error = 1,
  test = 2,
}

struct error {
  1: i32 return,
  2: i32 init,
  3: KWEnum test,
}

struct Holder {
  1: error body,
}

service test {
  void fn(1: error arg),
  void return(1: error arg),
}