namespace rb SpecNamespace

struct Hello {
  1: string greeting = "hello world"
}

struct Foo {
  1: i32 simple = 53,
  2: string words = "words",
  3: Hello hello = {'greeting' : "hello, world!"},
  4: list<i32> ints = [1, 2, 2, 3],
  5: map<i32, map<string, double>> complex,
  6: set<i16> shorts = [5, 17, 239]
}

struct BoolStruct {
  1: bool yesno = 1
}

service NonblockingService {
  Hello greeting(1:bool english)
  bool block()
  async void unblock()
  async void shutdown()
  void sleep(1:double seconds)
}
