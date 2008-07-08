
namespace rb TestNamespace

struct Goodbyez {
  1: i32 val = 325;
}

senum Thinger {
  "ASDFKJ",
  "r32)*F#@",
  "ASDFLJASDF"
}

struct BoolPasser {
  1: bool value = 1
}

struct Hello {
  1: i32 simple = 53,
  2: map<i32,i32> complex = {23:532, 6243:632, 2355:532},
  3: map<i32, map<i32,i32>> complexer,
  4: string words = "words",
  5: Goodbyez thinz = {'val' : 36632}
}

const map<i32,map<i32,i32>> CMAP = { 235: {235:235}, 53:{53:53} }
const i32 CINT = 325;
const Hello WHOA = {'simple' : 532}

exception Goodbye {
  1: i32 simple,
  2: map<i32,i32> complex,
  3: map<i32, map<i32,i32>> complexer,
}

service SmallService {
  Thinger testThinger(1:Thinger bootz),
  Hello testMe(1:i32 hello=64, 2: Hello wonk) throws (1: Goodbye g),
  void testVoid() throws (1: Goodbye g),
  i32 testI32(1:i32 boo)
}
