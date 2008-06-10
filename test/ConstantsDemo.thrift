namespace cpp yozone

struct thing {
  1: i32 hello,
  2: i32 goodbye
}

enum enumconstants {
  ONE = 1,
  TWO = 2
}

struct thing2 {
  1: enumconstants val = TWO
}

typedef i32 myIntType
const myIntType myInt = 3

const map<enumconstants,string> GEN_ENUM_NAMES = {ONE : "HOWDY", TWO: PARTNER}

const i32 hex_const = 0x0001F

const i32 GEN_ME = -3523553
const double GEn_DUB = 325.532
const double GEn_DU = 085.2355
const string GEN_STRING = "asldkjasfd"

const map<i32,i32> GEN_MAP = { 35532 : 233, 43523 : 853 }
const list<i32> GEN_LIST = [ 235235, 23598352, 3253523 ]

const map<i32, map<i32, i32>> GEN_MAPMAP = { 235 : { 532 : 53255, 235:235}}

const map<string,i32> GEN_MAP2 = { "hello" : 233, "lkj98d" : 853, 'lkjsdf' : 098325 }

const thing GEN_THING = { 'hello' : 325, 'goodbye' : 325352 }

const map<i32,thing> GEN_WHAT = { 35 : { 'hello' : 325, 'goodbye' : 325352 } }

const set<i32> GEN_SET = [ 235, 235, 53235 ]

exception Blah {
  1:  i32 bing }

exception Gak {}

service yowza {
  void blingity(),
  i32 blangity() throws (1: Blah hoot )
}
