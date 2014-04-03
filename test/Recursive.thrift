struct RecTree {
  1: list<RecTree> children
  2: i16 item
}

struct RecList {
  1: RecList nextitem (cpp.ref = "true")
  3: i16 item
}

struct CoRec {
  1:  CoRec2  other (cpp.ref = "true")
}

struct CoRec2 {
  1: CoRec other
}

struct VectorTest {
  1: list<RecList> lister;
}

service TestService
{
  RecTree echoTree(1:RecTree tree)
  RecList echoList(1:RecList lst)
  CoRec echoCoRec(1:CoRec item)
}
