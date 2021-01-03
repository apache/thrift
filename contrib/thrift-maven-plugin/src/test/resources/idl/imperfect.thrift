namespace java strictness

exception BadSituation {}

service ImperfectService {
  void test() throws (BadSituation ouch)
}
