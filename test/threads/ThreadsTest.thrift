service ThreadsTest {
  i32 threadOne(1: i32 sleep=15),
  i32 threadTwo(2: i32 sleep=15),
  i32 threadThree(3: i32 sleep=15),
  i32 threadFour(4: i32 sleep=15)

  i32 stop();

}
