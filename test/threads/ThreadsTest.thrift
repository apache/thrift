#!/usr/local/bin/thrift -cpp -py

service ThreadsTest {
  void threadOne(1: i32 sleep=15),
  void threadTwo(2: i32 sleep=15),
  void threadThree(3: i32 sleep=15),
  void threadFour(4: i32 sleep=15)

  void stop();

}
