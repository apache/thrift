#include <iostream>
#include <string>

#include "ThreadFactoryTests.h"
#include "TimerManagerTests.h"
#include "ThreadManagerTests.h"

int main(int argc, char** argv) {

  std::string arg;

  if(argc < 2) {

    arg = "all";

  } else {
    
    arg = std::string(argv[1]);
  }

  bool runAll = arg.compare("all") == 0;

  if(runAll || arg.compare("thread-factory") == 0) {

    ThreadFactoryTests threadFactoryTests;
    
    std::cout << "ThreadFactory tests..." << std::endl;
    
    std::cout << "\tThreadFactory hello-world test" << std::endl;

    assert(threadFactoryTests.helloWorldTest());

    size_t count =  1000;

    std::cout << "\t\tThreadFactory reap N threads test: N = " << count << std::endl;

    assert(threadFactoryTests.reapNThreads(count));

    std::cout << "\t\tThreadFactory synchronous start test" << std::endl;

    assert(threadFactoryTests.synchStartTest());
  }

  if(runAll || arg.compare("timer-manager") == 0) {

    std::cout << "TimerManager tests..." << std::endl;

    std::cout << "\t\tTimerManager test00" << std::endl;

    TimerManagerTests timerManagerTests;

    assert(timerManagerTests.test00());
  }

  if(runAll || arg.compare("thread-manager") == 0) {

    std::cout << "ThreadManager tests..." << std::endl;

    size_t workerCount = 100;

    size_t taskCount = 100000;

    long long delay = 10LL;

    std::cout << "\t\tThreadManager load test: worker count: " << workerCount << " task count: " << taskCount << " delay: " << delay << std::endl;

    ThreadManagerTests threadManagerTests;

    assert(threadManagerTests.loadTest(taskCount, delay, workerCount));
  }
}

