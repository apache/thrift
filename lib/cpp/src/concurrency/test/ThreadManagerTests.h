#include <config.h>
#include <concurrency/ThreadManager.h>
#include <concurrency/PosixThreadFactory.h>
#include <concurrency/Monitor.h>
#include <concurrency/Util.h>

#include <assert.h>
#include <set>
#include <iostream>
#include <set>
#include <stdint.h>

namespace facebook { namespace thrift { namespace concurrency { namespace test {

using namespace facebook::thrift::concurrency;

/** ThreadManagerTests class 

    @author marc
    @version $Id:$ */

class ThreadManagerTests {

public:

  static const double ERROR;

  class Task: public Runnable {

  public:
    
    Task(Monitor& monitor, size_t& count, long long timeout) : 
      _monitor(monitor),
      _count(count),
      _timeout(timeout),
      _done(false) {}

    void run() {

      _startTime = Util::currentTime();

      {Synchronized s(_sleep);


	_sleep.wait(_timeout);
      }

      _endTime = Util::currentTime();

      _done = true;
      
      {Synchronized s(_monitor);

	// std::cout << "Thread " << _count << " completed " << std::endl;
      
	_count--;

	if(_count == 0) {
	  
	  _monitor.notify();
	}
      }
    }
    
    Monitor& _monitor;
    size_t& _count;
    long long _timeout;
    long long _startTime;
    long long _endTime;
    bool _done;
    Monitor _sleep;
  };

  /** Dispatch count tasks, each of which blocks for timeout milliseconds then completes.
      Verify that all tasks completed and that thread manager cleans up properly on delete. */

  bool loadTest(size_t count=100, long long timeout=100LL, size_t workerCount=4) {

    Monitor monitor;

    size_t activeCount = count;

    shared_ptr<ThreadManager> threadManager = ThreadManager::newSimpleThreadManager(workerCount);

    shared_ptr<PosixThreadFactory> threadFactory = shared_ptr<PosixThreadFactory>(new PosixThreadFactory());

    threadFactory->priority(PosixThreadFactory::HIGHEST);
      
    threadManager->threadFactory(threadFactory);

    threadManager->start();
      
    std::set<shared_ptr<ThreadManagerTests::Task> > tasks;

    for(size_t ix = 0; ix < count; ix++) {

      tasks.insert(shared_ptr<ThreadManagerTests::Task>(new ThreadManagerTests::Task(monitor, activeCount, timeout)));
    }

    long long time00 = Util::currentTime();

    for(std::set<shared_ptr<ThreadManagerTests::Task> >::iterator ix = tasks.begin(); ix != tasks.end(); ix++) {

	threadManager->add(*ix);
    }

    {Synchronized s(monitor);
      
      while(activeCount > 0) {
	
	monitor.wait();
      }
    }

    long long time01 = Util::currentTime();

    long long firstTime = 9223372036854775807LL;
    long long lastTime = 0;

    double averageTime = 0;
    long long minTime = 9223372036854775807LL;
    long long maxTime = 0;

    for(std::set<shared_ptr<ThreadManagerTests::Task> >::iterator ix = tasks.begin(); ix != tasks.end(); ix++) {
      
      shared_ptr<ThreadManagerTests::Task> task = *ix;

      long long delta = task->_endTime - task->_startTime;

      assert(delta > 0);

      if(task->_startTime < firstTime) {
	firstTime = task->_startTime;
      }

      if(task->_endTime > lastTime) {
	lastTime = task->_endTime;
      }

      if(delta < minTime) {
	minTime = delta;
      }

      if(delta > maxTime) {
	maxTime = delta;
      }

      averageTime+= delta;
    }
    
    averageTime /= count;

    std::cout << "\t\t\tfirst start: " << firstTime << "ms Last end: " << lastTime << "ms min: " << minTime << "ms max: " << maxTime << "ms average: " << averageTime << "ms" << std::endl;

    double expectedTime = ((count + (workerCount - 1)) / workerCount) * timeout;

    double error = ((time01 - time00) - expectedTime) / expectedTime;

    if(error < 0) {
      error*= -1.0;
    }

    bool success = error < ERROR;

    std::cout << "\t\t\t" << (success ? "Success" : "Failure") << "! expected time: " << expectedTime << "ms elapsed time: "<< time01 - time00 << "ms error%: " << error * 100.0 << std::endl;

    return success;
  }
};

const double ThreadManagerTests::ERROR = .20;

}}}} // facebook::thrift::concurrency

using namespace facebook::thrift::concurrency::test;

