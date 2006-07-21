#include <ThreadManager.h>
#include <PosixThreadFactory.h>
#include <Monitor.h>
#include <Util.h>

#include <assert.h>
#include <set>
#include <iostream>

namespace facebook { namespace thrift { namespace concurrency { namespace test {

using namespace facebook::thrift::concurrency;

/** ThreadManagerTests class 

    @author marc
    @version $Id:$ */

class ThreadManagerTests {

public:

  class Task: public Runnable {

  public:
    
    Task(Monitor& monitor, size_t& count, long long timeout) : 
      _monitor(monitor),
      _count(count),
      _timeout(timeout),
      _done(false) {}

    void run() {

      Monitor sleep;

      {Synchronized s(sleep);

	long long time00 = Util::currentTime();

	sleep.wait(_timeout);

	long long time01 = Util::currentTime();

	double error = ((time01 - time00) - _timeout) / (double)_timeout;
	
	if(error < 0.0) {
	  
	  error*= -1.0;
	}

	if(error > .20) {
	  
	  assert(false);
	}
      }

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
    bool _done;
  };

  /** Dispatch count tasks, each of which blocks for timeout milliseconds then completes.
      Verify that all tasks completed and that thread manager cleans up properly on delete. */

  bool loadTest(size_t count=100, long long timeout=100LL, size_t workerCount=4) {

    Monitor monitor;

    size_t activeCount = count;

    ThreadManager* threadManager = ThreadManager::newSimpleThreadManager(workerCount);
      
    threadManager->threadFactory(new PosixThreadFactory());

    threadManager->start();
      
    std::set<ThreadManagerTests::Task*> tasks;

    for(size_t ix = 0; ix < count; ix++) {

      tasks.insert(new ThreadManagerTests::Task(monitor, activeCount, timeout));
    }

    long long time00 = Util::currentTime();

    for(std::set<ThreadManagerTests::Task*>::iterator ix = tasks.begin(); ix != tasks.end(); ix++) {

	threadManager->add(*ix);
    }

    {Synchronized s(monitor);
      
      while(activeCount > 0) {
	
	monitor.wait();
      }
    }

    long long time01 = Util::currentTime();

    for(std::set<ThreadManagerTests::Task*>::iterator ix = tasks.begin(); ix != tasks.end(); ix++) {

      delete *ix;
      
    }

    double expectedTime = ((count + (workerCount - 1)) / workerCount) * timeout;

    double error = ((time01 - time00) - expectedTime) / expectedTime;

    if(error < 0) {
      error*= -1.0;
    }

    bool success = error < .10;

    delete threadManager;

    std::cout << "\t\t\t" << (success ? "Success" : "Failure") << "! expected time: " << expectedTime << "ms elapsed time: "<< time01 - time00 << "ms error%: " << error * 100.0 << std::endl;

    return true;
  }
};
  
}}}} // facebook::thrift::concurrency

using namespace facebook::thrift::concurrency::test;

