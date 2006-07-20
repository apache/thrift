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
      _addTime(Util::currentTime()),
      _success(false),
      _done(false) {}

    void run() {

      _startTime = Util::currentTime();
      
      Monitor sleep;

      {Synchronized s(sleep);

	sleep.wait(_timeout);
      }

      _endTime = Util::currentTime();

      _done = true;
      
      _success = true;

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
    long long _addTime;
    long long _startTime;
    long long _endTime;
    bool _success;
    bool _done;
  };

  /** Dispatch count tasks, each of which blocks for timeout milliseconds then completes.
      Verify that all tasks completed and that thread manager cleans up properly on delete. */

  bool test00(size_t count=100, long long timeout=100LL, size_t workerCount=4) {

    Monitor monitor;

    size_t activeCount = count;

    ThreadManager* threadManager = ThreadManager::newSimpleThreadManager(workerCount);
      
    threadManager->threadFactory(new PosixThreadFactory());
      
    std::set<ThreadManagerTests::Task*> tasks;

    for(size_t ix = 0; ix < count; ix++) {

      tasks.insert(new ThreadManagerTests::Task(monitor, activeCount, timeout));
    }

    for(std::set<ThreadManagerTests::Task*>::iterator ix = tasks.begin(); ix != tasks.end(); ix++) {

	threadManager->add(*ix);
    }

    {Synchronized s(monitor);
      
      while(activeCount > 0) {
	
	monitor.wait();
      }
    }

    bool success;

    for(std::set<ThreadManagerTests::Task*>::iterator ix = tasks.begin(); ix != tasks.end(); ix++) {

      success = success || (*ix)->_success;

      delete *ix;
      
    }

    delete threadManager;

    std::cout << "\t\t\t" << (success ? "Success" : "Failure") << "!" << std::endl;

    return true;
  }
};
  
}}}} // facebook::thrift::concurrency

using namespace facebook::thrift::concurrency::test;

