#include <TimerManager.h>
#include <PosixThreadFactory.h>
#include <Monitor.h>
#include <Util.h>

#include <assert.h>
#include <iostream>

namespace facebook { namespace thrift { namespace concurrency { namespace test {

using namespace facebook::thrift::concurrency;

/** ThreadManagerTests class 

    @author marc
    @version $Id:$ */

class TimerManagerTests {

 class Task: public Runnable {

  public:
    
  Task(Monitor& monitor, long long timeout) : 
    _timeout(timeout),
    _startTime(Util::currentTime()),
    _monitor(monitor),
    _success(false),
    _done(false) {}

    void run() {

      _endTime = Util::currentTime();

      // Figure out error percentage

      long long delta = _endTime - _startTime;


      delta = delta > _timeout ?  delta - _timeout : _timeout - delta;

      float error = delta / _timeout;

      if(error < .10) {
	_success = true;
      }
      
      std::cout << "\t\t\tHello World" << std::endl;

      _done = true;
      
      {Synchronized s(_monitor);
	_monitor.notifyAll();
      }
    }


    long long _timeout;
    long long _startTime;
    long long _endTime;
    Monitor& _monitor;
    bool _success;
    bool _done;
  };

public:

  /** This test creates two tasks and waits for the first to expire within 10% of the expected expiration time.  It then verifies that
      the timer manager properly clean up itself and the remaining orphaned timeout task when the manager goes out of scope and its 
      destructor is called. */

  bool test00(long long timeout=1000LL) {

    TimerManagerTests::Task* orphanTask = new TimerManagerTests::Task(_monitor, 10 * timeout);

    {

      TimerManager timerManager;
      
      timerManager.threadFactory(new PosixThreadFactory());
      
      timerManager.start();
      
      assert(timerManager.state() == TimerManager::STARTED);

      TimerManagerTests::Task* task = new TimerManagerTests::Task(_monitor, timeout);

      {Synchronized s(_monitor);

	timerManager.add(orphanTask, 10 * timeout);

	timerManager.add(task, timeout);

	_monitor.wait();
      }

      assert(task->_done);


      std::cout << "\t\t\t" << (task->_success ? "Success" : "Failure") << "!" << std::endl;

      delete task;
    }

    // timerManager.stop(); This is where it happens via destructor

    assert(!orphanTask->_done);

    delete orphanTask;

    return true;
  }

  friend class TestTask;

  Monitor _monitor;
};
  

}}}} // facebook::thrift::concurrency

using namespace facebook::thrift::concurrency::test;

