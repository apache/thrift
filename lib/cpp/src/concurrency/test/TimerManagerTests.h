#include <TimerManager.h>
#include <PosixThreadFactory.h>
#include <Monitor.h>

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
    
  Task(Monitor& monitor) : 
    _monitor(monitor),
    _done(false) {}

    void run() {

      std::cout << "\t\t\tHello World" << std::endl;

      _done = true;
      
      {Synchronized s(_monitor);
	_monitor.notifyAll();
      }
    }
    
    Monitor& _monitor;
    bool _done;
  };

public:

  bool test00() {

    TimerManager* timerManager =  new TimerManager();

    timerManager->threadFactory(new PosixThreadFactory());

    timerManager->start();

    assert(timerManager->state() == TimerManager::STARTED);

    TimerManagerTests::Task* task = new TimerManagerTests::Task(_monitor);

    {Synchronized s(_monitor);

      timerManager->add(task, 1000LL);

      _monitor.wait();
    }

    assert(task->_done);

    delete task;

    std::cout << "\t\t\tSuccess!" << std::endl;

    return true;
  }

  friend class TestTask;

  Monitor _monitor;
};
  

}}}} // facebook::thrift::concurrency

using namespace facebook::thrift::concurrency::test;

