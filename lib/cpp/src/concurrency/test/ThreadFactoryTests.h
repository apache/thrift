#include <concurrency/Thread.h>
#include <concurrency/PosixThreadFactory.h>
#include <concurrency/Monitor.h>
#include <concurrency/Util.h>

#include <assert.h>
#include <iostream>
#include <set>

namespace facebook { namespace thrift { namespace concurrency { namespace test {

using namespace facebook::thrift::concurrency;

/** ThreadManagerTests class 

    @author marc
    @version $Id:$ */

class ThreadFactoryTests {

public:

  static const double ERROR;
  
  class Task: public Runnable {

  public:

    Task() {}

    void run() {
      std::cout << "\t\t\tHello World" << std::endl;
    }
  };

  /** Hello world test */

  bool helloWorldTest() {

    PosixThreadFactory threadFactory =  PosixThreadFactory();

    shared_ptr<Task> task = shared_ptr<Task>(new ThreadFactoryTests::Task());

    shared_ptr<Thread> thread = threadFactory.newThread(task);

    thread->start();

    thread->join();

    std::cout << "\t\t\tSuccess!" << std::endl;

    return true;
  }

  /** Reap N threads  */

 class ReapNTask: public Runnable {

  public:

  ReapNTask(Monitor& monitor, int& activeCount) :
    _monitor(monitor),
      _count(activeCount) {
      }
    
    void run() {

      {Synchronized s(_monitor);

	_count--;

	//std::cout << "\t\t\tthread count: " << _count << std::endl;

	if(_count == 0) {
	  _monitor.notify();
	}
      }
    }

    Monitor& _monitor;

    int& _count;
  };

  bool reapNThreads(int count=10) {

    Monitor* monitor = new Monitor();

    int* activeCount  = new int(count);

    PosixThreadFactory threadFactory =  PosixThreadFactory();

    std::set<shared_ptr<Thread> > threads;

    for(int ix = 0; ix < count; ix++) {
      threads.insert(threadFactory.newThread(shared_ptr<Runnable>(new ReapNTask(*monitor, *activeCount))));
    }

    for(std::set<shared_ptr<Thread> >::const_iterator thread = threads.begin(); thread != threads.end(); thread++) {

      (*thread)->start();
    }


    {Synchronized s(*monitor);

      while(*activeCount > 0) {
	monitor->wait(1000);
      }
    }

    for(std::set<shared_ptr<Thread> >::const_iterator thread = threads.begin(); thread != threads.end(); thread++) {

      threads.erase(*thread);
    }

    std::cout << "\t\t\tSuccess!" << std::endl;

    return true;
  }

 class SynchStartTask: public Runnable {

  public:

    enum STATE {
      UNINITIALIZED,
      STARTING,
      STARTED,
      STOPPING,
      STOPPED
    };

  SynchStartTask(Monitor& monitor,
		 volatile  STATE& state) :
    _monitor(monitor),
    _state(state) {
    }

    void run() {

      {Synchronized s(_monitor);

	if(_state == SynchStartTask::STARTING) {
	  _state = SynchStartTask::STARTED;
	  _monitor.notify();
	}
      }

      {Synchronized s(_monitor);
	
	while(_state == SynchStartTask::STARTED) {
	  _monitor.wait();
	}

	if(_state == SynchStartTask::STOPPING) {
	  
	  _state = SynchStartTask::STOPPED;
	  
	  _monitor.notifyAll();
	}
      }
    }

    private:
    Monitor& _monitor;
    volatile  STATE& _state;
  };

  bool synchStartTest() {

    Monitor monitor;
    
    SynchStartTask::STATE state = SynchStartTask::UNINITIALIZED;
    
    shared_ptr<SynchStartTask> task = shared_ptr<SynchStartTask>(new SynchStartTask(monitor, state));

    PosixThreadFactory threadFactory =  PosixThreadFactory();

    shared_ptr<Thread> thread = threadFactory.newThread(task);

    if(state == SynchStartTask::UNINITIALIZED) {

      state = SynchStartTask::STARTING;

      thread->start();
    }

    {Synchronized s(monitor);
      
      while(state == SynchStartTask::STARTING) {
	monitor.wait();
      }
    }

    assert(state != SynchStartTask::STARTING);

    {Synchronized s(monitor);

      monitor.wait(100);

      if(state == SynchStartTask::STARTED) {

	state = SynchStartTask::STOPPING;

	monitor.notify();
      }
      
      while(state == SynchStartTask::STOPPING) {
	monitor.wait();
      }
    }

    assert(state == SynchStartTask::STOPPED);

    bool success = true;

    std::cout << "\t\t\t" << (success ? "Success" : "Failure") << "!" << std::endl;

    return true;
  }

  /** See how accurate monitor timeout is. */

  bool monitorTimeoutTest(size_t count=1000, long long timeout=10) {

    Monitor monitor;

    long long startTime = Util::currentTime();

    for(size_t ix = 0; ix < count; ix++) {
      {Synchronized s(monitor);
	monitor.wait(timeout);
      }
    }

    long long endTime = Util::currentTime();

    double error = ((endTime - startTime) - (count * timeout)) / (double)(count * timeout);

    if(error < 0.0)  {

      error *= 1.0;
    }

    bool success = error < ThreadFactoryTests::ERROR;

    std::cout << "\t\t\t" << (success ? "Success" : "Failure") << "! expected time: " << count * timeout << "ms elapsed time: "<< endTime - startTime << "ms error%: " << error * 100.0 << std::endl;

    return success;
  }
};

const double ThreadFactoryTests::ERROR = .20;

}}}} // facebook::thrift::concurrency::test

