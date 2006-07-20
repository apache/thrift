#include "TimerManager.h"
#include "Exception.h"
#include "Util.h"

#include <assert.h>
#include <iostream>
#include <set>

namespace facebook { namespace thrift { namespace concurrency { 

/** TimerManager class
    
    @author marc
    @version $Id:$ */

typedef std::multimap<long long, TimerManager::Task*>::iterator task_iterator;
typedef std::pair<task_iterator, task_iterator> task_range;

class TimerManager::Task : public Runnable {

public:
  enum STATE {
    WAITING,
    EXECUTING,
    CANCELLED,
    COMPLETE
  };

  Task(Runnable* runnable) :
    _runnable(runnable),
    _state(WAITING) 
  {}
  
  ~Task() {};
  
  void run() {
    if(_state == EXECUTING) {
      _runnable->run();
      _state = COMPLETE;
    }
  }

 private:

  Runnable* _runnable;

  class TimerManager::Dispatcher;

  friend class TimerManager::Dispatcher;

  STATE _state;
};

class TimerManager::Dispatcher: public Runnable {

public:
  Dispatcher(TimerManager* manager) : 
    _manager(manager) {
}
  
  ~Dispatcher() {}
  
  /** Dispatcher entry point

      As long as dispatcher thread is running, pull tasks off the task _taskMap and execute. */

  void run() {

    {Synchronized s(_manager->_monitor);

      if(_manager->_state == TimerManager::STARTING) {

	_manager->_state = TimerManager::STARTED;

	_manager->_monitor.notifyAll();
      }
    }

    do {

      std::set<TimerManager::Task*> expiredTasks;

      {Synchronized s(_manager->_monitor);

	task_iterator expiredTaskEnd;

	long long now = Util::currentTime();

	while(_manager->_state == TimerManager::STARTED && 
	      (expiredTaskEnd = _manager->_taskMap.upper_bound(now)) == _manager->_taskMap.begin()) {

	  long long timeout = 0LL;

	  if(!_manager->_taskMap.empty()) {

	    timeout = _manager->_taskMap.begin()->first - now;
	  }

	  assert((timeout != 0 && _manager->_taskCount > 0) || (timeout == 0 && _manager->_taskCount == 0));
	    
 	  _manager->_monitor.wait(timeout);

	  now = Util::currentTime();
	}
	
	if(_manager->_state == TimerManager::STARTED) {
	  
	  for(task_iterator ix = _manager->_taskMap.begin(); ix != expiredTaskEnd; ix++) {

	    TimerManager::Task* task = ix->second;
	    
	    expiredTasks.insert(task);

	    if(task->_state == TimerManager::Task::WAITING) {

	      task->_state = TimerManager::Task::EXECUTING;
	    }
	    
	    _manager->_taskCount--;
	  }
	  
	  _manager->_taskMap.erase(_manager->_taskMap.begin(), expiredTaskEnd);
	}
      }
      
      for(std::set<Task*>::iterator ix =  expiredTasks.begin(); ix != expiredTasks.end(); ix++) {
	
	(*ix)->run();

	delete *ix;
      }
      
    } while(_manager->_state == TimerManager::STARTED);

    {Synchronized s(_manager->_monitor);

      if(_manager->_state == TimerManager::STOPPING) {

	_manager->_state = TimerManager::STOPPED; 

	_manager->_monitor.notify();

      }
    }
    
    return;
  }

 private:

  TimerManager* _manager;

  friend class TimerManager;
};

TimerManager::TimerManager() :
  _taskCount(0),
  _state(TimerManager::UNINITIALIZED),
  _dispatcher(new Dispatcher(this)) {
}


TimerManager::~TimerManager() {

  /* If we haven't been explicitly stopped, do so now.  We don't need to grab the monitor here, since
     stop already takes care of reentrancy. */
  
  if(_state != STOPPED) {
    
    try {
      
      stop();
      
    } catch(...) {
      
      // uhoh
      
    }
  }
}

void TimerManager::start() {

  bool doStart = false;

  {Synchronized s(_monitor);

    if(_threadFactory == NULL) {throw InvalidArgumentException();}

    if(_state == TimerManager::UNINITIALIZED) {

      _state = TimerManager::STARTING;

      doStart = true;
    }
  }

  if(doStart) {

    _dispatcherThread = _threadFactory->newThread(_dispatcher);

    _dispatcherThread->start();
  }

  {Synchronized s(_monitor);

    while(_state == TimerManager::STARTING) {

      _monitor.wait();
    }
    
    assert(_state != TimerManager::STARTING);
  }
}

void TimerManager::stop() {

  bool doStop = false;

  {Synchronized s(_monitor);

    if(_state == TimerManager::UNINITIALIZED) {

      _state = TimerManager::STOPPED;

    } else if(_state != STOPPING &&  _state != STOPPED) {

      doStop = true;

      _state = STOPPING;

      _monitor.notifyAll();
    }

    while(_state != STOPPED) {

      _monitor.wait();
    }
  }

  if(doStop) {

    // Clean up any outstanding tasks

    for(task_iterator ix =  _taskMap.begin(); ix != _taskMap.end(); ix++) {

      delete ix->second;

      _taskMap.erase(ix);
    }

    delete _dispatcher;
  }
}

const ThreadFactory* TimerManager::threadFactory() const {

  Synchronized s(_monitor); 

  return _threadFactory;
}
      
void TimerManager::threadFactory(const ThreadFactory*  value) {
    
  Synchronized s(_monitor); 
  
  _threadFactory = value;
}

size_t TimerManager::taskCount() const {

  return _taskCount;
}
      
void TimerManager::add(Runnable* task, long long timeout) {

  long long now = Util::currentTime();

  timeout += now;

  {Synchronized s(_monitor); 

    if(_state != TimerManager::STARTED) {
      throw IllegalStateException();
    }

    _taskCount++;

    _taskMap.insert(std::pair<long long, Task*>(timeout, new Task(task)));

    /* If the task map was empty, or if we have an expiration that is earlier than any previously seen,
       kick the dispatcher so it can update its timeout */

    if(_taskCount == 1 || timeout < _taskMap.begin()->first) {

      _monitor.notify();
    }
  }
}

void TimerManager::add(Runnable* task, const struct timespec& value) {

  long long  expiration;

  Util::toMilliseconds(expiration, value);

  long long now = Util::currentTime();

  if(expiration < now) {
    throw  InvalidArgumentException();
  }

  add(task, expiration - now);
}


void TimerManager::remove(Runnable* task) {
  {Synchronized s(_monitor); 

    if(_state != TimerManager::STARTED) {
      throw IllegalStateException();
    }
  }
}

const TimerManager::STATE TimerManager::state() const { return _state;}

}}} // facebook::thrift::concurrency

