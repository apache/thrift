#include "TimerManager.h"
#include "Util.h"

#include <assert.h>

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

  STATE _state;
};

class TimerManager::Dispatcher: public Runnable {

  enum STATE {
    UNINITIALIZED,
    STARTING,
    STARTED,
    STOPPING,
    STOPPED
  };
  
public:
  Dispatcher(TimerManager* manager) : 
    _manager(manager),
    _state(UNINITIALIZED)
  {}
  
  ~Dispatcher() {}
  
  /** Dispatcher entry point

      As long as dispatcher thread is running, pull tasks off the task _taskMap and execute. */

  void run() {

    {Synchronized(_manager->_monitor);

      if(_state == STARTING) {
	_state = STARTED;
      }
    }

    do {

      std::set<TimerManager::Task*> expiredTasks;

      {Synchronized(_manager->_monitor);
	
	long long now = Util::currentTime();

	task_iterator expiredTaskEnd;
	
	while(_state == STARTED && 
	      (expiredTaskEnd = _manager->_taskMap.upper_bound(now)) == _manager->_taskMap.end()) {
	  
	  _manager->_monitor.wait(_manager->_nextTimeout - now);
	  
	}
	
	if(_state == STARTED) {
	  
	  for(task_iterator ix = _manager->_taskMap.begin(); ix != expiredTaskEnd; ix++) {

	    TimerManager::Task* task = ix->second;
	    
	    expiredTasks.insert(task);
	    
	    _manager->_taskCount--;
	  }
	  
	  _manager->_taskMap.erase(_manager->_taskMap.begin(), expiredTaskEnd);
	}
      }
      
      for(std::set<Task*>::iterator ix =  expiredTasks.begin(); ix != expiredTasks.end(); ix++) {
	
	(*ix)->run();

	delete *ix;
      }
      
    } while(_state == STARTED);

    {Synchronized(_manager->_monitor);

      if(_state == STOPPING) {

	_state = STOPPED; 

	_manager->_monitor.notify();

      }
    }
    
    return;
  }

 private:

  TimerManager* _manager;

  friend class TimerManager;

  STATE _state;
};

TimerManager::TimerManager() {}

TimerManager::~TimerManager() {}

const ThreadFactory* TimerManager::threadFactory() const {

  Synchronized s(_monitor); 

  return _threadFactory;
}
      
void TimerManager::threadFactory(const ThreadFactory*  value) {
    
  Synchronized s(_monitor); 
  
  _threadFactory = value;
}

void TimerManager::add(Runnable* task, long long timeout) {

  long long now = Util::currentTime();

  timeout += now;

  {Synchronized s(_monitor); 

    _taskCount++;

    _taskMap.insert(std::pair<long long, Task*>(timeout, new Task(task)));

    /* If the task map was empty, or if we have an expiration that is earlier than any previously seen,
       kick the dispatcher so it can update its timeout */

    if(_taskCount == 1 || timeout < _nextTimeout) {

      _monitor.notify();
    }
    
    if(timeout < _nextTimeout) {

      _nextTimeout = timeout;
    }
  }
}

void TimerManager::add(Runnable* task, const struct timespec& value) {

  long long  expiration;

  Util::toMilliseconds(expiration, value);

  /* XXX
     Need to convert this to an explicit exception */

  long long now = Util::currentTime();

  assert(expiration < now);

  add(task, expiration - now);
}


void TimerManager::remove(Runnable* task) {

}

}}} // facebook::thrift::concurrency

