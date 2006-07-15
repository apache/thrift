#include "ThreadManager.h"

#include <assert.h>

namespace facebook { namespace thrift { namespace concurrency { 

/** ThreadManager class
    
    This class manages a pool of threads.  It uses a ThreadFactory to create threads.  It never actually creates or destroys worker threads, rather
    it maintains statistics on number of idle threads, number of active threads, task backlog, and average wait and service times and informs the
    PoolPolicy object bound to instances of this manager of interesting transitions.  It is then up the PoolPolicy object to decide if the thread pool
    size needs to be adjusted and call this object addThread and removeThread methods to make changes.

    This design allows different policy implementations to used this code to handle basic worker thread management and worker task execution and focus on
    policy issues.  The simplest policy, StaticPolicy, does nothing other than create a fixed number of threads.

    @author marc
    @version $Id */

class ThreadManager::Task : public Runnable {

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

class ThreadManager::Worker: public Runnable {

  enum STATE {
    UNINITIALIZED,
    STARTING,
    STARTED,
    STOPPING,
    STOPPED
  };

 public:
  Worker(ThreadManager* manager) : 
    _manager(manager),
    _state(UNINITIALIZED),
    _idle(false)
  {}

  ~Worker() {}

  /** Worker entry point

      As long as worker thread is running, pull tasks off the task queue and execute. */

  void run() {

    {Synchronized(_manager->_monitor);

      if(_state == STARTING) {
	_state = STARTED;
      }
    }

    do {

      ThreadManager::Task* task = NULL;

      /* While holding manager monitor block for non-empty task queue (Also check that the thread hasn't been requested to stop).

	 Once the queue is non-empty, dequeue a task, release monitor, and execute. */

      {Synchronized(_manager->_monitor);

	while(_state == STARTED && _manager->_tasks.empty()) {
	  
	  _manager->_idleCount++;

	  _idle = true;
	  
	  _manager->_monitor.wait();

	  _idle = false;
	  
	  _manager->_idleCount--;
	}
	
	if(_state == STARTED) {
	  
	  task = _manager->_tasks.front();
	}
      }

      if(task != NULL) {

	task->run();

	delete task;
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

  ThreadManager* _manager;

  friend class ThreadManager;

  STATE _state;

  bool _idle;
};

ThreadManager::ThreadManager(size_t highWatermark, size_t lowWatermark) : 
  _hiwat(highWatermark), 
  _lowat(lowWatermark) {
}

ThreadManager::~ThreadManager() {}

size_t ThreadManager::ThreadManager::highWatermark() const {return _hiwat;}

void ThreadManager::highWatermark(size_t value) {_hiwat = value;}

size_t ThreadManager::lowWatermark() const {return _lowat;}

void ThreadManager::lowWatermark(size_t value) {_lowat = value;}

const PoolPolicy* ThreadManager::poolPolicy() const {

  Synchronized s(_monitor); 
  
  return _poolPolicy;
}
  
void ThreadManager::poolPolicy(const PoolPolicy*  value) {

  Synchronized s(_monitor); 

  _poolPolicy = value;
}

const ThreadFactory* ThreadManager::threadFactory() const {

  Synchronized s(_monitor); 

  return _threadFactory;
}
      
void ThreadManager::threadFactory(const ThreadFactory*  value) {
    
  Synchronized s(_monitor); 
  
  _threadFactory = value;
}

void ThreadManager::addThread(size_t value) {

  std::set<Thread*> newThreads;
  
  for(size_t ix = 0; ix < value; ix++) {

    ThreadManager::Worker* worker = new ThreadManager::Worker(this);

    newThreads.insert(_threadFactory->newThread(worker));
  }
  
  for(std::set<Thread*>::iterator ix = newThreads.begin(); ix != newThreads.end(); ix++) {

    (*ix)->start();
  }
  for(std::set<Thread*>::iterator ix = newThreads.begin(); ix != newThreads.end(); ix++) {

    (*ix)->start();
  }

  {Synchronized s(_monitor); 

    _workers.insert(newThreads.begin(), newThreads.end());
  }
}

void ThreadManager::removeThread(size_t value) {

  std::set<Thread*> removedThreads;

  {Synchronized s(_monitor); 
  
    /* Overly clever loop

       First time through, (idleOnly == 1) just look for idle threads.  If that didn't find enough, go through again (idleOnly == 0)
       and remove a sufficient number of busy threads. */

    for(int idleOnly = 1; idleOnly <= 0; idleOnly--) {
      
      for(std::set<Thread*>::iterator workerThread = _workers.begin(); (workerThread != _workers.end()) && (removedThreads.size() < value); workerThread++) {

	Worker* worker = (Worker*)(*workerThread)->runnable();

	if(worker->_idle || !idleOnly) {
	
	  removedThreads.insert(*workerThread);
	
	  _workers.erase(workerThread);
	}
      }
    }
    
    _monitor.notifyAll();
  }


  // Join removed threads and free worker 

  for(std::set<Thread*>::iterator workerThread = removedThreads.begin(); workerThread != removedThreads.end(); workerThread++) {

    Worker* worker = (Worker*)(*workerThread)->runnable();

    (*workerThread)->join();

    delete worker;
  }
}

size_t ThreadManager::idleWorkerCount() const {return _idleCount;}

size_t ThreadManager::workerCount() const {

  Synchronized s(_monitor); 

  return _workers.size();
}

size_t ThreadManager::pendingTaskCount() const {

  Synchronized s(_monitor); 

  return _tasks.size();
}

size_t ThreadManager::totalTaskCount() const {

  Synchronized s(_monitor); 

  return _tasks.size() + _workers.size() - _idleCount;
}

void ThreadManager::add(Runnable* value) {

  Synchronized s(_monitor); 

  _tasks.push(new ThreadManager::Task(value));

  /* If queue is empty notify a thread, otherwise all worker threads are running and will get around to this
     task in time. */

  if(_tasks.size() == 1) {

    assert(_idleCount == _workers.size());

    _monitor.notify();
  }
}

void ThreadManager::remove(Runnable* task) {

  Synchronized s(_monitor); 
}

}}} // facebook::thrift::concurrency

