#include "ThreadManager.h"
#include "Exception.h"
#include "Monitor.h"

#include <assert.h>
#include <queue>
#include <set>

namespace facebook { namespace thrift { namespace concurrency { 

/** ThreadManager class
    
    This class manages a pool of threads.  It uses a ThreadFactory to create threads.  It never actually creates or destroys worker threads, rather
    it maintains statistics on number of idle threads, number of active threads, task backlog, and average wait and service times.

    @author marc
    @version $Id:$ */

class ThreadManager::Impl : public ThreadManager  {

 public:

  Impl() : _stopped(false) {}



  ~Impl() {

    if(!_stopped) {
      stop();
    }
  }

  void stop();

  const ThreadFactory* threadFactory() const {

    Synchronized s(_monitor); 

    return _threadFactory;
  }
      
  void threadFactory(const ThreadFactory*  value) {
    
    Synchronized s(_monitor); 
    
    _threadFactory = value;
  }

  void addWorker(size_t value);
  
  void removeWorker(size_t value);
  
  size_t idleWorkerCount() const {return _idleCount;}

  size_t workerCount() const {

    Synchronized s(_monitor); 

    return _workerCount;
  }
  
  size_t pendingTaskCount() const {

    Synchronized s(_monitor); 

    return _tasks.size();
  }

  size_t totalTaskCount() const {

    Synchronized s(_monitor); 
    
    return _tasks.size() + _workerCount - _idleCount;
  }
  
  void add(Runnable* value);

  void remove(Runnable* task);

private:

  size_t _workerCount;

  size_t _idleCount;

  bool _stopped;

  const ThreadFactory* _threadFactory;

  friend class ThreadManager::Task;

  std::queue<Task*> _tasks;

  Monitor _monitor;

  friend class ThreadManager::Worker;

  std::set<Thread*> _workers;
};

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
  
  friend class ThreadManager::Worker;

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
  Worker(ThreadManager::Impl* manager) : 
    _manager(manager),
    _state(UNINITIALIZED),
    _idle(false)
  {}

  ~Worker() {}

  /** Worker entry point

      As long as worker thread is running, pull tasks off the task queue and execute. */

  void run() {

    {Synchronized s(_manager->_monitor);

      if(_state == STARTING) {
	_state = STARTED;
      }

      _manager->_workerCount++;

      _manager->_monitor.notifyAll();
    }

    do {

      ThreadManager::Task* task = NULL;

      /* While holding manager monitor block for non-empty task queue (Also check that the thread hasn't been requested to stop).

	 Once the queue is non-empty, dequeue a task, release monitor, and execute. */

      {Synchronized s(_manager->_monitor);

	while(_state == STARTED && _manager->_tasks.empty()) {
	  
	  _manager->_idleCount++;

	  _idle = true;
	  
	  _manager->_monitor.wait();

	  _idle = false;
	  
	  _manager->_idleCount--;
	}
	
	if(_state == STARTED) {

	  if(!_manager->_tasks.empty()) {
	  
	    task = _manager->_tasks.front();

	    _manager->_tasks.pop();

	    if(task->_state == ThreadManager::Task::WAITING) {

	      task->_state = ThreadManager::Task::EXECUTING;
	    }
	  }
	}
      }

      if(task != NULL) {

	if(task->_state == ThreadManager::Task::EXECUTING) {
	  try {
	    
	    task->run();

	  } catch(...) {
	    
	    // XXX need to log this
	  }
	  
	  delete task;
	}
      }
      
    } while(_state == STARTED);

    {Synchronized s(_manager->_monitor);

      _manager->_workerCount--;

      if(_state == STOPPING) {

	_state = STOPPED; 

	_manager->_monitor.notify();

      }
    }
    
    return;
  }

 private:

  ThreadManager::Impl* _manager;

  friend class ThreadManager::Impl;

  STATE _state;

  bool _idle;
};

void ThreadManager::Impl::addWorker(size_t value) {
    
  std::set<Thread*> newThreads;
  
  for(size_t ix = 0; ix < value; ix++) {

    class ThreadManager::Worker;
      
    ThreadManager::Worker* worker = new ThreadManager::Worker(this);

    newThreads.insert(_threadFactory->newThread(worker));
  }
  
  for(std::set<Thread*>::iterator ix = newThreads.begin(); ix != newThreads.end(); ix++) {

    ThreadManager::Worker* worker = (ThreadManager::Worker*)(*ix)->runnable();

    worker->_state = ThreadManager::Worker::STARTING;

    (*ix)->start();
  }

  {Synchronized s(_monitor); 
      
    _workers.insert(newThreads.begin(), newThreads.end());

    while(_workerCount != _workers.size()) {
      _monitor.wait();
    }
  }
}

void ThreadManager::Impl::stop() {

  bool doStop = false;

  {Synchronized s(_monitor); 

    if(!_stopped) {
      doStop = true;
      _stopped = true;
    }
  }

  if(doStop) {
    removeWorker(_workerCount);
  }
}
  
void ThreadManager::Impl::removeWorker(size_t value) {

  std::set<Thread*> removedThreads;

    {Synchronized s(_monitor); 
  
      /* Overly clever loop
	 
	 First time through, (idleOnly == 1) just look for idle threads.  If that didn't find enough, go through again (idleOnly == 0)
	 and remove a sufficient number of busy threads. */

      for(int idleOnly = 1; idleOnly >= 0; idleOnly--) {
      
	for(std::set<Thread*>::iterator workerThread = _workers.begin(); (workerThread != _workers.end()) && (removedThreads.size() < value); workerThread++) {

	  Worker* worker = (Worker*)(*workerThread)->runnable();

	  if(worker->_idle || !idleOnly) {

	    if(worker->_state == ThreadManager::Worker::STARTED) {

	      worker->_state = ThreadManager::Worker::STOPPING;
	    }

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
  
void ThreadManager::Impl::add(Runnable* value) {

    Synchronized s(_monitor); 

    bool isEmpty = _tasks.empty();

    _tasks.push(new ThreadManager::Task(value));

    /* If queue was empty notify a thread, otherwise all worker threads are running and will get around to this
       task in time. */

    if(isEmpty && _idleCount > 0) {

      _monitor.notify();
    }
  }

void ThreadManager::Impl::remove(Runnable* task) {

    Synchronized s(_monitor); 
}

class SimpleThreadManager : public ThreadManager::Impl {

public:

  SimpleThreadManager(size_t workerCount=4) : 
    _workerCount(workerCount),
    _firstTime(true) {
  }

  void add(Runnable* task) {

    bool addWorkers = false;

    {Synchronized s(_monitor);

      if(_firstTime) {

	_firstTime = false;

	addWorkers = true;
      }
    }

    if(addWorkers) {

      addWorker(_workerCount);
    }

    Impl::add(task);
  }

private:

  const size_t _workerCount;
  bool _firstTime;
  Monitor _monitor;
};


ThreadManager* ThreadManager::newThreadManager() {
   return new ThreadManager::Impl();
}

ThreadManager* ThreadManager::newSimpleThreadManager(size_t count) {
   return new SimpleThreadManager(count);
}

}}} // facebook::thrift::concurrency

