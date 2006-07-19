#include "ThreadManager.h"
#include "Monitor.h"

#include <assert.h>
#include <queue>
#include <set>

namespace facebook { namespace thrift { namespace concurrency { 

/** ThreadManager class
    
    This class manages a pool of threads.  It uses a ThreadFactory to create threads.  It never actually creates or destroys worker threads, rather
    it maintains statistics on number of idle threads, number of active threads, task backlog, and average wait and service times and informs the
    PoolPolicy object bound to instances of this manager of interesting transitions.  It is then up the PoolPolicy object to decide if the thread pool
    size needs to be adjusted and call this object addThread and removeThread methods to make changes.

    This design allows different policy implementations to used this code to handle basic worker thread management and worker task execution and focus on
    policy issues.  The simplest policy, StaticPolicy, does nothing other than create a fixed number of threads.

    @author marc
    @version $Id:$ */

class ThreadManager::Impl : public ThreadManager  {

 public:

  Impl(size_t highWatermark, size_t lowWatermark) : 
    _hiwat(highWatermark), 
    _lowat(lowWatermark) {
  }

  ~Impl() {}

  size_t highWatermark() const {return _hiwat;}

  void highWatermark(size_t value) {_hiwat = value;}

  size_t lowWatermark() const {return _lowat;}

  void lowWatermark(size_t value) {_lowat = value;}

  const PoolPolicy* poolPolicy() const {

    Synchronized s(_monitor); 
  
    return _poolPolicy;
  }
  
  void poolPolicy(const PoolPolicy*  value) {

    Synchronized s(_monitor); 

    _poolPolicy = value;
  }

  const ThreadFactory* threadFactory() const {

    Synchronized s(_monitor); 

    return _threadFactory;
  }
      
  void threadFactory(const ThreadFactory*  value) {
    
    Synchronized s(_monitor); 
    
    _threadFactory = value;
  }

  void addThread(size_t value);
  
  void removeThread(size_t value);
  
  size_t idleWorkerCount() const {return _idleCount;}

  size_t workerCount() const {

    Synchronized s(_monitor); 

    return _workers.size();
  }
  
  size_t pendingTaskCount() const {

    Synchronized s(_monitor); 

    return _tasks.size();
  }

  size_t totalTaskCount() const {

    Synchronized s(_monitor); 
    
    return _tasks.size() + _workers.size() - _idleCount;
  }
  
  void add(Runnable* value);

  void remove(Runnable* task);

private:

  size_t _hiwat;

  size_t _lowat;

  size_t _idleCount;

  const PoolPolicy* _poolPolicy;;

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
	  
	  task = _manager->_tasks.front();
	}
      }

      if(task != NULL) {

	task->run();

	delete task;
      }

    } while(_state == STARTED);

    {Synchronized s(_manager->_monitor);

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

void ThreadManager::Impl::addThread(size_t value) {
    
    std::set<Thread*> newThreads;
  
    for(size_t ix = 0; ix < value; ix++) {

      class ThreadManager::Worker;
      
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
  
void ThreadManager::Impl::removeThread(size_t value) {

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
  
void ThreadManager::Impl::add(Runnable* value) {

    Synchronized s(_monitor); 

    _tasks.push(new ThreadManager::Task(value));

    /* If queue is empty notify a thread, otherwise all worker threads are running and will get around to this
       task in time. */

    if(_tasks.size() == 1) {

      assert(_idleCount == _workers.size());

      _monitor.notify();
    }
  }

void ThreadManager::Impl::remove(Runnable* task) {

    Synchronized s(_monitor); 
  }

ThreadManager* ThreadManager::newThreadManager(size_t lowWatermark, size_t highWatermark) {
  return new ThreadManager::Impl(lowWatermark, highWatermark);
}

/**  Basic Pool Policy Implementation */

class BasicPoolPolicy::Impl : public PoolPolicy {

 public:

  Impl() {}

  ~Impl() {}

  void onEmpty(ThreadManager* source) const  {}

  void onLowWatermark(ThreadManager* source) const  {}

  void onHighWatermark(ThreadManager* source) const {}
};

BasicPoolPolicy::BasicPoolPolicy() : _impl(new BasicPoolPolicy::Impl())  {}

BasicPoolPolicy::~BasicPoolPolicy() { delete _impl;}

void BasicPoolPolicy::onEmpty(ThreadManager* source) const {_impl->onEmpty(source);}

void BasicPoolPolicy::onLowWatermark(ThreadManager* source) const {_impl->onLowWatermark(source);}

void BasicPoolPolicy::onHighWatermark(ThreadManager* source) const {_impl->onHighWatermark(source);}


}}} // facebook::thrift::concurrency

