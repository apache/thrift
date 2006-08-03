#include "PosixThreadFactory.h"

#include <assert.h>
#include <pthread.h>

#include <iostream>

#include <boost/weak_ptr.hpp>

namespace facebook { namespace thrift { namespace concurrency {

using namespace boost;

/**  The POSIX thread class. 

     @author marc
     @version $Id:$ */

class PthreadThread: public Thread {

public:
  enum STATE {uninitialized, 
	      starting,
	      started,
	      stopping,
	      stopped
  };

  static const int MB = 1024 * 1024;

  static void* threadMain(void* arg);

private:

  pthread_t _pthread;

  STATE _state;

  int _policy;

  int _priority;

  int _stackSize;

  weak_ptr<PthreadThread> _self;

public:
  
  PthreadThread(int policy, int priority, int stackSize, shared_ptr<Runnable> runnable) : 
    _pthread(0),
    _state(uninitialized), 
    _policy(policy),
    _priority(priority),
    _stackSize(stackSize) {

    this->Thread::runnable(runnable);
  }

  ~PthreadThread() {
  }

  void start() {

    if(_state != uninitialized) {
      return;
    }

    _state = starting;

    pthread_attr_t thread_attr;

    assert(pthread_attr_init(&thread_attr) == 0);

    assert(pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_JOINABLE) == 0);

    // Set thread stack size

    assert(pthread_attr_setstacksize(&thread_attr, MB * _stackSize) == 0);

    // Set thread policy

    assert(pthread_attr_setschedpolicy(&thread_attr, _policy) == 0);

    struct sched_param sched_param;
    sched_param.sched_priority = _priority;

    // Set thread priority

    assert(pthread_attr_setschedparam(&thread_attr, &sched_param) == 0);

    shared_ptr<PthreadThread>* selfRef = new shared_ptr<PthreadThread>();

    *selfRef = _self.lock();

    assert(pthread_create(&_pthread, &thread_attr, threadMain, (void*)selfRef) == 0);
  }

  void join() {
    
    if(_state != stopped) {
      
      void* ignore;
      
      pthread_join(_pthread, &ignore);
    }
  }

  shared_ptr<Runnable> runnable() const {return Thread::runnable();}

  void runnable(shared_ptr<Runnable> value) {Thread::runnable(value);}

  void weakRef(shared_ptr<PthreadThread> self) {
    assert(self.get() == this);
    _self = weak_ptr<PthreadThread>(self);
  }
};

void* PthreadThread::threadMain(void* arg) {
  // XXX need a lock here when testing thread state

  shared_ptr<PthreadThread> thread = *(shared_ptr<PthreadThread>*)arg;

  delete reinterpret_cast<shared_ptr<PthreadThread>*>(arg);

  if(thread == NULL) {
    return (void*)0;
  }

  if(thread->_state != starting) {
    return (void*)0;
  }

  thread->_state = starting;

  thread->runnable()->run();

  if(thread->_state != stopping && thread->_state != stopped) {
    thread->_state = stopping;
  }
    
  return (void*)0;
}

/** POSIX Thread factory implementation */

class PosixThreadFactory::Impl {

private:

  POLICY _policy;

  PRIORITY _priority;

  int _stackSize;

  bool _detached;

  /** Converts generic posix thread schedule policy enums into pthread API values. */

  static int toPthreadPolicy(POLICY policy) {
    switch(policy) {
    case OTHER: return SCHED_OTHER; break;
    case FIFO: return SCHED_FIFO; break;
    case ROUND_ROBIN: return SCHED_RR; break;
    default: return SCHED_OTHER; break;
    }
  }

  /** Converts relative thread priorities to absolute value based on posix thread scheduler policy

      The idea is simply to divide up the priority range for the given policy into the correpsonding relative
      priority level (lowest..highest) and then pro-rate accordingly. */

  static int toPthreadPriority(POLICY policy, PRIORITY priority) {

    int pthread_policy = toPthreadPolicy(policy);

    int min_priority = sched_get_priority_min(pthread_policy);

    int max_priority = sched_get_priority_max(pthread_policy);

    int quanta = (HIGHEST - LOWEST) + 1;

    float stepsperquanta = (max_priority - min_priority) / quanta;

    if(priority <= HIGHEST) {

      return (int)(min_priority + stepsperquanta * priority);
    } else {

      // should never get here for priority increments.

      assert(false);

      return (int)(min_priority + stepsperquanta * NORMAL);
    }
  }

public:

  Impl(POLICY policy, PRIORITY priority, int stackSize, bool detached) : 
    _policy(policy),
    _priority(priority),
    _stackSize(stackSize),
    _detached(detached) {
  }

  /** Creates a new POSIX thread to run the runnable object 

      @param runnable A runnable object */

  shared_ptr<Thread> newThread(shared_ptr<Runnable> runnable) const {

    shared_ptr<PthreadThread> result = shared_ptr<PthreadThread>(new PthreadThread(toPthreadPolicy(_policy), toPthreadPriority(_policy, _priority), _stackSize, runnable));
    result->weakRef(result);
    runnable->thread(result);
    return result;
  }

  int stackSize() const { return _stackSize;}

  void stackSize(int value) { _stackSize = value;}

  PRIORITY priority() const { return _priority;}

  /** Sets priority.
      
      XXX
      Need to handle incremental priorities properly. */

  void priority(PRIORITY value) { _priority = value;}

};

PosixThreadFactory::PosixThreadFactory(POLICY policy, PRIORITY priority, int stackSize, bool detached) : 
  _impl(new PosixThreadFactory::Impl(policy, priority, stackSize, detached)) {}

shared_ptr<Thread> PosixThreadFactory::newThread(shared_ptr<Runnable> runnable) const {return _impl->newThread(runnable);}

int PosixThreadFactory::stackSize() const {return _impl->stackSize();}

void PosixThreadFactory::stackSize(int value) {_impl->stackSize(value);}

PosixThreadFactory::PRIORITY PosixThreadFactory::priority() const {return _impl->priority();}

void PosixThreadFactory::priority(PosixThreadFactory::PRIORITY value) {_impl->priority(value);}

}}} // facebook::thrift::concurrency
