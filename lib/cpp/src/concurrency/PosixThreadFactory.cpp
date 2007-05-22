// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "PosixThreadFactory.h"
#include "Exception.h"

#include <assert.h>
#include <pthread.h>

#include <iostream>

#include <boost/weak_ptr.hpp>

namespace facebook { namespace thrift { namespace concurrency {

using namespace boost;

/**
 * The POSIX thread class.
 *
 * @author marc
 * @version $Id:$
 */
class PthreadThread: public Thread {
 public:

  enum STATE {
    uninitialized,
    starting,
    started,
    stopping,
    stopped
  };

  static const int MB = 1024 * 1024;

  static void* threadMain(void* arg);

 private:
  pthread_t pthread_;
  STATE state_;
  int policy_;
  int priority_;
  int stackSize_;
  weak_ptr<PthreadThread> self_;

 public:

  PthreadThread(int policy, int priority, int stackSize, shared_ptr<Runnable> runnable) :
    pthread_(0),
    state_(uninitialized),
    policy_(policy),
    priority_(priority),
    stackSize_(stackSize) {

    this->Thread::runnable(runnable);
  }

  ~PthreadThread() {}

  void start() {
    if (state_ != uninitialized) {
      return;
    }

    state_ = starting;

    pthread_attr_t thread_attr;
    if(pthread_attr_init(&thread_attr) != 0) {
        throw SystemResourceException("pthread_attr_init failed");
    }

    if(pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_JOINABLE) != 0) {
        throw SystemResourceException("pthread_attr_setdetachstate failed");
    }

    // Set thread stack size
    if(pthread_attr_setstacksize(&thread_attr, MB * stackSize_) != 0) {
        throw SystemResourceException("pthread_attr_setstacksize failed");
    }

    // Set thread policy
    if(pthread_attr_setschedpolicy(&thread_attr, policy_) != 0) {
        throw SystemResourceException("pthread_attr_setschedpolicy failed");
    }

    struct sched_param sched_param;
    sched_param.sched_priority = priority_;

    // Set thread priority
    if(pthread_attr_setschedparam(&thread_attr, &sched_param) != 0) {
        throw SystemResourceException("pthread_attr_setschedparam failed");
    }

    // Create reference
    shared_ptr<PthreadThread>* selfRef = new shared_ptr<PthreadThread>();
    *selfRef = self_.lock();

    if(pthread_create(&pthread_, &thread_attr, threadMain, (void*)selfRef) != 0) {
        throw SystemResourceException("pthread_create failed");
    }
  }

  void join() {
    if (state_ != stopped) {
      void* ignore;
      pthread_join(pthread_, &ignore);
    }
  }

  id_t id() {
    return pthread_;
  }

  shared_ptr<Runnable> runnable() const { return Thread::runnable(); }

  void runnable(shared_ptr<Runnable> value) { Thread::runnable(value); }

  void weakRef(shared_ptr<PthreadThread> self) {
    assert(self.get() == this);
    self_ = weak_ptr<PthreadThread>(self);
  }
};

void* PthreadThread::threadMain(void* arg) {
  shared_ptr<PthreadThread> thread = *(shared_ptr<PthreadThread>*)arg;
  delete reinterpret_cast<shared_ptr<PthreadThread>*>(arg);

  if (thread == NULL) {
    return (void*)0;
  }

  if (thread->state_ != starting) {
    return (void*)0;
  }

  thread->state_ = starting;
  thread->runnable()->run();
  if (thread->state_ != stopping && thread->state_ != stopped) {
    thread->state_ = stopping;
  }

  return (void*)0;
}

/**
 * POSIX Thread factory implementation
 */
class PosixThreadFactory::Impl {

 private:
  POLICY policy_;
  PRIORITY priority_;
  int stackSize_;
  bool detached_;

  /**
   * Converts generic posix thread schedule policy enums into pthread
   * API values.
   */
  static int toPthreadPolicy(POLICY policy) {
    switch (policy) {
    case OTHER:
      return SCHED_OTHER;
    case FIFO:
      return SCHED_FIFO;
    case ROUND_ROBIN:
      return SCHED_RR;
    }
    return SCHED_OTHER;
  }

  /**
   * Converts relative thread priorities to absolute value based on posix
   * thread scheduler policy
   *
   *  The idea is simply to divide up the priority range for the given policy
   * into the correpsonding relative priority level (lowest..highest) and
   * then pro-rate accordingly.
   */
  static int toPthreadPriority(POLICY policy, PRIORITY priority) {
    int pthread_policy = toPthreadPolicy(policy);
    int min_priority = sched_get_priority_min(pthread_policy);
    int max_priority = sched_get_priority_max(pthread_policy);
    int quanta = (HIGHEST - LOWEST) + 1;
    float stepsperquanta = (max_priority - min_priority) / quanta;

    if (priority <= HIGHEST) {
      return (int)(min_priority + stepsperquanta * priority);
    } else {
      // should never get here for priority increments.
      assert(false);
      return (int)(min_priority + stepsperquanta * NORMAL);
    }
  }

 public:

  Impl(POLICY policy, PRIORITY priority, int stackSize, bool detached) :
    policy_(policy),
    priority_(priority),
    stackSize_(stackSize),
    detached_(detached) {}

  /**
   * Creates a new POSIX thread to run the runnable object
   *
   * @param runnable A runnable object
   */
  shared_ptr<Thread> newThread(shared_ptr<Runnable> runnable) const {
    shared_ptr<PthreadThread> result = shared_ptr<PthreadThread>(new PthreadThread(toPthreadPolicy(policy_), toPthreadPriority(policy_, priority_), stackSize_, runnable));
    result->weakRef(result);
    runnable->thread(result);
    return result;
  }

  int stackSize() const { return stackSize_; }

  void stackSize(int value) { stackSize_ = value; }

  PRIORITY priority() const { return priority_; }

  Thread::id_t currentThreadId() const {return pthread_self();}

  /**
   * Sets priority.
   *
   *  XXX
   *  Need to handle incremental priorities properly.
   */
  void priority(PRIORITY value) { priority_ = value; }
};

PosixThreadFactory::PosixThreadFactory(POLICY policy, PRIORITY priority, int stackSize, bool detached) :
  impl_(new PosixThreadFactory::Impl(policy, priority, stackSize, detached)) {}

shared_ptr<Thread> PosixThreadFactory::newThread(shared_ptr<Runnable> runnable) const { return impl_->newThread(runnable); }

int PosixThreadFactory::stackSize() const { return impl_->stackSize(); }

void PosixThreadFactory::stackSize(int value) { impl_->stackSize(value); }

PosixThreadFactory::PRIORITY PosixThreadFactory::priority() const { return impl_->priority(); }

void PosixThreadFactory::priority(PosixThreadFactory::PRIORITY value) { impl_->priority(value); }

Thread::id_t PosixThreadFactory::currentThreadId() const {return impl_->currentThreadId();}

}}} // facebook::thrift::concurrency
