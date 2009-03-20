// Copyright (c) 2008- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_CONCURRENCY_FUNCTION_RUNNER_H
#define _THRIFT_CONCURRENCY_FUNCTION_RUNNER_H 1

#include <tr1/functional>
#include "thrift/lib/cpp/concurrency/Thread.h"

namespace apache { namespace thrift { namespace concurrency {

/**
 * Convenient implementation of Runnable that will execute arbitrary callbacks.
 * Interfaces are provided to accept both a generic 'void(void)' callback, and
 * a 'void* (void*)' pthread_create-style callback.
 *
 * Example use:
 *  void* my_thread_main(void* arg);
 *  shared_ptr<ThreadFactory> factory = ...;
 *  shared_ptr<Thread> thread =
 *    factory->newThread(shared_ptr<FunctionRunner>(
 *      new FunctionRunner(my_thread_main, some_argument)));
 *  thread->start();
 *
 *
 */

class FunctionRunner : public Runnable {
 public:
  // This is the type of callback 'pthread_create()' expects.
  typedef void* (*PthreadFuncPtr)(void *arg);
  // This a fully-generic void(void) callback for custom bindings.
  typedef std::tr1::function<void()> VoidFunc;

  /**
   * Given a 'pthread_create' style callback, this FunctionRunner will
   * execute the given callback.  Note that the 'void*' return value is ignored.
   */
  FunctionRunner(PthreadFuncPtr func, void* arg)
   : func_(std::tr1::bind(func, arg))
  { }

  /**
   * Given a generic callback, this FunctionRunner will execute it.
   */
  FunctionRunner(const VoidFunc& cob)
   : func_(cob)
  { }


  void run() {
    func_();
  }

 private:
  VoidFunc func_;
};

}}} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_FUNCTION_RUNNER_H
