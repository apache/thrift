/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <thrift/concurrency/TimerManager.h>
#include <thrift/concurrency/Exception.h>

#include <assert.h>
#include <iostream>
#include <memory>
#include <set>

namespace apache {
namespace thrift {
namespace concurrency {

using std::shared_ptr;
using std::weak_ptr;

/**
 * TimerManager class
 *
 * @version $Id:$
 */
class TimerManager::Task : public Runnable {

public:
  enum STATE { WAITING, EXECUTING, CANCELLED, COMPLETE };

  Task(shared_ptr<Runnable> runnable) : runnable_(runnable), state_(WAITING) {}

  ~Task() override = default;

  void run() override {
    if (state_ == EXECUTING) {
      runnable_->run();
      state_ = COMPLETE;
    }
  }

  bool operator==(const shared_ptr<Runnable> & runnable) const { return runnable_ == runnable; }

  task_iterator it_;

private:
  shared_ptr<Runnable> runnable_;
  friend class TimerManager::Dispatcher;
  STATE state_;
};

class TimerManager::Dispatcher : public Runnable {

public:
  Dispatcher(TimerManager* manager) : manager_(manager) {}

  ~Dispatcher() override = default;

  /**
   * Dispatcher entry point
   *
   * As long as dispatcher thread is running, pull tasks off the task taskMap_
   * and execute.
   */
  void run() override {
    {
      Synchronized s(manager_->monitor_);
      if (manager_->state_ == TimerManager::STARTING) {
        manager_->state_ = TimerManager::STARTED;
        manager_->monitor_.notifyAll();
      }
    }

    do {
      std::set<shared_ptr<TimerManager::Task> > expiredTasks;
      {
        Synchronized s(manager_->monitor_);
        task_iterator expiredTaskEnd;
        auto now = std::chrono::steady_clock::now();
        while (manager_->state_ == TimerManager::STARTED
               && (expiredTaskEnd = manager_->taskMap_.upper_bound(now))
                  == manager_->taskMap_.begin()) {
          std::chrono::milliseconds timeout(0);
          if (!manager_->taskMap_.empty()) {
            timeout = std::chrono::duration_cast<std::chrono::milliseconds>(manager_->taskMap_.begin()->first - now);
            //because the unit of steady_clock is smaller than millisecond,timeout may be 0.
            if (timeout.count() == 0) {
              timeout = std::chrono::milliseconds(1);
            }
            manager_->monitor_.waitForTimeRelative(timeout);
          } else {
            manager_->monitor_.waitForTimeRelative(0);
          }
          now = std::chrono::steady_clock::now();
        }

        if (manager_->state_ == TimerManager::STARTED) {
          for (auto ix = manager_->taskMap_.begin(); ix != expiredTaskEnd; ix++) {
            shared_ptr<TimerManager::Task> task = ix->second;
            expiredTasks.insert(task);
            task->it_ = manager_->taskMap_.end();
            if (task->state_ == TimerManager::Task::WAITING) {
              task->state_ = TimerManager::Task::EXECUTING;
            }
            manager_->taskCount_--;
          }
          manager_->taskMap_.erase(manager_->taskMap_.begin(), expiredTaskEnd);
        }
      }

      for (const auto & expiredTask : expiredTasks) {
        expiredTask->run();
      }

    } while (manager_->state_ == TimerManager::STARTED);

    {
      Synchronized s(manager_->monitor_);
      if (manager_->state_ == TimerManager::STOPPING) {
        manager_->state_ = TimerManager::STOPPED;
        manager_->monitor_.notifyAll();
      }
    }
    return;
  }

private:
  TimerManager* manager_;
  friend class TimerManager;
};

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4355) // 'this' used in base member initializer list
#endif

TimerManager::TimerManager()
  : taskCount_(0),
    state_(TimerManager::UNINITIALIZED),
    dispatcher_(std::make_shared<Dispatcher>(this)) {
}

#if defined(_MSC_VER)
#pragma warning(pop)
#endif

TimerManager::~TimerManager() {

  // If we haven't been explicitly stopped, do so now.  We don't need to grab
  // the monitor here, since stop already takes care of reentrancy.

  if (state_ != STOPPED) {
    try {
      stop();
    } catch (...) {
      // We're really hosed.
    }
  }
}

void TimerManager::start() {
  bool doStart = false;
  {
    Synchronized s(monitor_);
    if (!threadFactory_) {
      throw InvalidArgumentException();
    }
    if (state_ == TimerManager::UNINITIALIZED) {
      state_ = TimerManager::STARTING;
      doStart = true;
    }
  }

  if (doStart) {
    dispatcherThread_ = threadFactory_->newThread(dispatcher_);
    dispatcherThread_->start();
  }

  {
    Synchronized s(monitor_);
    while (state_ == TimerManager::STARTING) {
      monitor_.wait();
    }
    assert(state_ != TimerManager::STARTING);
  }
}

void TimerManager::stop() {
  bool doStop = false;
  {
    Synchronized s(monitor_);
    if (state_ == TimerManager::UNINITIALIZED) {
      state_ = TimerManager::STOPPED;
    } else if (state_ != STOPPING && state_ != STOPPED) {
      doStop = true;
      state_ = STOPPING;
      monitor_.notifyAll();
    }
    while (state_ != STOPPED) {
      monitor_.wait();
    }
  }

  if (doStop) {
    // Clean up any outstanding tasks
    taskMap_.clear();

    // Remove dispatcher's reference to us.
    dispatcher_->manager_ = nullptr;
  }
}

shared_ptr<const ThreadFactory> TimerManager::threadFactory() const {
  Synchronized s(monitor_);
  return threadFactory_;
}

void TimerManager::threadFactory(shared_ptr<const ThreadFactory> value) {
  Synchronized s(monitor_);
  threadFactory_ = value;
}

size_t TimerManager::taskCount() const {
  return taskCount_;
}

TimerManager::Timer TimerManager::add(shared_ptr<Runnable> task, const std::chrono::milliseconds &timeout) {
  return add(task, std::chrono::steady_clock::now() + timeout);
}

TimerManager::Timer TimerManager::add(shared_ptr<Runnable> task,
    const std::chrono::time_point<std::chrono::steady_clock>& abstime) {
  auto now = std::chrono::steady_clock::now();

  if (abstime < now) {
    throw InvalidArgumentException();
  }
  Synchronized s(monitor_);
  if (state_ != TimerManager::STARTED) {
    throw IllegalStateException();
  }

  // If the task map is empty, we will kick the dispatcher for sure. Otherwise, we kick him
  // if the expiration time is shorter than the current value. Need to test before we insert,
  // because the new task might insert at the front.
  bool notifyRequired = (taskCount_ == 0) ? true : abstime < taskMap_.begin()->first;

  shared_ptr<Task> timer(new Task(task));
  taskCount_++;
  timer->it_ = taskMap_.emplace(abstime, timer);

  // If the task map was empty, or if we have an expiration that is earlier
  // than any previously seen, kick the dispatcher so it can update its
  // timeout
  if (notifyRequired) {
    monitor_.notify();
  }

  return timer;
}

void TimerManager::remove(shared_ptr<Runnable> task) {
  Synchronized s(monitor_);
  if (state_ != TimerManager::STARTED) {
    throw IllegalStateException();
  }
  bool found = false;
  for (auto ix = taskMap_.begin(); ix != taskMap_.end();) {
    if (*ix->second == task) {
      found = true;
      taskCount_--;
      taskMap_.erase(ix++);
    } else {
      ++ix;
    }
  }
  if (!found) {
    throw NoSuchTaskException();
  }
}

void TimerManager::remove(Timer handle) {
  Synchronized s(monitor_);
  if (state_ != TimerManager::STARTED) {
    throw IllegalStateException();
  }

  shared_ptr<Task> task = handle.lock();
  if (!task) {
    throw NoSuchTaskException();
  }

  if (task->it_ == taskMap_.end()) {
    // Task is being executed
    throw UncancellableTaskException();
  }

  taskMap_.erase(task->it_);
  taskCount_--;
}

TimerManager::STATE TimerManager::state() const {
  return state_;
}
}
}
} // apache::thrift::concurrency
