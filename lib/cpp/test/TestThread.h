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

#ifndef _THRIFT_TEST_TESTTHREAD_H_
#define _THRIFT_TEST_TESTTHREAD_H_ 1

#include <chrono>
#include <functional>
#include <future>
#include <memory>
#include <thread>

/**
 * A std::thread with the two things these tests relied on boost::thread for:
 * it detaches rather than calling std::terminate when it is destroyed without
 * a join, so a test that fails early reports the failure, and it can wait for
 * the thread for a bounded time.
 */
class TestThread {
public:
  TestThread() = default;

  explicit TestThread(std::function<void()> fn) {
    std::shared_ptr<std::promise<void> > done = std::make_shared<std::promise<void> >();
    finished_ = done->get_future();
    thread_ = std::thread([fn, done]() {
      fn();
      done->set_value();
    });
  }

  TestThread(TestThread&&) = default;

  TestThread& operator=(TestThread&& other) {
    release();
    thread_ = std::move(other.thread_);
    finished_ = std::move(other.finished_);
    return *this;
  }

  ~TestThread() { release(); }

  void join() { thread_.join(); }

  /**
   * Joins the thread if it finishes within the timeout.
   * \returns  true if the thread finished and was joined
   */
  bool try_join_for(std::chrono::milliseconds timeout) {
    if (finished_.wait_for(timeout) != std::future_status::ready) {
      return false;
    }
    thread_.join();
    return true;
  }

private:
  void release() {
    if (thread_.joinable()) {
      thread_.detach();
    }
  }

  std::thread thread_;
  std::future<void> finished_;
};

#endif
