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

#define BOOST_TEST_MODULE TNonblockingServerTest
#include <boost/test/unit_test.hpp>
#include <memory>

#include "thrift/TConfiguration.h"
#include "thrift/concurrency/Monitor.h"
#include "thrift/concurrency/Thread.h"
#include "thrift/concurrency/ThreadManager.h"
#include "thrift/protocol/TBinaryProtocol.h"
#include "thrift/server/TNonblockingServer.h"
#include "thrift/transport/TNonblockingServerSocket.h"
#include "thrift/transport/TSocket.h"
#include "thrift/transport/TTransportUtils.h"

#include <new>

#include "gen-cpp/ParentService.h"

#include <event.h>

using apache::thrift::concurrency::Guard;
using apache::thrift::concurrency::Monitor;
using apache::thrift::concurrency::Mutex;
using apache::thrift::concurrency::ThreadFactory;
using apache::thrift::concurrency::Runnable;
using apache::thrift::concurrency::Thread;
using apache::thrift::concurrency::ThreadFactory;
using apache::thrift::server::TServerEventHandler;
using std::make_shared;
using std::shared_ptr;

using namespace apache::thrift;

struct Handler : public test::ParentServiceIf {
  void addString(const std::string& s) override { strings_.push_back(s); }
  void getStrings(std::vector<std::string>& _return) override { _return = strings_; }
  std::vector<std::string> strings_;

  // dummy overrides not used in this test
  int32_t incrementGeneration() override { return 0; }
  int32_t getGeneration() override { return 0; }
  void getDataWait(std::string&, const int32_t) override {}
  void onewayWait() override {}
  void exceptionWait(const std::string&) override {}
  void unexpectedExceptionWait(const std::string&) override {}
};

class Fixture {
private:
  struct ListenEventHandler : public TServerEventHandler {
    public:
      ListenEventHandler(Mutex* mutex) : listenMonitor_(mutex), ready_(false) {}

      void preServe() override /* override */ {
        Guard g(listenMonitor_.mutex());
        ready_ = true;
        listenMonitor_.notify();
      }

      Monitor listenMonitor_;
      bool ready_;
  };

  struct Runner : public Runnable {
    int port;
    shared_ptr<event_base> userEventBase;
    shared_ptr<TProcessor> processor;
    shared_ptr<concurrency::ThreadManager> threadManager;
    shared_ptr<server::TNonblockingServer> server;
    shared_ptr<ListenEventHandler> listenHandler;
    shared_ptr<transport::TNonblockingServerSocket> socket;
    Mutex mutex_;

    Runner() {
      port = 0;
      listenHandler.reset(new ListenEventHandler(&mutex_));
    }

    void run() override {
      // When binding to explicit port, allow retrying to workaround bind failures on ports in use
      int retryCount = port ? 10 : 0;
      startServer(retryCount);
    }

    void readyBarrier() {
      // block until server is listening and ready to accept connections
      Guard g(mutex_);
      while (!listenHandler->ready_) {
        listenHandler->listenMonitor_.wait();
      }
    }
  private:
    void startServer(int retry_count) {
      try {
        socket.reset(new transport::TNonblockingServerSocket(port));
        if (threadManager) {
          server.reset(new server::TNonblockingServer(
              processor, make_shared<protocol::TBinaryProtocolFactory>(), socket, threadManager));
        } else {
          server.reset(new server::TNonblockingServer(processor, socket));
        }
        server->setServerEventHandler(listenHandler);
        if (userEventBase) {
          server->registerEvents(userEventBase.get());
        }
        server->serve();
      } catch (const transport::TTransportException&) {
        if (retry_count > 0) {
          ++port;
          startServer(retry_count - 1);
        } else {
          throw;
        }
      }
    }
  };

  struct EventDeleter {
    void operator()(event_base* p) { event_base_free(p); }
  };

protected:
  Fixture() : processor(make_shared<test::ParentServiceProcessor>(make_shared<Handler>())) {}

  ~Fixture() {
    if (server) {
      server->stop();
    }
    if (thread) {
      thread->join();
    }
  }

  void setEventBase(event_base* user_event_base) {
    userEventBase_.reset(user_event_base, EventDeleter());
  }

  int startServer(int port) {
    shared_ptr<Runner> runner(new Runner);
    runner->port = port;
    runner->processor = processor;
    runner->threadManager = threadManager_;
    runner->userEventBase = userEventBase_;

    shared_ptr<ThreadFactory> threadFactory(
        new ThreadFactory(false));
    thread = threadFactory->newThread(runner);
    thread->start();
    runner->readyBarrier();

    server = runner->server;
    return runner->port;
  }

  bool canCommunicate(int serverPort) {
    shared_ptr<transport::TSocket> socket(new transport::TSocket("localhost", serverPort));
    socket->open();
    test::ParentServiceClient client(make_shared<protocol::TBinaryProtocol>(
        make_shared<transport::TFramedTransport>(socket)));
    client.addString("foo");
    std::vector<std::string> strings;
    client.getStrings(strings);
    return strings.size() == 1 && !(strings[0].compare("foo"));
  }

private:
  shared_ptr<event_base> userEventBase_;
  shared_ptr<concurrency::ThreadManager> threadManager_;

protected:
  // Replaces the processor the fixture builds, and switches the server to the
  // thread-pooled path, which dispatches through Task::run() rather than
  // running the processor on the I/O thread.
  void useThreadPool(const shared_ptr<TProcessor>& replacement) {
    processor = replacement;
    threadManager_ = concurrency::ThreadManager::newSimpleThreadManager(2);
    threadManager_->threadFactory(make_shared<ThreadFactory>());
    threadManager_->start();
  }

private:
  shared_ptr<TProcessor> processor;
protected:
  shared_ptr<server::TNonblockingServer> server;
private:
  shared_ptr<apache::thrift::concurrency::Thread> thread;

};

BOOST_AUTO_TEST_SUITE(TNonblockingServerTest)

BOOST_FIXTURE_TEST_CASE(get_specified_port, Fixture) {
  int specified_port = startServer(12345);
  BOOST_REQUIRE_GE(specified_port, 12345);
  BOOST_REQUIRE_EQUAL(server->getListenPort(), specified_port);
  BOOST_CHECK(canCommunicate(specified_port));

  server->stop();
}

BOOST_FIXTURE_TEST_CASE(get_assigned_port, Fixture) {
  int specified_port = startServer(0);
  BOOST_REQUIRE_EQUAL(specified_port, 0);
  int assigned_port = server->getListenPort();
  BOOST_REQUIRE_NE(assigned_port, 0);
  BOOST_CHECK(canCommunicate(assigned_port));

  server->stop();
}

BOOST_FIXTURE_TEST_CASE(provide_event_base, Fixture) {
  event_base* eb = event_base_new();
  setEventBase(eb);
  startServer(0);

  // assert that the server works
  BOOST_CHECK(canCommunicate(server->getListenPort()));
#if LIBEVENT_VERSION_NUMBER > 0x02010400
  // also assert that the event_base is actually used when it's easy
  BOOST_CHECK_GT(event_base_get_num_events(eb, EVENT_BASE_COUNT_ADDED), 0);
#endif
}

// The frame cap TNonblockingServer applies to every accepted connection lives on
// the server itself: TConfiguration appears nowhere in TNonblockingServer.{h,cpp},
// so the default here and the library-wide default are two independent numbers
// that have already drifted 16x apart once. Pin them together, and pin that the
// setter still wins, so a caller that needs larger frames keeps its escape hatch.
BOOST_AUTO_TEST_CASE(default_max_frame_size_matches_configuration) {
  auto socket = make_shared<transport::TNonblockingServerSocket>(0);
  auto processor = make_shared<test::ParentServiceProcessor>(make_shared<Handler>());
  server::TNonblockingServer server(processor, socket);

  BOOST_CHECK_EQUAL(server.getMaxFrameSize(),
                    static_cast<size_t>(TConfiguration::DEFAULT_MAX_FRAME_SIZE));

  server.setMaxFrameSize(4096);
  BOOST_CHECK_EQUAL(server.getMaxFrameSize(), static_cast<size_t>(4096));
}

// Fails the first call the way argument deserialization fails when a declared
// container count does not fit in memory: the exception leaves process(). It
// cannot be raised from the handler instead, because generated dispatch code
// catches std::exception around the handler call and answers with a
// TApplicationException -- the allocation that fails is the one made before
// that point, reading the arguments.
struct FailsFirstCallProcessor : public TProcessor {
  explicit FailsFirstCallProcessor(const shared_ptr<TProcessor>& delegate)
    : delegate_(delegate), failed_(false) {}

  bool process(shared_ptr<protocol::TProtocol> in,
               shared_ptr<protocol::TProtocol> out,
               void* connectionContext) override {
    if (!failed_) {
      failed_ = true;
      throw std::bad_alloc();
    }
    return delegate_->process(in, out, connectionContext);
  }

private:
  shared_ptr<TProcessor> delegate_;
  bool failed_;
};

// A request that cannot be allocated is one request. Task::run() answered
// std::bad_alloc with exit(1), so any peer able to make one allocation fail
// ended the process for every other client connected to it -- and a declared
// container count is enough to try. The inline path in this same file already
// logs and closes the connection for every std::exception; this pins the
// thread-pooled path to the same behaviour.
//
// If the process does end, this test does not fail an assertion: the binary
// exits mid-run and takes the whole suite with it.
//
// It does not assert anything about the doomed connection. A task that throws
// writes nothing, and the connection reads an empty write buffer as "no reply
// was owed" -- so the caller of that one request never hears back. An empty
// buffer is not the same thing as a oneway call: a void method still owes a
// reply, and an exception is one of the replies it can owe. That confusion is
// pre-existing, applies to every exception this catch handles, and differs from
// the inline path, which closes the connection. Recorded separately.
BOOST_FIXTURE_TEST_CASE(bad_alloc_does_not_end_the_process, Fixture) {
  useThreadPool(make_shared<FailsFirstCallProcessor>(
      make_shared<test::ParentServiceProcessor>(make_shared<Handler>())));

  startServer(0);
  int port = server->getListenPort();
  BOOST_REQUIRE_GT(port, 0);

  // Send the doomed call without waiting for a reply. A task that throws leaves
  // outputTransport_ empty, and the connection answers an empty buffer with
  // nothing at all -- so a client that waited here would wait for ever. That is
  // the same for every exception this path catches and is not what this test is
  // about; see the note above.
  {
    auto socket = make_shared<transport::TSocket>("localhost", port);
    socket->open();
    test::ParentServiceClient client(
        make_shared<protocol::TBinaryProtocol>(make_shared<transport::TFramedTransport>(socket)));
    client.send_addString("this one cannot be allocated");
    socket->close();
  }

  // The finding is that the process does not survive the above. A second
  // connection, served normally, is what says it did.
  BOOST_CHECK_MESSAGE(canCommunicate(port),
                      "the server stopped serving after one failed allocation");
}

BOOST_AUTO_TEST_SUITE_END()
