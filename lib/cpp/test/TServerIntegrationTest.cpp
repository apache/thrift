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

#define BOOST_TEST_MODULE TServerIntegrationTest
#include <atomic>
#include <boost/test/unit_test.hpp>
#include <chrono>
#include <thread>
#include <thrift/server/TSimpleServer.h>
#include <thrift/server/TThreadPoolServer.h>
#include <thrift/server/TThreadedServer.h>
#include <memory>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransport.h>
#include "gen-cpp/ParentService.h"
#include "TestThread.h"
#include <string>
#include <vector>

using apache::thrift::concurrency::Guard;
using apache::thrift::concurrency::Monitor;
using apache::thrift::concurrency::Mutex;
using apache::thrift::concurrency::Synchronized;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TBinaryProtocolFactory;
using apache::thrift::protocol::TProtocol;
using apache::thrift::protocol::TProtocolFactory;
using apache::thrift::transport::TServerSocket;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TTransportFactory;
using apache::thrift::server::TServer;
using apache::thrift::server::TServerEventHandler;
using apache::thrift::server::TSimpleServer;
using apache::thrift::server::TThreadPoolServer;
using apache::thrift::server::TThreadedServer;
using std::dynamic_pointer_cast;
using std::make_shared;
using std::shared_ptr;
using apache::thrift::test::ParentServiceClient;
using apache::thrift::test::ParentServiceIf;
using apache::thrift::test::ParentServiceIfFactory;
using apache::thrift::test::ParentServiceIfSingletonFactory;
using apache::thrift::test::ParentServiceProcessor;
using apache::thrift::test::ParentServiceProcessorFactory;
using apache::thrift::TProcessor;
using apache::thrift::TProcessorFactory;
using std::chrono::milliseconds;

/**
 * preServe runs after listen() is successful, when we can connect
 */
class TServerReadyEventHandler : public TServerEventHandler, public Monitor {
public:
  TServerReadyEventHandler() : isListening_(false), accepted_(0) {}
  ~TServerReadyEventHandler() override = default;
  void preServe() override {
    Synchronized sync(*this);
    isListening_ = true;
    notify();
  }
  void* createContext(shared_ptr<TProtocol> input,
                              shared_ptr<TProtocol> output) override {
    Synchronized sync(*this);
    ++accepted_;
    notify();

    (void)input;
    (void)output;
    return nullptr;
  }
  bool isListening() const { return isListening_; }
  uint64_t acceptedCount() const { return accepted_; }

private:
  bool isListening_;
  uint64_t accepted_;
};

/**
 * Reusing another generated test, just something to serve up
 */
class ParentHandler : public ParentServiceIf {
public:
  ParentHandler() : generation_(0) {}

  int32_t incrementGeneration() override {
    Guard g(mutex_);
    return ++generation_;
  }

  int32_t getGeneration() override {
    Guard g(mutex_);
    return generation_;
  }

  void addString(const std::string& s) override {
    Guard g(mutex_);
    strings_.push_back(s);
  }

  void getStrings(std::vector<std::string>& _return) override {
    Guard g(mutex_);
    _return = strings_;
  }

  void getDataWait(std::string& _return, const int32_t length) override {
    THRIFT_UNUSED_VARIABLE(_return);
    THRIFT_UNUSED_VARIABLE(length);
  }

  void onewayWait() override {}

  void exceptionWait(const std::string& message) override { THRIFT_UNUSED_VARIABLE(message); }

  void unexpectedExceptionWait(const std::string& message) override { THRIFT_UNUSED_VARIABLE(message); }

protected:
  Mutex mutex_;
  int32_t generation_;
  std::vector<std::string> strings_;
};

void autoSocketCloser(TSocket* pSock) {
  pSock->close();
  delete pSock;
}

template <class TServerType>
class TServerIntegrationTestFixture {
public:
  TServerIntegrationTestFixture(const shared_ptr<TProcessorFactory>& _processorFactory)
    : pServer(new TServerType(_processorFactory,
                              shared_ptr<TServerTransport>(
                                  new TServerSocket("localhost", 0)),
                              shared_ptr<TTransportFactory>(new TTransportFactory),
                              shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory))),
      pEventHandler(shared_ptr<TServerReadyEventHandler>(new TServerReadyEventHandler)),
    bStressDone(false),
    bStressConnectionCount(0),
    bStressRequestCount(0) {
    pServer->setServerEventHandler(pEventHandler);
  }

  TServerIntegrationTestFixture(const shared_ptr<TProcessor>& _processor)
    : pServer(
          new TServerType(_processor,
                          shared_ptr<TServerTransport>(new TServerSocket("localhost", 0)),
                          shared_ptr<TTransportFactory>(new TTransportFactory),
                          shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory))),
      pEventHandler(shared_ptr<TServerReadyEventHandler>(new TServerReadyEventHandler)),
      bStressDone(false),
    bStressConnectionCount(0),
    bStressRequestCount(0) {
    pServer->setServerEventHandler(pEventHandler);
  }

  void startServer() {
    pServerThread.reset(new TestThread(std::bind(&TServerType::serve, pServer.get())));

    // block until listen() completes so clients will be able to connect
    Synchronized sync(*(pEventHandler.get()));
    while (!pEventHandler->isListening()) {
      pEventHandler->wait();
    }

    BOOST_TEST_MESSAGE("  server is listening");
  }

  void blockUntilAccepted(uint64_t numAccepted) {
    Synchronized sync(*(pEventHandler.get()));
    while (pEventHandler->acceptedCount() < numAccepted) {
      pEventHandler->wait();
    }

    BOOST_TEST_MESSAGE("  server has accepted " << numAccepted);
  }

  void stopServer() {
    if (pServerThread) {
      pServer->stop();
      BOOST_TEST_MESSAGE("  server stop completed");

      pServerThread->join();
      BOOST_TEST_MESSAGE("  server thread joined");
      pServerThread.reset();
    }
  }

  ~TServerIntegrationTestFixture() { stopServer(); }

  /**
   * Performs a baseline test where some clients are opened and issue a single operation
   * and then disconnect at different intervals.
   * \param[in]  numToMake  the number of concurrent clients
   * \param[in]  expectedHWM  the high water mark we expect of concurrency
   * \param[in]  purpose  a description of the test for logging purposes
   */
  void baseline(int64_t numToMake, int64_t expectedHWM, const std::string& purpose) {
    BOOST_TEST_MESSAGE("Testing " << typeid(TServerType).name() << ": " << purpose << " with " << numToMake
                                   << " clients, expect " << expectedHWM << " HWM");

    startServer();

    std::vector<shared_ptr<TSocket> > holdSockets;
    std::vector<shared_ptr<TestThread> > holdThreads;

    for (int64_t i = 0; i < numToMake; ++i) {
      shared_ptr<TSocket> pClientSock(new TSocket("localhost", getServerPort()),
                                             autoSocketCloser);
      holdSockets.push_back(pClientSock);
      shared_ptr<TProtocol> pClientProtocol(new TBinaryProtocol(pClientSock));
      ParentServiceClient client(pClientProtocol);
      pClientSock->open();
      client.incrementGeneration();
      holdThreads.push_back(shared_ptr<TestThread>(
          new TestThread(std::bind(&TServerIntegrationTestFixture::delayClose,
                                        this,
                                        pClientSock,
                                        milliseconds(10 * numToMake)))));
    }

    BOOST_CHECK_EQUAL(expectedHWM, pServer->getConcurrentClientCountHWM());

    for (const shared_ptr<TestThread>& pThread : holdThreads) { pThread->join(); }
    holdThreads.clear();
    holdSockets.clear();

    stopServer();
  }

  /**
   * Helper method used to close a connection after a delay.
   * \param[in]  toClose  the connection to close
   * \param[in]  after  the delay to impose
   */
  void delayClose(shared_ptr<TTransport> toClose, milliseconds after) {
    std::this_thread::sleep_for(after);
    toClose->close();
  }

  /**
   * \returns  the server port number
   */
  int getServerPort() {
    auto* pSock = dynamic_cast<TServerSocket*>(pServer->getServerTransport().get());
    if (!pSock) { throw std::logic_error("how come?"); }
    return pSock->getPort();
  }

  /**
   * Performs a stress test by spawning threads that connect, do a number of operations
   * and disconnect, then a random delay, then do it over again.  This is done for a fixed
   * period of time to test for concurrency correctness.
   * \param[in]  numToMake  the number of concurrent clients
   */
  void stress(int64_t numToMake, const milliseconds& duration) {
    BOOST_TEST_MESSAGE("Stress testing " << typeid(TServerType).name() << " with " << numToMake
        << " clients for " << std::chrono::duration_cast<std::chrono::seconds>(duration).count() << " seconds");

    startServer();

    std::vector<shared_ptr<TestThread> > holdThreads;
    for (int64_t i = 0; i < numToMake; ++i) {
      holdThreads.push_back(shared_ptr<TestThread>(
        new TestThread(std::bind(&TServerIntegrationTestFixture::stressor, this))));
    }

    std::this_thread::sleep_for(duration);
    bStressDone = true;

    BOOST_TEST_MESSAGE("  serviced " << bStressConnectionCount << " connections (HWM "
        << pServer->getConcurrentClientCountHWM() << ") totaling " << bStressRequestCount << " requests");

    for (const shared_ptr<TestThread>& pThread : holdThreads) { pThread->join(); }
    holdThreads.clear();

    BOOST_CHECK(bStressRequestCount > 0);

    stopServer();
  }

  /**
   * Helper method to stress the system
   */
  void stressor() {
  while (!bStressDone) {
      shared_ptr<TSocket> pSocket(new TSocket("localhost", getServerPort()), autoSocketCloser);
      shared_ptr<TProtocol> pProtocol(new TBinaryProtocol(pSocket));
      ParentServiceClient client(pProtocol);
      pSocket->open();
      bStressConnectionCount.fetch_add(1, std::memory_order_relaxed);
      for (int i = 0; i < rand() % 1000; ++i) {
      client.incrementGeneration();
        bStressRequestCount.fetch_add(1, std::memory_order_relaxed);
      }
    }
  }

  shared_ptr<TServerType> pServer;
  shared_ptr<TServerReadyEventHandler> pEventHandler;
  shared_ptr<TestThread> pServerThread;
  std::atomic<bool> bStressDone;
  std::atomic<int64_t> bStressConnectionCount;
  std::atomic<int64_t> bStressRequestCount;
};

template <class TServerType>
class TServerIntegrationProcessorFactoryTestFixture
    : public TServerIntegrationTestFixture<TServerType> {
public:
  TServerIntegrationProcessorFactoryTestFixture()
    : TServerIntegrationTestFixture<TServerType>(make_shared<ParentServiceProcessorFactory>(
          make_shared<ParentServiceIfSingletonFactory>(
              make_shared<ParentHandler>()))) {}
};

template <class TServerType>
class TServerIntegrationProcessorTestFixture : public TServerIntegrationTestFixture<TServerType> {
public:
  TServerIntegrationProcessorTestFixture()
    : TServerIntegrationTestFixture<TServerType>(
          make_shared<ParentServiceProcessor>(make_shared<ParentHandler>())) {}
};

BOOST_AUTO_TEST_SUITE(constructors)

BOOST_FIXTURE_TEST_CASE(test_simple_factory,
                        TServerIntegrationProcessorFactoryTestFixture<TSimpleServer>) {
  baseline(3, 1, "factory");
}

BOOST_FIXTURE_TEST_CASE(test_simple, TServerIntegrationProcessorTestFixture<TSimpleServer>) {
  baseline(3, 1, "processor");
}

BOOST_FIXTURE_TEST_CASE(test_threaded_factory,
                        TServerIntegrationProcessorFactoryTestFixture<TThreadedServer>) {
  baseline(10, 10, "factory");
}

BOOST_FIXTURE_TEST_CASE(test_threaded, TServerIntegrationProcessorTestFixture<TThreadedServer>) {
  baseline(10, 10, "processor");
}

BOOST_FIXTURE_TEST_CASE(test_threaded_bound,
                        TServerIntegrationProcessorTestFixture<TThreadedServer>) {
  pServer->setConcurrentClientLimit(4);
  baseline(10, 4, "limit by server framework");
}

BOOST_FIXTURE_TEST_CASE(test_threaded_stress,
                        TServerIntegrationProcessorFactoryTestFixture<TThreadedServer>) {
  stress(10, std::chrono::seconds(3));
}

BOOST_FIXTURE_TEST_CASE(test_threadpool_factory,
                        TServerIntegrationProcessorFactoryTestFixture<TThreadPoolServer>) {
  pServer->getThreadManager()->threadFactory(
      shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::ThreadFactory));
  pServer->getThreadManager()->start();

  // thread factory has 4 threads as a default
  // thread factory however is a bad way to limit concurrent clients
  // as accept() will be called to grab a 5th client socket, in this case
  // and then the thread factory will block adding the thread to manage
  // that client.
  baseline(10, 5, "limit by thread manager");
}

BOOST_FIXTURE_TEST_CASE(test_threadpool,
                        TServerIntegrationProcessorTestFixture<TThreadPoolServer>) {
  pServer->getThreadManager()->threadFactory(
      shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::ThreadFactory));
  pServer->getThreadManager()->start();

  // thread factory has 4 threads as a default
  // thread factory however is a bad way to limit concurrent clients
  // as accept() will be called to grab a 5th client socket, in this case
  // and then the thread factory will block adding the thread to manage
  // that client.
  baseline(10, 5, "limit by thread manager");
}

BOOST_FIXTURE_TEST_CASE(test_threadpool_bound,
                        TServerIntegrationProcessorTestFixture<TThreadPoolServer>) {
  pServer->getThreadManager()->threadFactory(
      shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::ThreadFactory));
  pServer->getThreadManager()->start();
  pServer->setConcurrentClientLimit(4);

  baseline(10, 4, "server framework connection limit");
}

BOOST_FIXTURE_TEST_CASE(test_threadpool_stress,
                        TServerIntegrationProcessorTestFixture<TThreadPoolServer>) {
  pServer->getThreadManager()->threadFactory(
      shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::ThreadFactory));
  pServer->getThreadManager()->start();

  stress(10, std::chrono::seconds(3));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_FIXTURE_TEST_SUITE(TServerIntegrationTest,
                         TServerIntegrationProcessorTestFixture<TThreadedServer>)

BOOST_AUTO_TEST_CASE(test_stop_with_interruptable_clients_connected) {
  // This tests THRIFT-2441 new behavior: stopping the server disconnects clients
  BOOST_TEST_MESSAGE("Testing stop with interruptable clients");

  startServer();

  shared_ptr<TSocket> pClientSock1(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock1->open();

  shared_ptr<TSocket> pClientSock2(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock2->open();

  // Ensure they have been accepted
  blockUntilAccepted(2);

  // The test fixture destructor will force the sockets to disconnect
  // Prior to THRIFT-2441, pServer->stop() would hang until clients disconnected
  stopServer();

  // extra proof the server end disconnected the clients
  uint8_t buf[1];
  BOOST_CHECK_EQUAL(0, pClientSock1->read(&buf[0], 1)); // 0 = disconnected
  BOOST_CHECK_EQUAL(0, pClientSock2->read(&buf[0], 1)); // 0 = disconnected
}

BOOST_AUTO_TEST_CASE(test_stop_with_uninterruptable_clients_connected) {
  // This tests pre-THRIFT-2441 behavior: stopping the server blocks until clients
  // disconnect.
    BOOST_TEST_MESSAGE("Testing stop with uninterruptable clients");

  dynamic_pointer_cast<TServerSocket>(pServer->getServerTransport())
      ->setInterruptableChildren(false); // returns to pre-THRIFT-2441 behavior

  startServer();

  shared_ptr<TSocket> pClientSock1(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock1->open();

  shared_ptr<TSocket> pClientSock2(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock2->open();

  // Ensure they have been accepted
  blockUntilAccepted(2);

  TestThread t1(std::bind(&TServerIntegrationTestFixture::delayClose,
                               this,
                               pClientSock1,
                               milliseconds(250)));
  TestThread t2(std::bind(&TServerIntegrationTestFixture::delayClose,
                               this,
                               pClientSock2,
                               milliseconds(250)));

  // Once the clients disconnect the server will stop
  stopServer();
  BOOST_CHECK(pServer->getConcurrentClientCountHWM() > 0);
  t1.join();
  t2.join();
}

BOOST_AUTO_TEST_CASE(test_concurrent_client_limit) {
  startServer();
  BOOST_TEST_MESSAGE("Testing the concurrent client limit");

  BOOST_CHECK_EQUAL(INT64_MAX, pServer->getConcurrentClientLimit());
  pServer->setConcurrentClientLimit(2);
  BOOST_CHECK_EQUAL(0, pServer->getConcurrentClientCount());
  BOOST_CHECK_EQUAL(2, pServer->getConcurrentClientLimit());

  shared_ptr<TSocket> pClientSock1(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock1->open();
  blockUntilAccepted(1);
  BOOST_CHECK_EQUAL(1, pServer->getConcurrentClientCount());

  shared_ptr<TSocket> pClientSock2(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock2->open();
  blockUntilAccepted(2);
  BOOST_CHECK_EQUAL(2, pServer->getConcurrentClientCount());

  // a third client cannot connect until one of the other two closes
  TestThread t2(std::bind(&TServerIntegrationTestFixture::delayClose,
                               this,
                               pClientSock2,
                               milliseconds(250)));
  shared_ptr<TSocket> pClientSock3(new TSocket("localhost", getServerPort()),
                                          autoSocketCloser);
  pClientSock2->open();
  blockUntilAccepted(2);
  BOOST_CHECK_EQUAL(2, pServer->getConcurrentClientCount());
  BOOST_CHECK_EQUAL(2, pServer->getConcurrentClientCountHWM());

  stopServer();
  BOOST_CHECK(pServer->getConcurrentClientCountHWM() > 0);
  t2.join();
}

BOOST_AUTO_TEST_SUITE_END()

// A handler whose incrementGeneration() parks the worker until the test releases
// it, so the single-worker pool can be driven into saturation on demand.
class BlockingParentHandler : public ParentServiceIf {
public:
  BlockingParentHandler() : entered_(0), released_(false) {}

  int32_t incrementGeneration() override {
    {
      Synchronized s(enteredMon_);
      ++entered_;
      enteredMon_.notifyAll();
    }
    {
      Synchronized s(releaseMon_);
      while (!released_) {
        releaseMon_.waitForTimeRelative(200);
      }
    }
    return 1;
  }

  int32_t getGeneration() override { return 0; }
  void addString(const std::string&) override {}
  void getStrings(std::vector<std::string>& _return) override { _return.clear(); }
  void getDataWait(std::string& _return, const int32_t) override {
    THRIFT_UNUSED_VARIABLE(_return);
  }
  void onewayWait() override {}
  void exceptionWait(const std::string&) override {}
  void unexpectedExceptionWait(const std::string&) override {}

  // Blocks until at least n calls have entered the handler, or maxWaits lapse.
  bool awaitEntered(int n, int maxWaits) {
    Synchronized s(enteredMon_);
    int waits = 0;
    while (entered_ < n) {
      if (waits++ >= maxWaits) {
        return false;
      }
      enteredMon_.waitForTimeRelative(100);
    }
    return true;
  }

  void release() {
    Synchronized s(releaseMon_);
    released_ = true;
    releaseMon_.notifyAll();
  }

private:
  Monitor enteredMon_;
  Monitor releaseMon_;
  int entered_;
  bool released_;
};

// When a TThreadPoolServer's ThreadManager::add() rejects a dispatch -- a full
// pending queue with a non-blocking timeout gives TooManyPendingTasksException,
// a positive timeout that lapses gives TimedOutException, both derived from
// TException rather than TTransportException -- the exception used to escape
// serve() and stop the whole server. serve() now drops just that one client and
// keeps accepting, like the Java TThreadPoolServer, which closes the latest
// connection on a RejectedExecutionException and continues.
//
// Run serve() under a wrapper that records an escaping exception rather than
// letting it reach std::terminate, so the two states are: serve() ended (before
// the fix) versus serve() still accepting (after it).
static void run_dispatch_rejection_case(int64_t dispatchTimeout) {
  auto handler = make_shared<BlockingParentHandler>();
  auto processor = make_shared<ParentServiceProcessor>(handler);

  auto tm = apache::thrift::concurrency::ThreadManager::newSimpleThreadManager(1, 1);
  tm->threadFactory(make_shared<apache::thrift::concurrency::ThreadFactory>());
  tm->start();

  auto serverSocket = make_shared<TServerSocket>("localhost", 0);
  auto server = make_shared<TThreadPoolServer>(processor,
                                               serverSocket,
                                               make_shared<TTransportFactory>(),
                                               make_shared<TBinaryProtocolFactory>(),
                                               tm);
  server->setTimeout(dispatchTimeout);

  auto ready = make_shared<TServerReadyEventHandler>();
  server->setServerEventHandler(ready);

  std::atomic<bool> serveThrew(false);
  TestThread serveThread([&server, &serveThrew]() {
    try {
      server->serve();
    } catch (...) {
      serveThrew = true;
    }
  });

  {
    Synchronized sync(*ready);
    int waits = 0;
    while (!ready->isListening() && waits++ < 200) {
      ready->waitForTimeRelative(50);
    }
  }
  BOOST_REQUIRE(ready->isListening());
  int port = serverSocket->getPort();
  BOOST_REQUIRE_GT(port, 0);

  auto connect = [port]() {
    auto sock = make_shared<TSocket>("localhost", port);
    sock->setConnTimeout(2000);
    sock->setSendTimeout(2000);
    sock->setRecvTimeout(2000);
    sock->open();
    return sock;
  };

  // Client A takes the one worker; its call parks in the handler.
  auto sockA = connect();
  {
    ParentServiceClient clientA(make_shared<TBinaryProtocol>(sockA));
    clientA.send_incrementGeneration();
  }
  BOOST_REQUIRE_MESSAGE(handler->awaitEntered(1, 50),
                        "the single worker never picked up the first client");

  // Fire more clients than the busy worker plus the one pending slot can hold.
  // serve() accepts each and tries to dispatch it; the ones past the queue are
  // rejected by ThreadManager::add().
  std::vector<shared_ptr<TSocket> > fired;
  int droppedWhileSending = 0;
  for (int i = 0; i < 6; ++i) {
    try {
      auto s = connect();
      try {
        ParentServiceClient c(make_shared<TBinaryProtocol>(s));
        c.send_incrementGeneration();
        fired.push_back(s);
      } catch (const TTransportException& e) {
        // serve() can reject and drop a client before its request is fully
        // written. Windows then fails the send with an aborted connection,
        // so that client never gets to read end-of-stream below. It is a drop
        // all the same; a send that merely timed out is not.
        if (e.getType() != TTransportException::TIMED_OUT) {
          ++droppedWhileSending;
        }
      }
    } catch (...) {
      // Once serve() is gone (before the fix) a later connect/send may fail.
    }
  }

  // Give serve() time to accept and attempt to dispatch all of them. A positive
  // dispatch timeout makes each rejection wait that long, so allow for several.
  std::this_thread::sleep_for(milliseconds(1500));

  // Two-state assertion: before the fix a rejected dispatch escaped serve().
  BOOST_CHECK_MESSAGE(!serveThrew, "a rejected dispatch ended the server");

  // Non-vacuous: a rejected client is closed by the server, so at least one of
  // the fired clients failed to send or reads end-of-stream. Only true if a
  // rejection happened.
  if (!serveThrew) {
    int closed = droppedWhileSending;
    for (const auto& s : fired) {
      try {
        uint8_t b[1];
        if (s->read(b, 1) == 0) {
          ++closed; // clean end-of-stream
        }
      } catch (const TTransportException& e) {
        // The server drops the connection with the request still unread, which
        // some platforms report to the client as a reset rather than a clean
        // EOF. Either is a drop. Only the one still-queued client that is simply
        // waiting (a receive timeout) is not.
        if (e.getType() != TTransportException::TIMED_OUT) {
          ++closed;
        }
      }
    }
    BOOST_CHECK_MESSAGE(closed > 0,
                        "no client was dropped; the pool was not driven into rejection");
  }

  // Release the parked worker and drop the saturation connections. A connected
  // client is served in a loop, so the single worker would otherwise stay parked
  // on client A's idle keep-alive socket; closing them lets it drain and become
  // free for a fresh client.
  handler->release();
  for (const auto& s : fired) {
    try { s->close(); } catch (...) {}
  }
  try { sockA->close(); } catch (...) {}

  // The server must now serve a fresh client.
  if (!serveThrew) {
    bool served = false;
    for (int attempt = 0; attempt < 40 && !served; ++attempt) {
      try {
        auto z = connect();
        ParentServiceClient cz(make_shared<TBinaryProtocol>(z));
        served = (cz.incrementGeneration() != 0);
        z->close();
      } catch (...) {
        std::this_thread::sleep_for(milliseconds(50));
      }
    }
    BOOST_CHECK_MESSAGE(served, "the server did not serve a fresh client after the rejection");
  }

  // Teardown: stop the server, join, then stop the pool.
  server->stop();
  serveThread.join();
  tm->stop();

  // The concurrent-client count must settle to exactly zero. A negative value
  // would mean a dropped client was counted down twice -- its deleter already
  // adjusts the count when the rejected client goes out of scope in serve().
  int64_t count = -1;
  for (int i = 0; i < 300; ++i) {
    count = server->getConcurrentClientCount();
    if (count == 0) {
      break;
    }
    std::this_thread::sleep_for(milliseconds(10));
  }
  BOOST_CHECK_EQUAL(count, 0);
}

BOOST_AUTO_TEST_SUITE(dispatch_rejection)

// Non-blocking dispatch (timeout -1): a full pending queue is rejected at once
// with TooManyPendingTasksException.
BOOST_AUTO_TEST_CASE(too_many_pending_tasks_does_not_end_the_server) {
  run_dispatch_rejection_case(-1);
}

// Positive dispatch timeout: a pending queue that stays full past the timeout is
// rejected with TimedOutException.
BOOST_AUTO_TEST_CASE(dispatch_timeout_does_not_end_the_server) {
  run_dispatch_rejection_case(100);
}

BOOST_AUTO_TEST_SUITE_END()

// ---------------------------------------------------------------------------
// A worker thread that cannot be started must not stop the whole server.
//
// TThreadedServer::onClientConnected() starts a worker thread for each accepted
// client.  If the thread cannot be started (for example the process is out of
// threads) std::thread's constructor throws.  That exception is not a
// TTransportException, so the accept loop's only catch does not handle it: it
// unwinds out of serve() and the server stops accepting.  The fix catches the
// failure, drops the one client, and keeps serving.
//
// A real std::thread cannot be made to fail on demand, so a thread whose
// start() always throws is injected through the ThreadFactory that the
// TThreadedServer constructor accepts.
// ---------------------------------------------------------------------------

#include <chrono>
#include <thread>
#include <thrift/concurrency/Exception.h>
#include <thrift/concurrency/Thread.h>
#include <thrift/concurrency/ThreadFactory.h>

using apache::thrift::concurrency::Runnable;
using apache::thrift::concurrency::SystemResourceException;
using apache::thrift::concurrency::Thread;
using apache::thrift::concurrency::ThreadFactory;

// A Thread whose start() always fails, simulating the OS refusing to create
// another worker thread.  It is created non-detached, matching TThreadedServer's
// default ThreadFactory(false).  Destroying this never-started thread is safe only
// because of the null-guard added to ~Thread()/join() in Thread.h (part of this
// fix): without it, ~Thread dereferences the null thread_ and crashes.
class FailingThread : public Thread {
public:
  FailingThread(bool detached,
                shared_ptr<Runnable> runnable,
                shared_ptr<std::atomic<int> > startAttempts)
    : Thread(detached, runnable), startAttempts_(startAttempts) {}

  void start() override {
    ++(*startAttempts_);
    throw SystemResourceException("cannot create worker thread");
  }

private:
  shared_ptr<std::atomic<int> > startAttempts_;
};

// A ThreadFactory that hands out FailingThreads and counts how many times a
// worker-thread start was attempted, so the test can prove the failing path was
// actually exercised.
class FailingThreadFactory : public ThreadFactory {
public:
  explicit FailingThreadFactory(shared_ptr<std::atomic<int> > startAttempts)
    : ThreadFactory(false), startAttempts_(startAttempts) {}

  shared_ptr<Thread> newThread(shared_ptr<Runnable> runnable) const override {
    return std::make_shared<FailingThread>(isDetached(), runnable, startAttempts_);
  }

private:
  shared_ptr<std::atomic<int> > startAttempts_;
};

BOOST_AUTO_TEST_SUITE(ThreadStartFailure)

BOOST_AUTO_TEST_CASE(test_threaded_server_thread_start_failure) {
  BOOST_TEST_MESSAGE("Testing that a failed worker-thread start does not stop the server");

  shared_ptr<std::atomic<int> > startAttempts = std::make_shared<std::atomic<int> >(0);
  shared_ptr<TProcessor> processor
      = make_shared<ParentServiceProcessor>(make_shared<ParentHandler>());
  shared_ptr<TServerSocket> serverSocket = make_shared<TServerSocket>("localhost", 0);
  shared_ptr<TServerReadyEventHandler> readyHandler = make_shared<TServerReadyEventHandler>();

  // Allocated with new, not on the stack: on the (unfixed) regression path
  // serve() unwinds and leaves a client in activeClientMap_ that can never be
  // drained, and running ~TThreadedServer would re-enter the server's own maps
  // while they are being destroyed (a use-after-free).  The server is therefore
  // deleted only when serve() returned normally (the fixed path); the shipped
  // code always takes that branch, so this test does not leak in CI.
  auto* server = new TThreadedServer(processor,
                                     serverSocket,
                                     make_shared<TTransportFactory>(),
                                     make_shared<TBinaryProtocolFactory>(),
                                     make_shared<FailingThreadFactory>(startAttempts));
  server->setServerEventHandler(readyHandler);

  std::atomic<bool> serveEnded(false);
  std::atomic<bool> serveThrew(false);
  std::string serveError;

  std::thread serveThread([&]() {
    try {
      server->serve();
    } catch (const std::exception& ex) {
      serveThrew = true;
      serveError = ex.what();
    } catch (...) {
      serveThrew = true;
      serveError = "unknown exception";
    }
    serveEnded = true;
  });

  // Wait (bounded) until the server is listening so a client can connect.
  bool listening = false;
  for (int i = 0; i < 200 && !listening && !serveEnded; ++i) {
    {
      Synchronized sync(*readyHandler);
      listening = readyHandler->isListening();
    }
    if (!listening) {
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
  }

  if (listening) {
    int port = serverSocket->getPort();

    // First client: drives accept() -> onClientConnected(), whose attempt to
    // start a worker thread throws.  Wait until that attempt is observed (it
    // happens in both the fixed and unfixed builds) or serve() ends.
    //
    // Connecting is not enough: TServerSocket::listen() sets TCP_DEFER_ACCEPT,
    // so accept() does not wake until the client sends data.  Send one byte so
    // the server accepts promptly; it is never read, because the worker thread
    // that would process it never starts.
    shared_ptr<TSocket> clientSock1 = make_shared<TSocket>("localhost", port);
    try {
      clientSock1->open();
      uint8_t wake = 0;
      clientSock1->write(&wake, 1);
    } catch (const std::exception&) {}
    for (int i = 0; i < 200 && startAttempts->load() < 1 && !serveEnded; ++i) {
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    // Second client: in the fixed build the accept loop is still running and
    // processes it (a second start attempt); in the unfixed build serve() has
    // already unwound, so although the listening socket is still open and the
    // connect succeeds, nothing accepts it and no second attempt happens.
    shared_ptr<TSocket> clientSock2 = make_shared<TSocket>("localhost", port);
    try {
      clientSock2->open();
      uint8_t wake = 0;
      clientSock2->write(&wake, 1);
    } catch (const std::exception&) {}
    for (int i = 0; i < 200 && startAttempts->load() < 2 && !serveEnded; ++i) {
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    try { clientSock1->close(); } catch (...) {}
    try { clientSock2->close(); } catch (...) {}
  }

  // Snapshot the observations before stopping the server: stop() makes serve()
  // return in the fixed build, which would otherwise erase the distinction.
  bool stillServing = !serveEnded.load();
  int attempts = startAttempts->load();

  // Settle the serve thread before asserting, so serveError is safe to read and
  // a failing assertion cannot leave a running thread behind to std::terminate.
  try { server->stop(); } catch (...) {}
  serveThread.join();

  bool threw = serveThrew.load();
  std::string err = serveError;

  BOOST_REQUIRE_MESSAGE(listening, "server never reached the listening state");

  // The failing start path must have run at least once, otherwise the test
  // proves nothing (the connection never reached onClientConnected).
  BOOST_CHECK_MESSAGE(attempts >= 1,
      "no worker-thread start was ever attempted; the client did not reach onClientConnected");

  // FIXED behavior, which FAILS against the unfixed code:
  //  - serve() is still running after the failed start (serveEnded == false), and
  //  - the server kept accepting, so a second start was attempted.
  BOOST_CHECK_MESSAGE(stillServing,
      "serve() stopped after a worker thread failed to start (threw=" << threw
      << ", error=\"" << err << "\"); the server should drop the client and keep serving");
  BOOST_CHECK_MESSAGE(attempts >= 2,
      "server stopped accepting after a failed worker-thread start; start attempts observed = "
      << attempts << " (expected >= 2)");

  if (!threw) {
    delete server;
  }
  // else: regression path, server deliberately leaked (see note above).
}

BOOST_AUTO_TEST_SUITE_END()
