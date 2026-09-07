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

// One request that cannot be processed is one request. These cover the servers
// built on TServerFramework, whose client loop lives in TConnectedClient::run():
// what happens to the other connections when processing a single message throws
// something that is not a Thrift exception.
//
// None of these fail an assertion when the behaviour regresses. The process
// ends instead, and the whole binary goes with it -- which is exactly the
// property under test.

#define BOOST_TEST_MODULE TServerExceptionTest
#include <boost/test/unit_test.hpp>

#include <memory>
#include <new>

#include <thrift/TProcessor.h>
#include <thrift/concurrency/ThreadFactory.h>
#include <thrift/concurrency/ThreadManager.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/server/TThreadPoolServer.h>
#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "gen-cpp/ParentService.h"

using apache::thrift::TProcessor;
using apache::thrift::concurrency::Runnable;
using apache::thrift::concurrency::Thread;
using apache::thrift::concurrency::ThreadFactory;
using apache::thrift::concurrency::ThreadManager;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TBinaryProtocolFactory;
using apache::thrift::protocol::TProtocol;
using apache::thrift::server::TServer;
using apache::thrift::server::TSimpleServer;
using apache::thrift::server::TThreadPoolServer;
using apache::thrift::server::TThreadedServer;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TBufferedTransportFactory;
using apache::thrift::transport::TServerSocket;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransportException;
using std::make_shared;
using std::shared_ptr;

// The generated service lives in apache::thrift::test; alias it rather than
// pulling all of apache::thrift into scope.
namespace gen = apache::thrift::test;

namespace {

// Not "localhost": TServerSocket::listen() resolves with AI_PASSIVE|AI_V4MAPPED
// and TSocket::open() with AI_PASSIVE|AI_ADDRCONFIG, so where /etc/hosts maps
// localhost to ::1 but the loopback carries no IPv6 address, the server binds
// ::1 and the client dials 127.0.0.1 and is refused (THRIFT-6191). Naming the
// address takes that out of the test.
const char* const kHost = "127.0.0.1";

struct Handler : public gen::ParentServiceIf {
  void addString(const std::string& s) override { strings_.push_back(s); }
  void getStrings(std::vector<std::string>& _return) override { _return = strings_; }
  std::vector<std::string> strings_;

  int32_t incrementGeneration() override { return 0; }
  int32_t getGeneration() override { return 0; }
  void getDataWait(std::string&, const int32_t) override {}
  void onewayWait() override {}
  void exceptionWait(const std::string&) override {}
  void unexpectedExceptionWait(const std::string&) override {}
};

// Throws once, the way reading the arguments of a call throws when a declared
// container count cannot be allocated: before any generated per-function
// try/catch has been entered, so nothing turns it into a TApplicationException.
class ThrowsOnceProcessor : public TProcessor {
public:
  explicit ThrowsOnceProcessor(const shared_ptr<TProcessor>& delegate)
    : delegate_(delegate), thrown_(false) {}

  bool process(shared_ptr<TProtocol> in, shared_ptr<TProtocol> out, void* context) override {
    if (!thrown_) {
      thrown_ = true;
      throw std::bad_alloc();
    }
    return delegate_->process(in, out, context);
  }

private:
  shared_ptr<TProcessor> delegate_;
  bool thrown_;
};

struct Serve : public Runnable {
  shared_ptr<TServer> server;
  void run() override { server->serve(); }
};

shared_ptr<TProcessor> throwingProcessor() {
  return make_shared<ThrowsOnceProcessor>(
      make_shared<gen::ParentServiceProcessor>(make_shared<Handler>()));
}

int portOf(const shared_ptr<TServerSocket>& socket) {
  for (int i = 0; i < 200 && socket->getPort() == 0; ++i) {
    THRIFT_SLEEP_USEC(10000);
  }
  return socket->getPort();
}

// Sends one call and does not wait for the reply. A processor that throws
// writes no response, so waiting would hang on a defect this test is not about.
void sendOneCall(int port) {
  auto socket = make_shared<TSocket>(kHost, port);
  socket->open();
  gen::ParentServiceClient client(
      make_shared<TBinaryProtocol>(make_shared<TBufferedTransport>(socket)));
  client.send_addString("first");
  socket->close();
}

bool serves(int port) {
  auto socket = make_shared<TSocket>(kHost, port);
  socket->open();
  gen::ParentServiceClient client(
      make_shared<TBinaryProtocol>(make_shared<TBufferedTransport>(socket)));
  client.addString("second");
  std::vector<std::string> strings;
  client.getStrings(strings);
  socket->close();
  return strings.size() == 1 && strings[0] == "second";
}

// Builds the server, drives one doomed call through it, and reports whether it
// still serves a fresh connection afterwards.
bool survivesOneFailedCall(const shared_ptr<TServer>& server,
                           const shared_ptr<TServerSocket>& socket) {
  auto runnable = make_shared<Serve>();
  runnable->server = server;
  auto thread = make_shared<ThreadFactory>(false)->newThread(runnable);
  thread->start();

  int port = portOf(socket);
  BOOST_REQUIRE_GT(port, 0);

  try {
    sendOneCall(port);
  } catch (const TTransportException&) {
    // the connection may be closed under the call; not what is being tested
  }
  THRIFT_SLEEP_USEC(300000);

  bool alive = serves(port);

  server->stop();
  thread->join();
  return alive;
}

} // namespace

BOOST_AUTO_TEST_SUITE(TServerExceptionTest)

BOOST_AUTO_TEST_CASE(threaded_server_survives_a_processing_exception) {
  auto socket = make_shared<TServerSocket>(kHost, 0);
  auto server = make_shared<TThreadedServer>(throwingProcessor(), socket,
                                             make_shared<TBufferedTransportFactory>(),
                                             make_shared<TBinaryProtocolFactory>());
  BOOST_CHECK(survivesOneFailedCall(server, socket));
}

BOOST_AUTO_TEST_CASE(simple_server_survives_a_processing_exception) {
  auto socket = make_shared<TServerSocket>(kHost, 0);
  auto server = make_shared<TSimpleServer>(throwingProcessor(), socket,
                                           make_shared<TBufferedTransportFactory>(),
                                           make_shared<TBinaryProtocolFactory>());
  BOOST_CHECK(survivesOneFailedCall(server, socket));
}

// Passes today: the thread manager's worker catches what escapes the client
// loop, so this one is a guard against the fix being written in a way that
// only helps the other two.
BOOST_AUTO_TEST_CASE(threadpool_server_survives_a_processing_exception) {
  auto socket = make_shared<TServerSocket>(kHost, 0);
  auto threadManager = ThreadManager::newSimpleThreadManager(2);
  threadManager->threadFactory(make_shared<ThreadFactory>());
  threadManager->start();
  auto server = make_shared<TThreadPoolServer>(throwingProcessor(), socket,
                                               make_shared<TBufferedTransportFactory>(),
                                               make_shared<TBinaryProtocolFactory>(),
                                               threadManager);
  BOOST_CHECK(survivesOneFailedCall(server, socket));
}

BOOST_AUTO_TEST_SUITE_END()
