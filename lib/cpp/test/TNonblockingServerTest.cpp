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
#include <boost/smart_ptr.hpp>

#include "thrift/concurrency/Thread.h"
#include "thrift/server/TNonblockingServer.h"

#include "gen-cpp/ParentService.h"

using namespace apache::thrift;

struct Handler : public test::ParentServiceIf {
  void addString(const std::string& s) { strings_.push_back(s); }
  void getStrings(std::vector<std::string>& _return) { _return = strings_; }
  std::vector<std::string> strings_;

  // dummy overrides not used in this test
  int32_t incrementGeneration() { return 0; }
  int32_t getGeneration() { return 0; }
  void getDataWait(std::string&, int32_t) {}
  void onewayWait() {}
  void exceptionWait(const std::string&) {}
  void unexpectedExceptionWait(const std::string&) {}
};

class Fixture {
private:
  struct Runner : public concurrency::Runnable {
    boost::shared_ptr<server::TNonblockingServer> server;
    bool error;
    virtual void run() {
      error = false;
      try {
        server->serve();
      } catch (const TException& x) {
        error = true;
      }
    }
  };

protected:
  Fixture() : processor(new test::ParentServiceProcessor(boost::make_shared<Handler>())) {}

  int startServer(int port) {
    boost::scoped_ptr<concurrency::ThreadFactory> threadFactory(
        new concurrency::PlatformThreadFactory(
#if !USE_BOOST_THREAD && !USE_STD_THREAD
            concurrency::PlatformThreadFactory::OTHER,
            concurrency::PlatformThreadFactory::NORMAL,
            1,
#endif
            true));

    int retry_count = port ? 10 : 0;
    for (int p = port; p <= port + retry_count; p++) {
      server.reset(new server::TNonblockingServer(processor, p));
      boost::shared_ptr<Runner> runner(new Runner);
      runner->server = server;
      thread = threadFactory->newThread(runner);
      thread->start();
      // wait 50ms for the server to begin listening
      THRIFT_SLEEP_USEC(50000);
      if (!runner->error) {
        return p;
      }
    }
    throw transport::TTransportException(transport::TTransportException::NOT_OPEN,
                                         "Failed to start server.");
  }

  bool canCommunicate(int serverPort) {
    boost::shared_ptr<transport::TSocket> socket(new transport::TSocket("localhost", serverPort));
    socket->open();
    test::ParentServiceClient client(boost::make_shared<protocol::TBinaryProtocol>(
        boost::make_shared<transport::TFramedTransport>(socket)));
    client.addString("foo");
    std::vector<std::string> strings;
    client.getStrings(strings);
    return strings.size() == 1 && !(strings[0].compare("foo"));
  }

private:
  boost::shared_ptr<test::ParentServiceProcessor> processor;
  boost::shared_ptr<concurrency::Thread> thread;

protected:
  boost::shared_ptr<server::TNonblockingServer> server;
};

BOOST_AUTO_TEST_SUITE(TNonblockingServerTest)

BOOST_FIXTURE_TEST_CASE(get_specified_port, Fixture) {
  int specified_port = startServer(12345);
  BOOST_REQUIRE_GE(specified_port, 12345);
  BOOST_REQUIRE_EQUAL(server->getListenPort(), specified_port);
  BOOST_CHECK(canCommunicate(specified_port));

  server->stop();
  BOOST_CHECK_EQUAL(server->getListenPort(), specified_port);
}

BOOST_FIXTURE_TEST_CASE(get_assigned_port, Fixture) {
  int specified_port = startServer(0);
  BOOST_REQUIRE_EQUAL(specified_port, 0);
  int assigned_port = server->getListenPort();
  BOOST_REQUIRE_NE(assigned_port, 0);
  BOOST_CHECK(canCommunicate(assigned_port));

  server->stop();
  BOOST_CHECK_EQUAL(server->getListenPort(), 0);
}

BOOST_AUTO_TEST_SUITE_END()
