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
#include <boost/test/auto_unit_test.hpp>
#include <boost/bind.hpp>
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include <thrift/server/TThreadedServer.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransport.h>
#include "gen-cpp/ParentService.h"
#include "TestPortFixture.h"
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
using apache::thrift::transport::TTransportFactory;
using apache::thrift::server::TServerEventHandler;
using apache::thrift::server::TThreadedServer;
using apache::thrift::test::ParentServiceClient;
using apache::thrift::test::ParentServiceIf;
using apache::thrift::test::ParentServiceProcessor;

/**
 * preServe runs after listen() is successful, when we can connect
 */
class TServerReadyEventHandler : public TServerEventHandler, public Monitor
{
public:
  TServerReadyEventHandler() : isListening_(false), accepted_(0) {}
  virtual ~TServerReadyEventHandler() {}
  virtual void preServe() {
    Synchronized sync(*this);
    isListening_ = true;
    notify();
  }
  virtual void* createContext(boost::shared_ptr<TProtocol> input,
                              boost::shared_ptr<TProtocol> output) {
    Synchronized sync(*this);
    ++accepted_;
    notify();

    (void)input;
    (void)output;
    return NULL;
  }
  bool isListening() const { return isListening_; }
  uint64_t acceptedCount() const { return accepted_; }
private:
  bool isListening_;
  uint64_t accepted_;
};

class ParentHandler : virtual public ParentServiceIf {
public:
  ParentHandler() : generation_(0) {}

  int32_t incrementGeneration() {
    Guard g(mutex_);
    return ++generation_;
  }

  int32_t getGeneration() {
    Guard g(mutex_);
    return generation_;
  }

  void addString(const std::string& s) {
    Guard g(mutex_);
    strings_.push_back(s);
  }

  void getStrings(std::vector<std::string>& _return) {
    Guard g(mutex_);
    _return = strings_;
  }

  void getDataWait(std::string& _return, int32_t length) {
  }

  void onewayWait() {
  }

  void exceptionWait(const std::string& message) {
  }

  void unexpectedExceptionWait(const std::string& message) {
  }

protected:
  Mutex mutex_;
  int32_t generation_;
  std::vector<std::string> strings_;
};

class TServerIntegrationTestFixture : public TestPortFixture
{
public:
  TServerIntegrationTestFixture() :
      pServer(new TThreadedServer(
                    boost::shared_ptr<ParentServiceProcessor>(new ParentServiceProcessor(
                            boost::shared_ptr<ParentServiceIf>(new ParentHandler))),
                    boost::shared_ptr<TServerTransport>(new TServerSocket("localhost", m_serverPort)),
                    boost::shared_ptr<TTransportFactory>(new TTransportFactory),
                    boost::shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory))),
      pEventHandler(boost::shared_ptr<TServerReadyEventHandler>(new TServerReadyEventHandler))
  {
    pServer->setServerEventHandler(pEventHandler);
  }

  void startServer() {
    pServerThread.reset(new boost::thread(boost::bind(&TThreadedServer::serve, pServer.get())));

    // block until listen() completes so clients will be able to connect
    Synchronized sync(*(pEventHandler.get()));
    while (!pEventHandler->isListening()) {
        pEventHandler->wait();
    }

    BOOST_MESSAGE("server is listening");
  }

  void blockUntilAccepted(uint64_t numAccepted) {
    Synchronized sync(*(pEventHandler.get()));
    while (pEventHandler->acceptedCount() < numAccepted) {
        pEventHandler->wait();
    }

    BOOST_MESSAGE(boost::format("server has accepted %1%") % numAccepted);
  }

  void stopServer() {
    pServer->stop();
    BOOST_MESSAGE("server stop completed");
    pServerThread->join();
    BOOST_MESSAGE("server thread joined");
  }

  ~TServerIntegrationTestFixture() {
    stopServer();
  }

  void delayClose(boost::shared_ptr<TTransport> toClose) {
    boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
    toClose->close();
  }

  boost::shared_ptr<TThreadedServer> pServer;
  boost::shared_ptr<TServerReadyEventHandler> pEventHandler;
  boost::shared_ptr<boost::thread> pServerThread;
};

BOOST_FIXTURE_TEST_SUITE ( TServerIntegrationTest, TServerIntegrationTestFixture )

BOOST_AUTO_TEST_CASE(test_execute_one_request_and_close)
{
    // this test establishes some basic sanity

    startServer();
    boost::shared_ptr<TSocket> pClientSock1(new TSocket("localhost", m_serverPort));
    boost::shared_ptr<TProtocol> pClientProtocol1(new TBinaryProtocol(pClientSock1));
    ParentServiceClient client1(pClientProtocol1);
    pClientSock1->open();
    client1.incrementGeneration();
    pClientSock1->close();
    stopServer();
}

BOOST_AUTO_TEST_CASE(test_stop_with_interruptable_clients_connected)
{
    // This tests THRIFT-2441 new behavior: stopping the server disconnects clients

    startServer();

    boost::shared_ptr<TSocket> pClientSock1(new TSocket("localhost", m_serverPort));
    pClientSock1->open();

    boost::shared_ptr<TSocket> pClientSock2(new TSocket("localhost", m_serverPort));
    pClientSock2->open();

    // Ensure they have been accepted
    blockUntilAccepted(2);

    // The test fixture destructor will force the sockets to disconnect
    // Prior to THRIFT-2441, pServer->stop() would hang until clients disconnected
    stopServer();

    // extra proof the server end disconnected the clients
    uint8_t buf[1];
    BOOST_CHECK_EQUAL(0, pClientSock1->read(&buf[0], 1));   // 0 = disconnected
    BOOST_CHECK_EQUAL(0, pClientSock2->read(&buf[0], 1));   // 0 = disconnected
    pClientSock1->close();
    pClientSock2->close();
}

BOOST_AUTO_TEST_CASE(test_stop_with_uninterruptable_clients_connected)
{
    // This tests pre-THRIFT-2441 behavior: stopping the server blocks until clients
    // disconnect.

    boost::dynamic_pointer_cast<TServerSocket>(pServer->getServerTransport())->
            setInterruptableChildren(false);    // returns to pre-THRIFT-2441 behavior
    startServer();

    boost::shared_ptr<TSocket> pClientSock1(new TSocket("localhost", m_serverPort));
    pClientSock1->open();

    boost::shared_ptr<TSocket> pClientSock2(new TSocket("localhost", m_serverPort));
    pClientSock2->open();

    // Ensure they have been accepted
    blockUntilAccepted(2);

    boost::thread t1(boost::bind(&TServerIntegrationTestFixture::delayClose, this, pClientSock1));
    boost::thread t2(boost::bind(&TServerIntegrationTestFixture::delayClose, this, pClientSock2));

    // Once the clients disconnect the server will stop
    stopServer();

    pClientSock1->close();
    pClientSock2->close();
}
BOOST_AUTO_TEST_SUITE_END()
