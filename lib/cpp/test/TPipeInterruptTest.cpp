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

#ifdef _WIN32

#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

#include <thrift/TOutput.h>
#include <thrift/TProcessor.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/TPipe.h>
#include <thrift/transport/TPipeServer.h>
#include <atomic>
#include <chrono>
#include <functional>
#include <future>
#include <memory>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

using apache::thrift::TOutput;
using apache::thrift::protocol::TBinaryProtocolFactory;
using apache::thrift::protocol::TProtocol;
using apache::thrift::server::TServerEventHandler;
using apache::thrift::server::TSimpleServer;
using apache::thrift::server::TThreadedServer;
using apache::thrift::transport::TPipeServer;
using apache::thrift::transport::TPipe;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TTransportFactory;
using namespace apache::thrift;

BOOST_AUTO_TEST_SUITE(TPipeInterruptTest)

// The peek() cases of TSocketInterruptTest have no counterpart here, because
// TPipe::peek() does not wait for the peer.

BOOST_AUTO_TEST_CASE(test_interrupt_before_accept) {
  TPipeServer pipe1("TPipeInterruptTest");
  pipe1.listen();
  pipe1.interrupt();
  BOOST_CHECK_THROW(pipe1.accept(), TTransportException);
}

static void acceptWorker(TPipeServer *pipe) {
  try
  {
    for (;;)
    {
      std::shared_ptr<TTransport> temp = pipe->accept();
    }
  }
  catch (...) {/*just want to make sure nothing crashes*/ }
}

static void interruptWorker(TPipeServer *pipe) {
  std::this_thread::sleep_for(std::chrono::milliseconds(10));
  pipe->interrupt();
}

BOOST_AUTO_TEST_CASE(stress_pipe_accept_interruption) {
  int interruptIters = 10;

  for (int i = 0; i < interruptIters; ++i)
  {
    TPipeServer pipeServer("TPipeInterruptTest");
    pipeServer.listen();
    std::thread acceptThread(std::bind(acceptWorker, &pipeServer));
    std::thread interruptThread(std::bind(interruptWorker, &pipeServer));
    try
    {
      for (;;)
      {
        TPipe client("TPipeInterruptTest");
        client.setConnTimeout(1);
        client.open();
      }
    } catch (...) { /*just testing for crashes*/ }
    interruptThread.join();
    acceptThread.join();
  }
}

// Runs a call on a thread of its own, so that a test can see whether the call is
// still blocked, and how it ended.  The thread owns everything the call uses: a
// test that gives up on a call that never returns leaves the thread behind
// instead of pulling the transport out from under it.
class BackgroundCall {
public:
  explicit BackgroundCall(std::function<void()> call) : state_(new State) {
    done_ = state_->done.get_future();
    std::shared_ptr<State> state = state_;
    std::thread([state, call]() {
      try {
        call();
        state->outcome = "returned";
      } catch (const TTransportException& ex) {
        state->interrupted = ex.getType() == TTransportException::INTERRUPTED;
        state->outcome = "threw TTransportException type "
                         + std::to_string(static_cast<int>(ex.getType())) + ": " + ex.what();
      } catch (const std::exception& ex) {
        state->outcome = std::string("threw ") + ex.what();
      }
      state->done.set_value();
    }).detach();
  }

  bool finishesWithin(std::chrono::milliseconds timeout) {
    return done_.wait_for(timeout) == std::future_status::ready;
  }

  // only meaningful once the call has finished
  bool interrupted() const { return state_->interrupted; }
  const std::string& outcome() const { return state_->outcome; }

private:
  struct State {
    State() : interrupted(false) {}
    std::promise<void> done;
    bool interrupted;
    std::string outcome;
  };

  std::shared_ptr<State> state_;
  std::future<void> done_;
};

// A connected pair of named pipe transports.  The client end is the one that
// TPipe::open() makes and the server end is the one that TPipeServer::accept()
// returns; the two are implemented differently.  Neither end sends anything.
struct ConnectedPipes {
  explicit ConnectedPipes(const std::string& pipename)
    : server(new TPipeServer(pipename, 1024)), client(new TPipe(pipename)) {
    server->listen();
    client->open();
    accepted = server->accept();
  }

  std::shared_ptr<TPipeServer> server;
  std::shared_ptr<TPipe> client;
  std::shared_ptr<TTransport> accepted;
};

// A call that is going to wait for the peer has started waiting after this long.
static const std::chrono::milliseconds settleTime(200);
// Waking up a call should never take anywhere near this long.
static const std::chrono::milliseconds wakeupTime(10000);

// A read or write that waits for the peer must stop waiting when another thread
// runs interrupt(), and end as interrupted.
static void checkWakesUp(BackgroundCall& call, const char* how, std::function<void()> interrupt) {
  BOOST_REQUIRE_MESSAGE(!call.finishesWithin(settleTime),
                        "the call did not wait for the peer, it " << call.outcome());
  interrupt();
  BOOST_REQUIRE_MESSAGE(call.finishesWithin(wakeupTime), how << " did not wake up the call");
  BOOST_CHECK_MESSAGE(call.interrupted(), "the call " << call.outcome());
}

// Closing a transport from another thread must wake up a read or write that is
// waiting on it, and that call must end as interrupted.
static void checkCloseWakesUp(std::shared_ptr<TTransport> transport, BackgroundCall& call) {
  checkWakesUp(call, "close()", [transport]() { transport->close(); });
}

// TPipeServer::interruptChildren() does the same to all the pipes that
// accept() returned.
static void checkInterruptChildrenWakesUp(std::shared_ptr<TPipeServer> server,
                                          BackgroundCall& call) {
  checkWakesUp(call, "interruptChildren()", [server]() { server->interruptChildren(); });
}

BOOST_AUTO_TEST_CASE(test_close_wakes_client_read) {
  ConnectedPipes pipes("TPipeInterruptTest.ClientRead");
  std::shared_ptr<TTransport> transport = pipes.client;
  BackgroundCall reader([transport]() {
    uint8_t buf[16];
    transport->read(buf, sizeof(buf));
  });
  checkCloseWakesUp(transport, reader);
}

BOOST_AUTO_TEST_CASE(test_close_wakes_server_read) {
  ConnectedPipes pipes("TPipeInterruptTest.ServerRead");
  std::shared_ptr<TTransport> transport = pipes.accepted;
  BackgroundCall reader([transport]() {
    uint8_t buf[16];
    transport->read(buf, sizeof(buf));
  });
  checkCloseWakesUp(transport, reader);
}

BOOST_AUTO_TEST_CASE(test_close_wakes_server_write) {
  ConnectedPipes pipes("TPipeInterruptTest.ServerWrite");
  std::shared_ptr<TTransport> transport = pipes.accepted;
  BackgroundCall writer([transport]() {
    // far more than the pipe buffers hold, and the client never reads
    std::vector<uint8_t> data(4 * 1024 * 1024);
    transport->write(&data[0], static_cast<uint32_t>(data.size()));
  });
  checkCloseWakesUp(transport, writer);
}

BOOST_AUTO_TEST_CASE(test_interruptable_child_read) {
  ConnectedPipes pipes("TPipeInterruptTest.ChildRead");
  std::shared_ptr<TTransport> transport = pipes.accepted;
  BackgroundCall reader([transport]() {
    uint8_t buf[16];
    transport->read(buf, sizeof(buf));
  });
  checkInterruptChildrenWakesUp(pipes.server, reader);
}

BOOST_AUTO_TEST_CASE(test_interruptable_child_write) {
  ConnectedPipes pipes("TPipeInterruptTest.ChildWrite");
  std::shared_ptr<TTransport> transport = pipes.accepted;
  BackgroundCall writer([transport]() {
    // far more than the pipe buffers hold, and the client never reads
    std::vector<uint8_t> data(4 * 1024 * 1024);
    transport->write(&data[0], static_cast<uint32_t>(data.size()));
  });
  checkInterruptChildrenWakesUp(pipes.server, writer);
}

// Once interrupted, a child reads no more, even though the client has sent
// data, so that a client that keeps sending cannot keep TServer::stop() from
// finishing either.  This is how TServerSocket's children behave, too.
static void checkInterruptedChildReadsNoMore(const std::string& pipename, bool consumeFirstByte) {
  ConnectedPipes pipes(pipename);
  std::shared_ptr<TPipe> child = std::dynamic_pointer_cast<TPipe>(pipes.accepted);
  BOOST_REQUIRE(child);
  const uint8_t data[4] = {1, 2, 3, 4};
  pipes.client->write(data, sizeof(data));
  // the data is there once the read that the child keeps pending has completed
  BOOST_REQUIRE_EQUAL(WAIT_OBJECT_0, WaitForSingleObject(child->getNativeWaitHandle(), 10000));
  uint8_t buf[1];
  if (consumeFirstByte) {
    // the rest of the data stays buffered in the child
    BOOST_REQUIRE_EQUAL(1u, child->read(buf, sizeof(buf)));
  }
  pipes.server->interruptChildren();
  try {
    uint32_t got = child->read(buf, sizeof(buf));
    BOOST_ERROR("the interrupted child still read " << got << " byte(s)");
  } catch (const TTransportException& ex) {
    BOOST_CHECK_EQUAL(TTransportException::INTERRUPTED, ex.getType());
  }
}

BOOST_AUTO_TEST_CASE(test_interrupted_child_ignores_received_data) {
  checkInterruptedChildReadsNoMore("TPipeInterruptTest.ReceivedData", false);
}

BOOST_AUTO_TEST_CASE(test_interrupted_child_ignores_buffered_data) {
  checkInterruptedChildReadsNoMore("TPipeInterruptTest.BufferedData", true);
}

// A read that already waits when the client's data and the interrupt arrive
// together must not return the data either.  The reader is held suspended
// while both arrive, so that its wait finds both of them signalled.
BOOST_AUTO_TEST_CASE(test_waiting_child_read_ignores_data_that_comes_with_interrupt) {
  ConnectedPipes pipes("TPipeInterruptTest.DataWithInterrupt");
  std::shared_ptr<TPipe> child = std::dynamic_pointer_cast<TPipe>(pipes.accepted);
  BOOST_REQUIRE(child);
  std::shared_ptr<std::promise<HANDLE> > threadHandle(new std::promise<HANDLE>);
  std::future<HANDLE> readerThread = threadHandle->get_future();
  BackgroundCall reader([child, threadHandle]() {
    HANDLE self = nullptr;
    ::DuplicateHandle(::GetCurrentProcess(), ::GetCurrentThread(), ::GetCurrentProcess(), &self,
                      THREAD_SUSPEND_RESUME, FALSE, 0);
    threadHandle->set_value(self);
    uint8_t buf[16];
    child->read(buf, sizeof(buf));
  });
  HANDLE thread = readerThread.get();
  BOOST_REQUIRE(thread != nullptr);
  BOOST_REQUIRE_MESSAGE(!reader.finishesWithin(settleTime),
                        "the read did not wait for the client, it " << reader.outcome());

  BOOST_REQUIRE(::SuspendThread(thread) != static_cast<DWORD>(-1));
  const uint8_t data[4] = {1, 2, 3, 4};
  pipes.client->write(data, sizeof(data));
  // the data is there once the read that the child keeps pending has completed
  BOOST_CHECK_EQUAL(WAIT_OBJECT_0, WaitForSingleObject(child->getNativeWaitHandle(), 10000));
  pipes.server->interruptChildren();
  ::ResumeThread(thread);
  ::CloseHandle(thread);

  BOOST_REQUIRE_MESSAGE(reader.finishesWithin(wakeupTime), "the read did not end");
  BOOST_CHECK_MESSAGE(reader.interrupted(), "the read " << reader.outcome());
}

// Closing the server interrupts its children too, as with TServerSocket.
BOOST_AUTO_TEST_CASE(test_close_server_interrupts_child_read) {
  ConnectedPipes pipes("TPipeInterruptTest.CloseServer");
  std::shared_ptr<TTransport> transport = pipes.accepted;
  BackgroundCall reader([transport]() {
    uint8_t buf[16];
    transport->read(buf, sizeof(buf));
  });
  std::shared_ptr<TPipeServer> server = pipes.server;
  checkWakesUp(reader, "closing the server", [server]() { server->close(); });
}

BOOST_AUTO_TEST_CASE(test_non_interruptable_child_read) {
  const std::string pipename("TPipeInterruptTest.NonInterruptableChildRead");
  TPipeServer server(pipename, 1024);
  server.setInterruptableChildren(false);
  server.listen();
  std::shared_ptr<TPipe> client(new TPipe(pipename));
  client->open();
  std::shared_ptr<TTransport> transport = server.accept();
  BackgroundCall reader([transport]() {
    uint8_t buf[16];
    transport->read(buf, sizeof(buf));
  });
  BOOST_REQUIRE_MESSAGE(!reader.finishesWithin(settleTime),
                        "the read did not wait for the client, it " << reader.outcome());
  server.interruptChildren();
  BOOST_CHECK_MESSAGE(!reader.finishesWithin(settleTime),
                      "interruptChildren() ended the read, it " << reader.outcome());
  server.close();
  BOOST_CHECK_MESSAGE(!reader.finishesWithin(settleTime),
                      "closing the server ended the read, it " << reader.outcome());
  // now only the client going away ends the read
  client->close();
  BOOST_REQUIRE_MESSAGE(reader.finishesWithin(wakeupTime), "the read did not end with the client");
  BOOST_CHECK_MESSAGE(!reader.interrupted(), "the read " << reader.outcome());
}

BOOST_AUTO_TEST_CASE(test_cannot_change_after_listen) {
  TPipeServer server("TPipeInterruptTest.ChangeAfterListen");
  server.listen();
  BOOST_CHECK_THROW(server.setInterruptableChildren(false), std::logic_error);
}

BOOST_AUTO_TEST_CASE(test_can_change_before_listen) {
  // made as an anonymous server and switched to a named one, but not listening
  TPipeServer server;
  server.setAnonymous(false);
  BOOST_CHECK_NO_THROW(server.setInterruptableChildren(false));
}

// Waits for a request that the client never sends.
class WaitingProcessor : public TProcessor {
public:
  bool process(std::shared_ptr<TProtocol> in, std::shared_ptr<TProtocol>, void*) override {
    uint8_t byte;
    in->getTransport()->readAll(&byte, 1);
    return true;
  }
};

// Tells the test when the server listens, so that a client can connect.
class ListeningSignal : public TServerEventHandler {
public:
  ListeningSignal() : listening_(promise_.get_future()) {}
  void preServe() override { promise_.set_value(); }
  bool waitFor(std::chrono::milliseconds timeout) {
    return listening_.wait_for(timeout) == std::future_status::ready;
  }

private:
  std::promise<void> promise_;
  std::future<void> listening_;
};

// TServer::stop() must end serve() while a client is connected and silent, as
// it does for a TServerSocket (THRIFT-3590).
template <typename Server>
static void checkStopWithClientConnected(const std::string& pipename) {
  std::shared_ptr<ListeningSignal> signal(new ListeningSignal);
  std::shared_ptr<TProcessor> processor(new WaitingProcessor);
  std::shared_ptr<TServerTransport> transport(new TPipeServer(pipename, 1024));
  std::shared_ptr<Server> server(new Server(processor, transport,
                                            std::make_shared<TTransportFactory>(),
                                            std::make_shared<TBinaryProtocolFactory>()));
  server->setServerEventHandler(signal);
  BackgroundCall serve([server]() { server->serve(); });
  BOOST_REQUIRE_MESSAGE(signal->waitFor(wakeupTime), "the server did not start listening");

  std::shared_ptr<TPipe> client(new TPipe(pipename));
  client->open();
  // the processor waits for the client once the server counts it
  const std::chrono::steady_clock::time_point deadline
      = std::chrono::steady_clock::now() + wakeupTime;
  while (server->getConcurrentClientCount() < 1 && std::chrono::steady_clock::now() < deadline) {
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
  }
  BOOST_REQUIRE_MESSAGE(server->getConcurrentClientCount() == 1,
                        "the server did not take the client on");
  BOOST_REQUIRE_MESSAGE(!serve.finishesWithin(settleTime),
                        "serve() ended early, it " << serve.outcome());

  server->stop();
  BOOST_CHECK_MESSAGE(serve.finishesWithin(wakeupTime),
                      "stop() did not end serve() while a client was connected");
  client->close();
}

BOOST_AUTO_TEST_CASE(test_stop_threaded_server_with_client_connected) {
  checkStopWithClientConnected<TThreadedServer>("TPipeInterruptTest.StopThreadedServer");
}

BOOST_AUTO_TEST_CASE(test_stop_simple_server_with_client_connected) {
  checkStopWithClientConnected<TSimpleServer>("TPipeInterruptTest.StopSimpleServer");
}

// A named pipe instance to hand to TPipe::setPipeHandle.  Nothing ever
// connects to it: the transport users below only need an implementation to
// work with, not a peer.
static HANDLE createPipeInstance(const char* pipename) {
  return CreateNamedPipeA(pipename, PIPE_ACCESS_DUPLEX | FILE_FLAG_OVERLAPPED,
                          PIPE_TYPE_BYTE | PIPE_READMODE_BYTE, PIPE_UNLIMITED_INSTANCES, 1024, 1024,
                          0, nullptr);
}

// The reader below fails thousands of reads a second and the library logs
// every one of them; keep that out of the test output.
static void discardOutput(const char*) {}

struct QuietOutput {
  QuietOutput() { TOutput::instance().setOutputFunction(discardOutput); }
  ~QuietOutput() { TOutput::instance().setOutputFunction(TOutput::errorTimeWrapper); }
};

// Reading on one thread while another thread closes the transport must not
// leave the reader using state that the close has already torn down.  The
// loops below keep that race open long enough to hit it, so a transport that
// gets this wrong dies here instead of passing.
BOOST_AUTO_TEST_CASE(stress_pipe_close_during_use) {
  const char* pipename = "\\\\.\\pipe\\TPipeInterruptTest.CloseDuringUse";
  int closeIters = 20000;

  QuietOutput quiet;

  HANDLE pipeInstance = createPipeInstance(pipename);
  BOOST_REQUIRE(pipeInstance != INVALID_HANDLE_VALUE);

  std::shared_ptr<TPipe> pipe(new TPipe());
  pipe->setPipeHandle(pipeInstance);
  BOOST_REQUIRE(pipe->isOpen());

  std::shared_ptr<std::atomic<bool> > stop(new std::atomic<bool>(false));
  BackgroundCall reader([pipe, stop]() {
    // read from the transport the way a client handler does
    uint8_t buf[16];
    while (!stop->load()) {
      try {
        if (pipe->isOpen())
          pipe->read(buf, sizeof(buf));
      } catch (const TTransportException&) {
        // the transport closing underneath us is expected
      }
    }
  });
  BackgroundCall closer([pipe, pipename, closeIters]() {
    for (int i = 0; i < closeIters; ++i) {
      pipe->close();
      pipe->setPipeHandle(createPipeInstance(pipename));
    }
  });

  bool closeDone = closer.finishesWithin(std::chrono::seconds(60));
  stop->store(true);
  BOOST_CHECK_MESSAGE(closeDone, "the close loop did not finish");
  BOOST_CHECK_MESSAGE(reader.finishesWithin(std::chrono::seconds(10)), "the reader did not finish");
}

BOOST_AUTO_TEST_SUITE_END()
#endif
