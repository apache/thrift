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

#include <boost/chrono/duration.hpp>
#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <boost/thread/thread.hpp>
#include <thrift/TOutput.h>
#include <thrift/transport/TPipe.h>
#include <thrift/transport/TPipeServer.h>
#include <atomic>
#include <chrono>
#include <functional>
#include <future>
#include <memory>
#include <string>
#include <thread>
#include <vector>

using apache::thrift::TOutput;
using apache::thrift::transport::TPipeServer;
using apache::thrift::transport::TPipe;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using namespace apache::thrift;

BOOST_AUTO_TEST_SUITE(TPipeInterruptTest)

// TODO: duplicate the test cases in TSocketInterruptTest for pipes,
// once pipes implement interruptChildren

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
  boost::this_thread::sleep(boost::posix_time::milliseconds(10));
  pipe->interrupt();
}

BOOST_AUTO_TEST_CASE(stress_pipe_accept_interruption) {
  int interruptIters = 10;

  for (int i = 0; i < interruptIters; ++i)
  {
    TPipeServer pipeServer("TPipeInterruptTest");
    pipeServer.listen();
    boost::thread acceptThread(std::bind(acceptWorker, &pipeServer));
    boost::thread interruptThread(std::bind(interruptWorker, &pipeServer));
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

// Closing a transport from another thread must wake up a read or write that is
// waiting on it, and that call must end as interrupted.
static void checkCloseWakesUp(std::shared_ptr<TTransport> transport, BackgroundCall& call) {
  BOOST_REQUIRE_MESSAGE(!call.finishesWithin(settleTime),
                        "the call did not wait for the peer, it " << call.outcome());
  transport->close();
  BOOST_REQUIRE_MESSAGE(call.finishesWithin(wakeupTime), "close() did not wake up the call");
  BOOST_CHECK_MESSAGE(call.interrupted(), "the call " << call.outcome());
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
