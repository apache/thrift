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
#include <memory>

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

struct StressState {
  StressState() : pipe(new TPipe()), stop(false) {}
  // the workers hold a reference, so both of these outlive any straggler
  // thread that the test gave up waiting for
  std::shared_ptr<TPipe> pipe;
  std::atomic<bool> stop;
};

// Read from the transport the way a client handler does.
static void readWorker(std::shared_ptr<StressState> state) {
  uint8_t buf[16];
  while (!state->stop.load()) {
    try {
      if (state->pipe->isOpen())
        state->pipe->read(buf, sizeof(buf));
    } catch (...) { /*the transport closing underneath us is expected*/ }
  }
}

static void closeWorker(std::shared_ptr<StressState> state, const char* pipename, int iters) {
  for (int i = 0; i < iters; ++i) {
    state->pipe->close();
    state->pipe->setPipeHandle(createPipeInstance(pipename));
  }
}

// Reading on one thread while another thread closes the transport must not
// leave the reader using state that the close has already torn down.  The
// loops below keep that race open long enough to hit it, so a transport that
// gets this wrong dies here instead of passing.
BOOST_AUTO_TEST_CASE(stress_pipe_close_during_use) {
  const char* pipename = "\\\\.\\pipe\\TPipeInterruptTest";
  int closeIters = 20000;

  QuietOutput quiet;

  HANDLE pipeInstance = createPipeInstance(pipename);
  BOOST_REQUIRE(pipeInstance != INVALID_HANDLE_VALUE);

  std::shared_ptr<StressState> state(new StressState());
  state->pipe->setPipeHandle(pipeInstance);
  BOOST_REQUIRE(state->pipe->isOpen());

  boost::thread readThread(std::bind(readWorker, state));
  boost::thread closeThread(std::bind(closeWorker, state, pipename, closeIters));

  bool closeDone = closeThread.try_join_for(boost::chrono::seconds(60));
  state->stop.store(true);
  BOOST_CHECK_MESSAGE(closeDone, "the close loop did not finish");
  BOOST_CHECK_MESSAGE(readThread.try_join_for(boost::chrono::seconds(10)),
                      "the reader did not finish");
}

BOOST_AUTO_TEST_SUITE_END()
#endif
