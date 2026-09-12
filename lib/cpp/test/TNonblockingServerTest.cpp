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
#include <atomic>
#include <climits>
#include <fstream>
#include <memory>
#include <string>

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

#ifndef _WIN32
#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

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
    shared_ptr<TConfiguration> configuration;
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
        if (configuration) {
          server->setConfiguration(configuration);
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
    // Drain the thread pool while the server is still alive. server is declared
    // after threadManager_, so it is destroyed first, and ~TNonblockingServer
    // deletes every TConnection and the IO threads with their notification
    // pipe. A task still unwinding at that point would call notifyIOThread() --
    // and then close() -- on a deleted connection, which it holds by raw
    // pointer. ThreadManager::stop() returns only once every worker has
    // finished the task in its hands, so afterwards nothing is left to touch
    // the objects about to be destroyed.
    if (threadManager_) {
      threadManager_->stop();
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
    runner->configuration = configuration_;

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
    // Without a timeout a server that never answers makes this block until the
    // whole test binary is killed, which reports as one opaque timeout. The
    // value only has to beat that: every call made here is a localhost
    // round-trip of a few bytes.
    socket->setRecvTimeout(10000);
    socket->open();
    test::ParentServiceClient client(make_shared<protocol::TBinaryProtocol>(
        make_shared<transport::TFramedTransport>(socket)));
    client.addString("foo");
    std::vector<std::string> strings;
    client.getStrings(strings);
    return strings.size() == 1 && !(strings[0].compare("foo"));
  }

  // A raw client that announces a frame of declaredSize (optionally sending
  // bodyBytes of payload) and reports whether the server closed the connection
  // without replying. A receive timeout distinguishes a refusal (the server
  // closed, read returns 0) from acceptance (the server waits for the rest of
  // the frame and the read times out).
  bool serverClosesOnFrame(int serverPort, uint32_t declaredSize, uint32_t bodyBytes) {
    transport::TSocket sock("localhost", serverPort);
    sock.setRecvTimeout(1500);
    sock.open();
    uint32_t netSize = htonl(declaredSize);
    sock.write(reinterpret_cast<uint8_t*>(&netSize), sizeof(netSize));
    if (bodyBytes) {
      std::vector<uint8_t> body(bodyBytes, 0);
      sock.write(body.data(), bodyBytes);
    }
    uint8_t buf[16];
    try {
      return sock.read(buf, sizeof(buf)) == 0;
    } catch (const transport::TTransportException&) {
      return false;
    }
  }

private:
  shared_ptr<event_base> userEventBase_;
  shared_ptr<concurrency::ThreadManager> threadManager_;

protected:
  shared_ptr<TConfiguration> configuration_;

private:

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

  // The configuration stores the frame size as int, so a value above INT_MAX
  // clamps rather than wrapping to a negative int (which would read back as a
  // huge size_t and disable the limit).
  server.setMaxFrameSize(static_cast<size_t>(INT_MAX) + 1000);
  BOOST_CHECK_EQUAL(server.getMaxFrameSize(), static_cast<size_t>(INT_MAX));
}

// Fails the first call the way argument deserialization fails when a declared
// container count does not fit in memory: the exception leaves process(). It
// cannot be raised from the handler instead, because generated dispatch code
// catches std::exception around the handler call and answers with a
// TApplicationException -- the allocation that fails is the one made before
// that point, reading the arguments.
struct FailsFirstCallProcessor : public TProcessor {
  explicit FailsFirstCallProcessor(const shared_ptr<TProcessor>& delegate)
    : delegate_(delegate), failed_(false), consumed_(false) {}

  bool process(shared_ptr<protocol::TProtocol> in,
               shared_ptr<protocol::TProtocol> out,
               void* connectionContext) override {
    // Both pool workers reach this, so the flag has to be claimed atomically:
    // read-then-write let two calls each see it unset and both throw.
    if (!failed_.exchange(true)) {
      // Announce the claim before unwinding, so a caller can wait until the
      // failure has been taken rather than guess.
      {
        Guard g(consumedMonitor_.mutex());
        consumed_ = true;
        consumedMonitor_.notifyAll();
      }
      throw std::bad_alloc();
    }
    return delegate_->process(in, out, connectionContext);
  }

  // Blocks until some call has consumed the injected failure.
  void awaitFailure() {
    Guard g(consumedMonitor_.mutex());
    while (!consumed_) {
      consumedMonitor_.wait();
    }
  }

private:
  shared_ptr<TProcessor> delegate_;
  std::atomic<bool> failed_;
  Monitor consumedMonitor_;
  bool consumed_;
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
  auto failing = make_shared<FailsFirstCallProcessor>(
      make_shared<test::ParentServiceProcessor>(make_shared<Handler>()));
  useThreadPool(failing);

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

  // Wait until that call has actually taken the injected failure. The two pool
  // workers are served in whatever order they are scheduled, so without this
  // the call below can reach process() first, consume the failure itself and
  // get no reply -- leaving the doomed call to be served normally and this test
  // waiting on a reply that is never owed.
  failing->awaitFailure();

  // The finding is that the process does not survive the above. A second
  // connection, served normally, is what says it did.
  BOOST_CHECK_MESSAGE(canCommunicate(port),
                      "the server stopped serving after one failed allocation");
}

#ifndef _WIN32
// The other way one connection used to end the process, and the worse of the
// two: TConnection::transition() throws std::bad_alloc when the read buffer
// cannot be grown to the frame size the peer asked for. transition() runs under
// eventHandler(), which libevent -- a C library -- calls. An exception unwinding
// out of a C frame reaches std::terminate, not a handler, so the process
// aborted rather than merely exiting.
//
// Run in a child, because making an allocation fail means capping the address
// space, and that cannot be undone for the rest of the suite. The child exiting
// 0 is the assertion; before the fix it died on SIGABRT.
BOOST_AUTO_TEST_CASE(allocation_failure_on_the_io_thread_does_not_end_the_process) {
  pid_t pid = fork();
  BOOST_REQUIRE_NE(pid, -1);

  if (pid == 0) {
    struct rlimit limit;
    limit.rlim_cur = limit.rlim_max = 256UL * 1024 * 1024;
    if (setrlimit(RLIMIT_AS, &limit) != 0) {
      _exit(2);
    }

    auto socket = make_shared<transport::TNonblockingServerSocket>(0);
    auto processor = make_shared<test::ParentServiceProcessor>(make_shared<Handler>());
    auto child = make_shared<server::TNonblockingServer>(processor, socket);
    // Above the cap on purpose, so that growing the buffer for the frame below
    // is an allocation that cannot succeed.
    child->setMaxFrameSize(512UL * 1024 * 1024);

    auto factory = make_shared<ThreadFactory>(false);
    struct Serve : public Runnable {
      shared_ptr<server::TNonblockingServer> server;
      void run() override { server->serve(); }
    };
    auto runnable = make_shared<Serve>();
    runnable->server = child;
    auto serveThread = factory->newThread(runnable);
    serveThread->start();

    for (int i = 0; i < 100 && child->getListenPort() == 0; ++i) {
      THRIFT_SLEEP_USEC(10000);
    }

    try {
      transport::TSocket client("localhost", child->getListenPort());
      client.open();
      uint32_t declared = 400UL * 1024 * 1024;
      uint8_t header[4] = {static_cast<uint8_t>(declared >> 24),
                           static_cast<uint8_t>(declared >> 16),
                           static_cast<uint8_t>(declared >> 8),
                           static_cast<uint8_t>(declared)};
      client.write(header, sizeof(header));
      client.flush();
      THRIFT_SLEEP_USEC(1500000);
      client.close();
    } catch (...) {
      _exit(3);
    }

    // Reached only if the allocation failure did not take the process with it.
    child->stop();
    serveThread->join();
    _exit(0);
  }

  int status = 0;
  BOOST_REQUIRE_NE(waitpid(pid, &status, 0), -1);
  BOOST_CHECK_MESSAGE(WIFEXITED(status),
                      "the server process was killed by a signal rather than exiting");
  if (WIFEXITED(status)) {
    BOOST_CHECK_EQUAL(WEXITSTATUS(status), 0);
  }
}
#endif

// The server enforces the maximum frame size from a configuration it is given,
// not only the library default. A frame above the configured maximum -- but far
// below the 16 MB default the server would otherwise apply -- is refused and the
// connection closed. Before the server honoured a configuration this frame was
// accepted and the server waited for its body.
BOOST_FIXTURE_TEST_CASE(honours_configured_max_frame_size, Fixture) {
  configuration_ = make_shared<TConfiguration>();
  configuration_->setMaxFrameSize(1024);
  startServer(0);
  int port = server->getListenPort();

  BOOST_CHECK(serverClosesOnFrame(port, 2000, 0));

  server->stop();
}

// The accepted socket carries the server's configuration, so a frame larger
// than the configured maximum message size is refused before the read buffer is
// grown for it, even when it is below the frame-size ceiling. Before the socket
// carried the configuration its budget was the 100 MB default and this frame was
// accepted.
BOOST_FIXTURE_TEST_CASE(honours_configured_max_message_size, Fixture) {
  configuration_ = make_shared<TConfiguration>(1024 /* maxMessageSize */);
  startServer(0);
  int port = server->getListenPort();

  BOOST_CHECK(serverClosesOnFrame(port, 2000, 0));

  server->stop();
}

// A generous configuration leaves ordinary traffic untouched: a real framed
// request still round-trips.
BOOST_FIXTURE_TEST_CASE(generous_configuration_still_serves, Fixture) {
  configuration_ = make_shared<TConfiguration>();
  startServer(0);
  BOOST_CHECK(canCommunicate(server->getListenPort()));

  server->stop();
}

// The read buffer grows as the payload arrives rather than being reserved on
// the header, so a frame much larger than the ~1 KiB initial reservation is
// assembled over many libevent callbacks, doubling the buffer each time it
// fills. This drives that path with real traffic: two multi-megabyte requests
// must round-trip byte-for-byte. The first grows the buffer far past
// IDLE_READ_BUFFER_LIMIT and then closes, so returnConnection() frees it via
// checkIdleBufferMemLimit(); every request -- the first included -- begins from
// a freed-or-fresh (null) buffer and has to regrow it from nothing. A lost or
// misplaced byte, or a mishandled null buffer, surfaces as a mismatch here. The
// two payloads use distinct fill bytes so that reading stale bytes left in a
// recycled buffer would fail the comparison rather than pass by coincidence.
BOOST_FIXTURE_TEST_CASE(read_buffer_grows_across_callbacks_and_regrows_after_reclaim, Fixture) {
  startServer(0);
  int port = server->getListenPort();
  BOOST_REQUIRE_GT(port, 0);

  // Far larger than the ~1 KiB initial reservation, so assembling either frame
  // forces repeated growth across callbacks; comfortably under the default
  // frame size limit so the frames themselves are accepted.
  const std::string first(2u * 1024 * 1024, 'a');
  const std::string second(2u * 1024 * 1024, 'b');

  {
    auto socket = make_shared<transport::TSocket>("localhost", port);
    socket->open();
    test::ParentServiceClient client(
        make_shared<protocol::TBinaryProtocol>(make_shared<transport::TFramedTransport>(socket)));
    client.addString(first);
    std::vector<std::string> strings;
    client.getStrings(strings);
    BOOST_REQUIRE_EQUAL(strings.size(), 1u);
    BOOST_CHECK(strings[0] == first);
    // socket closes here -> the grown (> IDLE_READ_BUFFER_LIMIT) read buffer is
    // freed when the connection is returned to the pool.
  }

  {
    auto socket = make_shared<transport::TSocket>("localhost", port);
    socket->open();
    test::ParentServiceClient client(
        make_shared<protocol::TBinaryProtocol>(make_shared<transport::TFramedTransport>(socket)));
    client.addString(second);
    std::vector<std::string> strings;
    client.getStrings(strings);
    BOOST_REQUIRE_EQUAL(strings.size(), 2u);
    BOOST_CHECK(strings[0] == first);
    BOOST_CHECK(strings[1] == second);
  }

  server->stop();
}

// The same growth machinery, but with the read buffer reclaimed *between frames
// on a live connection* rather than when the connection closes. APP_SEND_RESULT
// runs checkIdleBufferMemLimit() once every getResizeBufferEveryN() requests;
// forcing that to 1 frees the buffer grown by the first large request before the
// second arrives on the same connection. On the single I/O thread the order is
// fixed -- send the reply, reclaim, then read the next frame -- so the second
// request is guaranteed to regrow the buffer from empty mid-connection. Both
// requests round-tripping intact is the assertion.
BOOST_FIXTURE_TEST_CASE(read_buffer_regrows_after_mid_connection_reclaim, Fixture) {
  startServer(0);
  int port = server->getListenPort();
  BOOST_REQUIRE_GT(port, 0);
  // Reclaim an oversized read buffer after every request instead of every 512,
  // so the free lands between the two frames below rather than at close.
  server->setResizeBufferEveryN(1);

  const std::string first(2u * 1024 * 1024, 'a');
  const std::string second(2u * 1024 * 1024, 'b');

  auto socket = make_shared<transport::TSocket>("localhost", port);
  socket->open();
  test::ParentServiceClient client(
      make_shared<protocol::TBinaryProtocol>(make_shared<transport::TFramedTransport>(socket)));
  client.addString(first);
  client.addString(second);
  std::vector<std::string> strings;
  client.getStrings(strings);
  BOOST_REQUIRE_EQUAL(strings.size(), 2u);
  BOOST_CHECK(strings[0] == first);
  BOOST_CHECK(strings[1] == second);

  server->stop();
}

// Frames that arrive back-to-back must be read one at a time. A client may send
// its next request before the previous response is back -- a oneway call
// followed by another call does exactly that -- so the socket can hold more than
// the frame being read. The read buffer grows in doublings and can be larger
// than the frame (a fresh buffer rounds up, and a buffer kept from an earlier
// request may already exceed it), so each read has to stop at the end of the
// frame rather than the end of the buffer, or it takes bytes of the next
// request. Both requests go out in a single write, so they are guaranteed to be
// waiting on the socket together.
BOOST_FIXTURE_TEST_CASE(back_to_back_frames_are_read_one_at_a_time, Fixture) {
  startServer(0);
  int port = server->getListenPort();
  BOOST_REQUIRE_GT(port, 0);

  // Serialize two complete framed requests up front; a client would wait for
  // each response before sending the next call.
  auto requests = make_shared<transport::TMemoryBuffer>();
  test::ParentServiceClient writer(
      make_shared<protocol::TBinaryProtocol>(make_shared<transport::TFramedTransport>(requests)));
  writer.send_addString("pipelined");
  const size_t firstFrameSize = requests->getBufferAsString().size();
  writer.send_getStrings();
  const std::string bytes = requests->getBufferAsString();
  // A first frame that exactly fills a power-of-two buffer leaves no room to
  // read past it, and this test would pass without exercising the bound.
  BOOST_REQUIRE_NE(firstFrameSize & (firstFrameSize - 1), 0u);

  auto socket = make_shared<transport::TSocket>("localhost", port);
  socket->setRecvTimeout(5000);
  socket->open();
  socket->write(reinterpret_cast<const uint8_t*>(bytes.data()),
                static_cast<uint32_t>(bytes.size()));

  test::ParentServiceClient reader(
      make_shared<protocol::TBinaryProtocol>(make_shared<transport::TFramedTransport>(socket)));
  reader.recv_addString();
  std::vector<std::string> strings;
  reader.recv_getStrings(strings);
  BOOST_REQUIRE_EQUAL(strings.size(), 1u);
  BOOST_CHECK_EQUAL(strings[0], "pipelined");

  server->stop();
}

#if defined(__linux__)
// The declared frame size is a number the peer chooses; the read buffer must be
// grown as the payload arrives, not reserved in full when the four-byte header
// is read. Here the peer announces a 256 MiB frame and sends none of it, so a
// server that grows with the payload reserves next to nothing, while one that
// reserves on the header commits the whole frame. The reservation is address
// space (std::realloc; committed, not resident), so this reads VmSize from
// /proc, which is Linux-only. It runs in a child because the unfixed path
// reserves hundreds of MiB that would otherwise perturb the rest of the suite.
static long readVmSizeKB() {
  std::ifstream status("/proc/self/status");
  std::string key;
  while (status >> key) {
    if (key == "VmSize:") {
      long kb = -1;
      status >> kb;
      return kb;
    }
    std::getline(status, key);
  }
  return -1;
}

BOOST_AUTO_TEST_CASE(read_buffer_grows_with_the_payload_not_the_header) {
  pid_t pid = fork();
  BOOST_REQUIRE_NE(pid, -1);

  if (pid == 0) {
    auto socket = make_shared<transport::TNonblockingServerSocket>(0);
    auto processor = make_shared<test::ParentServiceProcessor>(make_shared<Handler>());
    auto child = make_shared<server::TNonblockingServer>(processor, socket);
    // Well above the default so the large header below clears the frame-size
    // check and reaches the buffer-growth path.
    child->setMaxFrameSize(512UL * 1024 * 1024);

    auto factory = make_shared<ThreadFactory>(false);
    struct Serve : public Runnable {
      shared_ptr<server::TNonblockingServer> server;
      void run() override { server->serve(); }
    };
    auto runnable = make_shared<Serve>();
    runnable->server = child;
    auto serveThread = factory->newThread(runnable);
    serveThread->start();

    for (int i = 0; i < 100 && child->getListenPort() == 0; ++i) {
      THRIFT_SLEEP_USEC(10000);
    }

    try {
      transport::TSocket client("localhost", child->getListenPort());
      client.open();
      long before = readVmSizeKB();
      uint32_t declared = 256UL * 1024 * 1024;
      uint8_t header[4] = {static_cast<uint8_t>(declared >> 24),
                           static_cast<uint8_t>(declared >> 16),
                           static_cast<uint8_t>(declared >> 8),
                           static_cast<uint8_t>(declared)};
      client.write(header, sizeof(header));
      client.flush();
      // Give the I/O thread time to read the header and act on it.
      THRIFT_SLEEP_USEC(1500000);
      long after = readVmSizeKB();
      client.close();
      child->stop();
      serveThread->join();

      // The server must not have reserved the declared frame. The unfixed path
      // grows by ~256-512 MiB; a payload-driven one by essentially nothing.
      // 64 MiB of slack covers unrelated allocations without admitting the
      // wrong behaviour.
      long grewKB = (before < 0 || after < 0) ? -1 : after - before;
      _exit((grewKB >= 0 && grewKB < 64L * 1024) ? 0 : 1);
    } catch (...) {
      _exit(3);
    }
  }

  int status = 0;
  BOOST_REQUIRE_NE(waitpid(pid, &status, 0), -1);
  BOOST_REQUIRE(WIFEXITED(status));
  BOOST_CHECK_EQUAL(WEXITSTATUS(status), 0);
}
#endif

BOOST_AUTO_TEST_SUITE_END()
