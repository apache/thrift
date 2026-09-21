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

// TEvhttpServer allocates one RequestContext per request and hands ownership of
// it to the completion callback that TAsyncBufferProcessor::process() is
// expected to invoke. A processor may instead throw straight back out of
// process() -- the generated TAsyncDispatchProcessor reads the message header
// (readMessageBegin) before it dispatches, outside any try/catch, so a request
// body too short to hold a header throws END_OF_FILE before any callback runs.
// This test drives exactly that path and checks that the request context is
// released on it, as well as on the ordinary success path.
//
// The check needs no sanitizer. process() is handed the very output buffer that
// the request context owns; the context is that buffer's only owner. The test
// keeps a weak_ptr to each output buffer it is handed, so after the run a
// buffer is still alive precisely when its request context was not released.

#define BOOST_TEST_MODULE TEvhttpServerRequestContextTest
#include <boost/test/unit_test.hpp>

#include <atomic>
#include <cstdint>
#include <functional>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include <event2/event.h>
#include <evhttp.h>

#include <thrift/async/TAsyncBufferProcessor.h>
#include <thrift/async/TEvhttpServer.h>
#include <thrift/concurrency/Thread.h>
#include <thrift/concurrency/ThreadFactory.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/PlatformSocket.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportException.h>

using apache::thrift::async::TAsyncBufferProcessor;
using apache::thrift::async::TEvhttpServer;
using apache::thrift::concurrency::Runnable;
using apache::thrift::concurrency::Thread;
using apache::thrift::concurrency::ThreadFactory;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TMessageType;
using apache::thrift::transport::TBufferBase;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TServerSocket;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::TException;
using std::shared_ptr;
using std::string;

// A processor with the synchronous shape of a generated dispatcher: it reads
// the message header immediately. A body too short to parse throws before the
// completion callback is invoked; a well-formed header reaches the callback. It
// records a weak_ptr to every output buffer it is handed so the test can tell,
// afterwards, which request contexts were released.
class RecordingBufferProcessor : public TAsyncBufferProcessor {
public:
  void process(std::function<void(bool)> _return,
               shared_ptr<TBufferBase> ibuf,
               shared_ptr<TBufferBase> obuf) override {
    // Record before doing anything that can throw.
    observed_.push_back(obuf);

    // The generated dispatcher reads the header here, outside any try/catch;
    // a body shorter than a message header throws END_OF_FILE straight out.
    TBinaryProtocol prot(std::static_pointer_cast<TTransport>(ibuf));
    string name;
    TMessageType type;
    int32_t seqid = 0;
    prot.readMessageBegin(name, type, seqid);

    // Reached only for a well-formed header. Put a byte in the reply buffer so
    // complete() has something to send, then hand ownership to the callback.
    uint8_t byte = 0;
    obuf->write(&byte, 1);
    _return(true);
  }

  size_t observedCount() const { return observed_.size(); }

  size_t aliveCount() const {
    size_t alive = 0;
    for (size_t i = 0; i < observed_.size(); ++i) {
      if (!observed_[i].expired()) {
        ++alive;
      }
    }
    return alive;
  }

private:
  // Only ever touched from the thread that pumps the event loop.
  std::vector<std::weak_ptr<TBufferBase> > observed_;
};

// Bind a probe socket to port 0, read the assigned port and release it. The
// caller retries the actual bind, which covers the small window in between.
static int findFreePort() {
  TServerSocket probe(0);
  probe.listen();
  int port = probe.getPort();
  probe.close();
  return port;
}

// A TEvhttpServer bound to a free loopback port, retrying if the port was taken
// between probing and binding.
static shared_ptr<TEvhttpServer> makeServer(const shared_ptr<TAsyncBufferProcessor>& processor,
                                            int& outPort) {
  for (int attempt = 0; attempt < 25; ++attempt) {
    int port = findFreePort();
    try {
      shared_ptr<TEvhttpServer> server(new TEvhttpServer(processor, port));
      outPort = port;
      return server;
    } catch (const TException&) {
      // Port taken in the meantime; try another.
    }
  }
  return shared_ptr<TEvhttpServer>();
}

static string httpPost(const string& body) {
  std::ostringstream os;
  os << "POST / HTTP/1.1\r\n"
     << "Host: 127.0.0.1\r\n"
     << "Content-Type: application/x-thrift\r\n"
     << "Content-Length: " << body.size() << "\r\n"
     << "Connection: close\r\n"
     << "\r\n"
     << body;
  return os.str();
}

// A blocking HTTP client run on its own thread. It sends one POST per request,
// reads the status line back, and records the three-digit status (or -1 if no
// well-formed reply arrived). The event loop is pumped on the main thread while
// this runs, so the blocking reads here always make progress.
struct ClientRunnable : public Runnable {
  int port;
  std::vector<string> requests;
  std::vector<int> statuses;
  std::atomic<bool>* done;

  void run() override {
    for (size_t i = 0; i < requests.size(); ++i) {
      statuses.push_back(sendOne(requests[i]));
    }
    done->store(true);
  }

  int sendOne(const string& request) {
    TSocket socket("127.0.0.1", port);
    socket.setConnTimeout(5000);
    socket.setRecvTimeout(5000);
    try {
      socket.open();
      socket.write(reinterpret_cast<const uint8_t*>(request.data()),
                   static_cast<uint32_t>(request.size()));
      socket.flush();

      string reply;
      uint8_t buf[256];
      while (reply.find("\r\n") == string::npos) {
        uint32_t got = socket.read(buf, sizeof(buf));
        if (got == 0) {
          break;
        }
        reply.append(reinterpret_cast<char*>(buf), got);
      }
      socket.close();

      // Status line: "HTTP/1.1 <code> <reason>".
      if (reply.compare(0, 5, "HTTP/") != 0) {
        return -1;
      }
      string::size_type sp = reply.find(' ');
      if (sp == string::npos) {
        return -1;
      }
      return std::atoi(reply.c_str() + sp + 1);
    } catch (const TTransportException&) {
      return -1;
    }
  }
};

// Runs the client on a thread while pumping the server's event loop on this
// thread until the client is finished, then reports what the client saw and how
// many request contexts were left unreleased.
struct ScenarioResult {
  std::vector<int> statuses;
  size_t observed;
  size_t alive;
};

static ScenarioResult runScenario(const std::vector<string>& requests) {
  shared_ptr<RecordingBufferProcessor> processor(new RecordingBufferProcessor());
  int port = 0;
  shared_ptr<TEvhttpServer> server = makeServer(processor, port);
  BOOST_REQUIRE(server);
  struct event_base* base = server->getEventBase();

  std::atomic<bool> done(false);
  shared_ptr<ClientRunnable> client(new ClientRunnable());
  client->port = port;
  client->requests = requests;
  client->done = &done;

  shared_ptr<ThreadFactory> factory(new ThreadFactory(false));
  shared_ptr<Thread> thread = factory->newThread(client);
  thread->start();

  // Pump the server while the client is working. The callbacks -- and therefore
  // every RequestContext allocation and release -- run on this thread.
  while (!done.load()) {
    event_base_loop(base, EVLOOP_NONBLOCK);
    THRIFT_SLEEP_USEC(200);
  }
  // Drain any events still pending after the last reply (e.g. connection close).
  for (int i = 0; i < 500; ++i) {
    event_base_loop(base, EVLOOP_NONBLOCK);
    THRIFT_SLEEP_USEC(200);
  }
  thread->join();

  ScenarioResult result;
  result.statuses = client->statuses;
  result.observed = processor->observedCount();
  result.alive = processor->aliveCount();
  return result;
}

// A message body too short to parse a header: process() throws before invoking
// the completion callback. Every such request must still free its context, and
// the server must answer each one and stay up.
BOOST_AUTO_TEST_CASE(context_released_when_processor_throws) {
  const size_t N = 32;
  std::vector<string> requests(N, httpPost("AB")); // 2 bytes: shorter than a header

  ScenarioResult result = runScenario(requests);

  // The path was actually exercised N times (guards against a silent 404 or a
  // request that never reached process(), which would make "0 unreleased"
  // vacuously true).
  BOOST_REQUIRE_EQUAL(result.observed, N);

  // The server answered every request and stayed up.
  BOOST_REQUIRE_EQUAL(result.statuses.size(), N);
  for (size_t i = 0; i < result.statuses.size(); ++i) {
    BOOST_CHECK_EQUAL(result.statuses[i], 500);
  }

  // No request context outlived its request.
  BOOST_CHECK_EQUAL(result.alive, 0u);
}

// The ordinary path still frees the context exactly once: the completion
// callback runs, the server answers 200, and nothing is left over. Under a
// sanitizer this also guards against a double free.
BOOST_AUTO_TEST_CASE(context_released_on_success) {
  shared_ptr<TMemoryBuffer> mb(new TMemoryBuffer());
  TBinaryProtocol writer(mb);
  writer.writeMessageBegin("ping", apache::thrift::protocol::T_CALL, 1);
  writer.writeMessageEnd();
  string body = mb->getBufferAsString();

  std::vector<string> requests(1, httpPost(body));

  ScenarioResult result = runScenario(requests);

  BOOST_REQUIRE_EQUAL(result.observed, 1u);
  BOOST_REQUIRE_EQUAL(result.statuses.size(), 1u);
  BOOST_CHECK_EQUAL(result.statuses[0], 200);
  BOOST_CHECK_EQUAL(result.alive, 0u);
}
