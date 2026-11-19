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

// TEvhttpClientChannel against a scripted loopback HTTP server: what the
// caller's receive buffer holds when a request completes.

#define BOOST_TEST_MODULE TEvhttpClientChannelTest
#include <boost/test/unit_test.hpp>

#include <atomic>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <memory>
#include <string>
#include <thread>
#include <vector>

#include <event2/event.h>

#include <thrift/async/TEvhttpClientChannel.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TTransportException.h>

using apache::thrift::async::TEvhttpClientChannel;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TServerSocket;
using apache::thrift::transport::TTransport;
using std::shared_ptr;
using std::string;
using std::vector;

namespace {

string okReply(const string& body) {
  return "HTTP/1.1 200 OK\r\nContent-Type: application/x-thrift\r\nContent-Length: "
         + std::to_string(body.size()) + "\r\n\r\n" + body;
}

const string serverError = "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n";

// In a server script: close the connection instead of answering.
const string closeConnection;

// The value of the Content-Length header in an HTTP request header, or 0.
size_t contentLength(const string& header) {
  string lower(header);
  for (char& c : lower) {
    c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
  }
  const string name = "\r\ncontent-length:";
  const string::size_type pos = lower.find(name);
  if (pos == string::npos) {
    return 0;
  }
  return static_cast<size_t>(std::strtoul(header.c_str() + pos + name.size(), nullptr, 10));
}

bool readMore(TTransport& connection, string& pending) {
  uint8_t buf[4096];
  const uint32_t got = connection.read(buf, sizeof(buf));
  if (got == 0) {
    return false;
  }
  pending.append(reinterpret_cast<const char*>(buf), got);
  return true;
}

// Reads one HTTP request from the connection; bytes after it stay in pending.
bool readRequest(TTransport& connection, string& pending) {
  string::size_type headerEnd;
  while ((headerEnd = pending.find("\r\n\r\n")) == string::npos) {
    if (!readMore(connection, pending)) {
      return false;
    }
  }
  const size_t length = headerEnd + 4 + contentLength(pending.substr(0, headerEnd));
  while (pending.size() < length) {
    if (!readMore(connection, pending)) {
      return false;
    }
  }
  pending.erase(0, length);
  return true;
}

// A loopback HTTP server on its own thread. It accepts one connection, answers
// the requests on it in the order of its script, and then waits for the client
// to close the connection.
class ScriptedServer {
public:
  explicit ScriptedServer(const vector<string>& script)
    : listener_(new TServerSocket("127.0.0.1", 0)), script_(script), requestsRead_(0) {
    listener_->setAcceptTimeout(10000);
    listener_->setRecvTimeout(10000);
    listener_->listen();
    thread_ = std::thread([this]() { serve(); });
  }

  ~ScriptedServer() {
    thread_.join();
    listener_->close();
  }

  int port() const { return listener_->getPort(); }

  int requestsRead() const { return requestsRead_.load(); }

private:
  void serve() {
    try {
      shared_ptr<TTransport> connection = listener_->accept();
      string pending;
      for (const string& reply : script_) {
        if (!readRequest(*connection, pending)) {
          return;
        }
        ++requestsRead_;
        if (reply.empty()) {
          connection->close();
          return;
        }
        connection->write(reinterpret_cast<const uint8_t*>(reply.data()),
                          static_cast<uint32_t>(reply.size()));
        connection->flush();
      }
      while (readMore(*connection, pending)) {
      }
    } catch (const std::exception&) {
      // A timeout ends the script; the test checks what the client saw.
    }
  }

  shared_ptr<TServerSocket> listener_;
  vector<string> script_;
  std::atomic<int> requestsRead_;
  std::thread thread_;
};

event_base* newEventBase() {
  event_base* base = event_base_new();
  BOOST_REQUIRE(base != nullptr);
  return base;
}

// A channel on its own event base, used the way a cob client uses it.
class Client {
public:
  explicit Client(int port)
    : base_(newEventBase(), &event_base_free),
      channel_(new TEvhttpClientChannel("127.0.0.1", "/", "127.0.0.1", port, base_.get())) {}

  // Sends one request and runs the event loop until the channel has completed
  // it; onComplete runs in the channel's completion callback. Returns whether
  // the request completed in time.
  bool call(TMemoryBuffer& reply, const std::function<void()>& onComplete) {
    TMemoryBuffer request;
    const uint8_t byte = 0;
    request.write(&byte, 1);

    bool completed = false;
    event_base* base = base_.get();
    channel_->sendAndRecvMessage(
        [&]() {
          onComplete();
          completed = true;
          event_base_loopbreak(base);
        },
        &request,
        &reply);

    struct timeval limit = {10, 0};
    event_base_loopexit(base, &limit);
    event_base_dispatch(base);
    return completed;
  }

private:
  std::unique_ptr<event_base, void (*)(event_base*)> base_;
  std::unique_ptr<TEvhttpClientChannel> channel_;
};

// A reply larger than a single network read, so that it arrives in many pieces.
string largeBody() {
  string body(1 << 20, ' ');
  for (size_t i = 0; i < body.size(); ++i) {
    body[i] = static_cast<char>('a' + i % 26);
  }
  return body;
}

// The caller reads the start of a reply and leaves the rest unread, as a reader
// does when a reply holds more than the one message it expects. The next
// request then fails.
void checkFailedRequestAfterPartlyReadReply(const string& failure) {
  const string head = "head";
  const string rest = "rest of the reply";
  ScriptedServer server(vector<string>{okReply(head + rest), failure});
  Client client(server.port());

  TMemoryBuffer reply;
  uint32_t unreadAfterReply = 0;
  BOOST_REQUIRE(client.call(reply, [&]() {
    uint8_t buf[4];
    reply.readAll(buf, sizeof(buf));
    unreadAfterReply = reply.available_read();
  }));
  uint32_t unreadAfterFailure = 1;
  BOOST_REQUIRE(client.call(reply, [&]() { unreadAfterFailure = reply.available_read(); }));

  BOOST_CHECK_EQUAL(server.requestsRead(), 2);
  BOOST_CHECK_EQUAL(unreadAfterReply, rest.size());
  BOOST_CHECK_EQUAL(unreadAfterFailure, 0u);
}

} // namespace

// A request that fails with an error status completes with an empty buffer, so
// the caller's reader sees the end of the input.
BOOST_AUTO_TEST_CASE(error_status_completes_with_empty_buffer) {
  checkFailedRequestAfterPartlyReadReply(serverError);
}

// The same for a connection that closes before the reply.
BOOST_AUTO_TEST_CASE(closed_connection_completes_with_empty_buffer) {
  checkFailedRequestAfterPartlyReadReply(closeConnection);
}

// The reply belongs to the caller's buffer: the caller can read it after the
// completion callback as well as in it.
BOOST_AUTO_TEST_CASE(reply_can_be_read_after_the_callback) {
  const string body = largeBody();
  ScriptedServer server(vector<string>{okReply(body)});
  Client client(server.port());

  TMemoryBuffer reply;
  BOOST_REQUIRE(client.call(reply, []() {}));

  BOOST_REQUIRE_EQUAL(reply.available_read(), body.size());
  BOOST_CHECK(reply.readAsString(static_cast<uint32_t>(body.size())) == body);
}
