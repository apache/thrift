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

#include <thrift/async/TAsyncBufferProcessor.h>
#include <thrift/async/TAsyncProtocolProcessor.h>
#include <thrift/async/TEvhttpServer.h>
#include <thrift/concurrency/ThreadFactory.h>
#include <thrift/concurrency/ThreadManager.h>
#include <thrift/processor/TMultiplexedProcessor.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/protocol/TCompactProtocol.h>
#include <thrift/protocol/THeaderProtocol.h>
#include <thrift/protocol/TJSONProtocol.h>
#include <thrift/server/TNonblockingServer.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/server/TThreadPoolServer.h>
#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/THttpServer.h>
#include <thrift/transport/THttpTransport.h>
#include <thrift/transport/TNonblockingSSLServerSocket.h>
#include <thrift/transport/TNonblockingServerSocket.h>
#include <thrift/transport/TSSLServerSocket.h>
#include <thrift/transport/TSSLSocket.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TTransportUtils.h>
#include <thrift/transport/TZlibTransport.h>

#include "SecondService.h"
#include "ThriftTest.h"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include <iostream>
#include <stdexcept>
#include <sstream>

#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#if _WIN32
#include <thrift/windows/TWinsockSingleton.h>
#endif

using namespace std;

using namespace apache::thrift;
using namespace apache::thrift::async;
using namespace apache::thrift::concurrency;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::server;

using namespace thrift::test;

// to handle a controlled shutdown, signal handling is mandatory
#ifdef HAVE_SIGNAL_H
apache::thrift::concurrency::Monitor gMonitor;
void signal_handler(int signum)
{
  if (signum == SIGINT) {
    gMonitor.notifyAll();
  }
}
#endif

class TestHandler : public ThriftTestIf {
public:
  TestHandler() = default;

  void testVoid() override { printf("testVoid()\n"); }

  void testString(string& out, const string& thing) override {
    printf("testString(\"%s\")\n", thing.c_str());
    out = thing;
  }

  bool testBool(const bool thing) override {
    printf("testBool(%s)\n", thing ? "true" : "false");
    return thing;
  }

  int8_t testByte(const int8_t thing) override {
    printf("testByte(%d)\n", (int)thing);
    return thing;
  }

  int32_t testI32(const int32_t thing) override {
    printf("testI32(%d)\n", thing);
    return thing;
  }

  int64_t testI64(const int64_t thing) override {
    printf("testI64(%" PRId64 ")\n", thing);
    return thing;
  }

  double testDouble(const double thing) override {
    printf("testDouble(%f)\n", thing);
    return thing;
  }

  void testBinary(std::string& _return, const std::string& thing) override {
    std::ostringstream hexstr;
    hexstr << std::hex << thing;
    printf("testBinary(%lu: %s)\n", safe_numeric_cast<unsigned long>(thing.size()), hexstr.str().c_str());
    _return = thing;
  }

  void testStruct(Xtruct& out, const Xtruct& thing) override {
    printf("testStruct({\"%s\", %d, %d, %" PRId64 "})\n",
           thing.string_thing.c_str(),
           (int)thing.byte_thing,
           thing.i32_thing,
           thing.i64_thing);
    out = thing;
  }

  void testNest(Xtruct2& out, const Xtruct2& nest) override {
    const Xtruct& thing = nest.struct_thing;
    printf("testNest({%d, {\"%s\", %d, %d, %" PRId64 "}, %d})\n",
           (int)nest.byte_thing,
           thing.string_thing.c_str(),
           (int)thing.byte_thing,
           thing.i32_thing,
           thing.i64_thing,
           nest.i32_thing);
    out = nest;
  }

  void testMap(map<int32_t, int32_t>& out, const map<int32_t, int32_t>& thing) override {
    printf("testMap({");
    map<int32_t, int32_t>::const_iterator m_iter;
    bool first = true;
    for (m_iter = thing.begin(); m_iter != thing.end(); ++m_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d => %d", m_iter->first, m_iter->second);
    }
    printf("})\n");
    out = thing;
  }

  void testStringMap(map<std::string, std::string>& out,
                     const map<std::string, std::string>& thing) override {
    printf("testMap({");
    map<std::string, std::string>::const_iterator m_iter;
    bool first = true;
    for (m_iter = thing.begin(); m_iter != thing.end(); ++m_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%s => %s", (m_iter->first).c_str(), (m_iter->second).c_str());
    }
    printf("})\n");
    out = thing;
  }

  void testSet(set<int32_t>& out, const set<int32_t>& thing) override {
    printf("testSet({");
    set<int32_t>::const_iterator s_iter;
    bool first = true;
    for (s_iter = thing.begin(); s_iter != thing.end(); ++s_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d", *s_iter);
    }
    printf("})\n");
    out = thing;
  }

  void testList(vector<int32_t>& out, const vector<int32_t>& thing) override {
    printf("testList({");
    vector<int32_t>::const_iterator l_iter;
    bool first = true;
    for (l_iter = thing.begin(); l_iter != thing.end(); ++l_iter) {
      if (first) {
        first = false;
      } else {
        printf(", ");
      }
      printf("%d", *l_iter);
    }
    printf("})\n");
    out = thing;
  }

  Numberz::type testEnum(const Numberz::type thing) override {
    printf("testEnum(%d)\n", thing);
    return thing;
  }

  UserId testTypedef(const UserId thing) override {
    printf("testTypedef(%" PRId64 ")\n", thing);
    return thing;
  }

  void testMapMap(map<int32_t, map<int32_t, int32_t> >& mapmap, const int32_t hello) override {
    printf("testMapMap(%d)\n", hello);

    map<int32_t, int32_t> pos;
    map<int32_t, int32_t> neg;
    for (int i = 1; i < 5; i++) {
      pos.insert(make_pair(i, i));
      neg.insert(make_pair(-i, -i));
    }

    mapmap.insert(make_pair(4, pos));
    mapmap.insert(make_pair(-4, neg));
  }

  void testInsanity(map<UserId, map<Numberz::type, Insanity> >& insane, const Insanity& argument) override {
    printf("testInsanity()\n");

    Insanity looney;
    map<Numberz::type, Insanity> first_map;
    map<Numberz::type, Insanity> second_map;

    first_map.insert(make_pair(Numberz::TWO, argument));
    first_map.insert(make_pair(Numberz::THREE, argument));

    second_map.insert(make_pair(Numberz::SIX, looney));

    insane.insert(make_pair(1, first_map));
    insane.insert(make_pair(2, second_map));

    printf("return");
    printf(" = {");
    map<UserId, map<Numberz::type, Insanity> >::const_iterator i_iter;
    for (i_iter = insane.begin(); i_iter != insane.end(); ++i_iter) {
      printf("%" PRId64 " => {", i_iter->first);
      map<Numberz::type, Insanity>::const_iterator i2_iter;
      for (i2_iter = i_iter->second.begin(); i2_iter != i_iter->second.end(); ++i2_iter) {
        printf("%d => {", i2_iter->first);
        map<Numberz::type, UserId> userMap = i2_iter->second.userMap;
        map<Numberz::type, UserId>::const_iterator um;
        printf("{");
        for (um = userMap.begin(); um != userMap.end(); ++um) {
          printf("%d => %" PRId64 ", ", um->first, um->second);
        }
        printf("}, ");

        vector<Xtruct> xtructs = i2_iter->second.xtructs;
        vector<Xtruct>::const_iterator x;
        printf("{");
        for (x = xtructs.begin(); x != xtructs.end(); ++x) {
          printf("{\"%s\", %d, %d, %" PRId64 "}, ",
                 x->string_thing.c_str(),
                 (int)x->byte_thing,
                 x->i32_thing,
                 x->i64_thing);
        }
        printf("}");

        printf("}, ");
      }
      printf("}, ");
    }
    printf("}\n");
  }

  void testMulti(Xtruct& hello,
                 const int8_t arg0,
                 const int32_t arg1,
                 const int64_t arg2,
                 const std::map<int16_t, std::string>& arg3,
                 const Numberz::type arg4,
                 const UserId arg5) override {
    (void)arg3;
    (void)arg4;
    (void)arg5;

    printf("testMulti()\n");

    hello.string_thing = "Hello2";
    hello.byte_thing = arg0;
    hello.i32_thing = arg1;
    hello.i64_thing = (int64_t)arg2;
  }

  void testException(const std::string& arg) override {
    printf("testException(%s)\n", arg.c_str());
    if (arg.compare("Xception") == 0) {
      Xception e;
      e.errorCode = 1001;
      e.message = arg;
      throw e;
    } else if (arg.compare("TException") == 0) {
      apache::thrift::TException e;
      throw e;
    } else {
      Xtruct result;
      result.string_thing = arg;
      return;
    }
  }

  void testMultiException(Xtruct& result,
                          const std::string& arg0,
                          const std::string& arg1) override {

    printf("testMultiException(%s, %s)\n", arg0.c_str(), arg1.c_str());

    if (arg0.compare("Xception") == 0) {
      Xception e;
      e.errorCode = 1001;
      e.message = "This is an Xception";
      throw e;
    } else if (arg0.compare("Xception2") == 0) {
      Xception2 e;
      e.errorCode = 2002;
      e.struct_thing.string_thing = "This is an Xception2";
      throw e;
    } else {
      result.string_thing = arg1;
      return;
    }
  }

  void testOneway(const int32_t aNum) override {
    printf("testOneway(%d): call received\n", aNum);
  }
};

class SecondHandler : public SecondServiceIf
{
  public:
    void secondtestString(std::string& result, const std::string& thing) override
    { result = "testString(\"" + thing + "\")"; }
};

class TestProcessorEventHandler : public TProcessorEventHandler {
  void* getContext(const char* fn_name, void* serverContext) override {
    (void)serverContext;
    return new std::string(fn_name);
  }
  void freeContext(void* ctx, const char* fn_name) override {
    (void)fn_name;
    delete static_cast<std::string*>(ctx);
  }
  void preRead(void* ctx, const char* fn_name) override { communicate("preRead", ctx, fn_name); }
  void postRead(void* ctx, const char* fn_name, uint32_t bytes) override {
    (void)bytes;
    communicate("postRead", ctx, fn_name);
  }
  void preWrite(void* ctx, const char* fn_name) override { communicate("preWrite", ctx, fn_name); }
  void postWrite(void* ctx, const char* fn_name, uint32_t bytes) override {
    (void)bytes;
    communicate("postWrite", ctx, fn_name);
  }
  void asyncComplete(void* ctx, const char* fn_name) override {
    communicate("asyncComplete", ctx, fn_name);
  }
  void handlerError(void* ctx, const char* fn_name) override {
    communicate("handlerError", ctx, fn_name);
  }

  void communicate(const char* event, void* ctx, const char* fn_name) {
    std::cout << event << ": " << *static_cast<std::string*>(ctx) << " = " << fn_name << std::endl;
  }
};

class TestHandlerAsync : public ThriftTestCobSvIf {
public:
  TestHandlerAsync(std::shared_ptr<TestHandler>& handler) : _delegate(handler) {}
  ~TestHandlerAsync() override = default;

  void testVoid(std::function<void()> cob) override {
    _delegate->testVoid();
    cob();
  }

  void testString(std::function<void(std::string const& _return)> cob,
                          const std::string& thing) override {
    std::string res;
    _delegate->testString(res, thing);
    cob(res);
  }

  void testBool(std::function<void(bool const& _return)> cob, const bool thing) override {
    bool res = _delegate->testBool(thing);
    cob(res);
  }

  void testByte(std::function<void(int8_t const& _return)> cob, const int8_t thing) override {
    int8_t res = _delegate->testByte(thing);
    cob(res);
  }

  void testI32(std::function<void(int32_t const& _return)> cob, const int32_t thing) override {
    int32_t res = _delegate->testI32(thing);
    cob(res);
  }

  void testI64(std::function<void(int64_t const& _return)> cob, const int64_t thing) override {
    int64_t res = _delegate->testI64(thing);
    cob(res);
  }

  void testDouble(std::function<void(double const& _return)> cob, const double thing) override {
    double res = _delegate->testDouble(thing);
    cob(res);
  }

  void testBinary(std::function<void(std::string const& _return)> cob,
                          const std::string& thing) override {
    std::string res;
    _delegate->testBinary(res, thing);
    cob(res);
  }

  void testStruct(std::function<void(Xtruct const& _return)> cob, const Xtruct& thing) override {
    Xtruct res;
    _delegate->testStruct(res, thing);
    cob(res);
  }

  void testNest(std::function<void(Xtruct2 const& _return)> cob, const Xtruct2& thing) override {
    Xtruct2 res;
    _delegate->testNest(res, thing);
    cob(res);
  }

  void testMap(std::function<void(std::map<int32_t, int32_t> const& _return)> cob,
                       const std::map<int32_t, int32_t>& thing) override {
    std::map<int32_t, int32_t> res;
    _delegate->testMap(res, thing);
    cob(res);
  }

  void testStringMap(
      std::function<void(std::map<std::string, std::string> const& _return)> cob,
      const std::map<std::string, std::string>& thing) override {
    std::map<std::string, std::string> res;
    _delegate->testStringMap(res, thing);
    cob(res);
  }

  void testSet(std::function<void(std::set<int32_t> const& _return)> cob,
                       const std::set<int32_t>& thing) override {
    std::set<int32_t> res;
    _delegate->testSet(res, thing);
    cob(res);
  }

  void testList(std::function<void(std::vector<int32_t> const& _return)> cob,
                        const std::vector<int32_t>& thing) override {
    std::vector<int32_t> res;
    _delegate->testList(res, thing);
    cob(res);
  }

  void testEnum(std::function<void(Numberz::type const& _return)> cob,
                        const Numberz::type thing) override {
    Numberz::type res = _delegate->testEnum(thing);
    cob(res);
  }

  void testTypedef(std::function<void(UserId const& _return)> cob, const UserId thing) override {
    UserId res = _delegate->testTypedef(thing);
    cob(res);
  }

  void testMapMap(
      std::function<void(std::map<int32_t, std::map<int32_t, int32_t> > const& _return)> cob,
      const int32_t hello) override {
    std::map<int32_t, std::map<int32_t, int32_t> > res;
    _delegate->testMapMap(res, hello);
    cob(res);
  }

  void testInsanity(
      std::function<void(std::map<UserId, std::map<Numberz::type, Insanity> > const& _return)> cob,
      const Insanity& argument) override {
    std::map<UserId, std::map<Numberz::type, Insanity> > res;
    _delegate->testInsanity(res, argument);
    cob(res);
  }

  void testMulti(std::function<void(Xtruct const& _return)> cob,
                         const int8_t arg0,
                         const int32_t arg1,
                         const int64_t arg2,
                         const std::map<int16_t, std::string>& arg3,
                         const Numberz::type arg4,
                         const UserId arg5) override {
    Xtruct res;
    _delegate->testMulti(res, arg0, arg1, arg2, arg3, arg4, arg5);
    cob(res);
  }

  void testException(
      std::function<void()> cob,
      std::function<void(::apache::thrift::TDelayedException* _throw)> exn_cob,
      const std::string& arg) override {
    try {
      _delegate->testException(arg);
    } catch (const apache::thrift::TException& e) {
      exn_cob(apache::thrift::TDelayedException::delayException(e));
      return;
    }
    cob();
  }

  void testMultiException(
      std::function<void(Xtruct const& _return)> cob,
      std::function<void(::apache::thrift::TDelayedException* _throw)> exn_cob,
      const std::string& arg0,
      const std::string& arg1) override {
    Xtruct res;
    try {
      _delegate->testMultiException(res, arg0, arg1);
    } catch (const apache::thrift::TException& e) {
      exn_cob(apache::thrift::TDelayedException::delayException(e));
      return;
    }
    cob(res);
  }

  void testOneway(std::function<void()> cob, const int32_t secondsToSleep) override {
    _delegate->testOneway(secondsToSleep);
    cob();
  }

protected:
  std::shared_ptr<TestHandler> _delegate;
};

namespace po = boost::program_options;

int main(int argc, char** argv) {

  string testDir = boost::filesystem::system_complete(argv[0]).parent_path().parent_path().parent_path().string();
  string certPath = testDir + "/keys/server.crt";
  string keyPath = testDir + "/keys/server.key";

#if _WIN32
  transport::TWinsockSingleton::create();
#endif
  int port = 9090;
  bool ssl = false;
  bool zlib = false;
  string transport_type = "buffered";
  string protocol_type = "binary";
  string server_type = "simple";
  string domain_socket = "";
  bool abstract_namespace = false;
  size_t workers = 4;
  int string_limit = 0;
  int container_limit = 0;

  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
    ("port", po::value<int>(&port)->default_value(port), "Port number to listen")
    ("domain-socket", po::value<string>(&domain_socket) ->default_value(domain_socket), "Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)")
    ("abstract-namespace", "Create the domain socket in the Abstract Namespace (no connection with filesystem pathnames)")
    ("server-type", po::value<string>(&server_type)->default_value(server_type), "type of server, \"simple\", \"thread-pool\", \"threaded\", or \"nonblocking\"")
    ("transport", po::value<string>(&transport_type)->default_value(transport_type), "transport: buffered, framed, http, zlib")
    ("protocol", po::value<string>(&protocol_type)->default_value(protocol_type), "protocol: binary, compact, header, json, multi, multic, multih, multij")
    ("ssl", "Encrypted Transport using SSL")
    ("zlib", "Wrapped Transport using Zlib")
    ("processor-events", "processor-events")
    ("workers,n", po::value<size_t>(&workers)->default_value(workers), "Number of thread pools workers. Only valid for thread-pool server type")
    ("string-limit", po::value<int>(&string_limit))
    ("container-limit", po::value<int>(&container_limit));

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help")) {
    cout << desc << "\n";
    return 1;
  }

  try {
    if (!server_type.empty()) {
      if (server_type == "simple") {
      } else if (server_type == "thread-pool") {
      } else if (server_type == "threaded") {
      } else if (server_type == "nonblocking") {
      } else {
        throw invalid_argument("Unknown server type " + server_type);
      }
    }

    if (!protocol_type.empty()) {
      if (protocol_type == "binary") {
      } else if (protocol_type == "compact") {
      } else if (protocol_type == "json") {
      } else if (protocol_type == "header") {
      } else if (protocol_type == "multi") {  // multiplexed binary
      } else if (protocol_type == "multic") { // multiplexed compact
      } else if (protocol_type == "multih") { // multiplexed header
      } else if (protocol_type == "multij") { // multiplexed json
      } else {
        throw invalid_argument("Unknown protocol type " + protocol_type);
      }
    }

    if (!transport_type.empty()) {
      if (transport_type == "buffered") {
      } else if (transport_type == "framed") {
      } else if (transport_type == "http") {
      } else if (transport_type == "zlib") {
        // crosstester will pass zlib as a flag and a transport right now...
      } else {
        throw invalid_argument("Unknown transport type " + transport_type);
      }
    }

  } catch (std::exception& e) {
    cerr << e.what() << endl;
    cout << desc << "\n";
    return 1;
  }

  if (vm.count("ssl")) {
    ssl = true;
  }

  if (vm.count("zlib")) {
    zlib = true;
  }

#if defined(HAVE_SIGNAL_H) && defined(SIGPIPE)
  if (ssl) {
    signal(SIGPIPE, SIG_IGN); // for OpenSSL, otherwise we end abruptly
  }
#endif

  if (vm.count("abstract-namespace")) {
    abstract_namespace = true;
  }

  // Dispatcher
  std::shared_ptr<TProtocolFactory> protocolFactory;
  if (protocol_type == "json" || protocol_type == "multij") {
    std::shared_ptr<TProtocolFactory> jsonProtocolFactory(new TJSONProtocolFactory());
    protocolFactory = jsonProtocolFactory;
  } else if (protocol_type == "compact" || protocol_type == "multic") {
    auto *compactProtocolFactory = new TCompactProtocolFactoryT<TBufferBase>();
    compactProtocolFactory->setContainerSizeLimit(container_limit);
    compactProtocolFactory->setStringSizeLimit(string_limit);
    protocolFactory.reset(compactProtocolFactory);
  } else if (protocol_type == "header" || protocol_type == "multih") {
    std::shared_ptr<TProtocolFactory> headerProtocolFactory(new THeaderProtocolFactory());
    protocolFactory = headerProtocolFactory;
  } else {
    auto* binaryProtocolFactory = new TBinaryProtocolFactoryT<TBufferBase>();
    binaryProtocolFactory->setContainerSizeLimit(container_limit);
    binaryProtocolFactory->setStringSizeLimit(string_limit);
    protocolFactory.reset(binaryProtocolFactory);
  }

  // Processors
  std::shared_ptr<TestHandler> testHandler(new TestHandler());
  std::shared_ptr<TProcessor> testProcessor(new ThriftTestProcessor(testHandler));

  if (vm.count("processor-events")) {
    testProcessor->setEventHandler(
        std::shared_ptr<TProcessorEventHandler>(new TestProcessorEventHandler()));
  }

  // Transport
  std::shared_ptr<TSSLSocketFactory> sslSocketFactory;
  std::shared_ptr<TServerSocket> serverSocket;

  if (ssl) {
    sslSocketFactory = std::shared_ptr<TSSLSocketFactory>(new TSSLSocketFactory());
    sslSocketFactory->loadCertificate(certPath.c_str());
    sslSocketFactory->loadPrivateKey(keyPath.c_str());
    sslSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");
    if (server_type != "nonblocking") {
      serverSocket = std::shared_ptr<TServerSocket>(new TSSLServerSocket(port, sslSocketFactory));
    }
  } else {
    if (domain_socket != "") {
      if (abstract_namespace) {
        std::string abstract_socket("\0", 1);
        abstract_socket += domain_socket;
        serverSocket = std::shared_ptr<TServerSocket>(new TServerSocket(abstract_socket));
      } else {
        unlink(domain_socket.c_str());
        serverSocket = std::shared_ptr<TServerSocket>(new TServerSocket(domain_socket));
      }
      port = 0;
    } else {
      serverSocket = std::shared_ptr<TServerSocket>(new TServerSocket(port));
    }
  }

  // Factory
  std::shared_ptr<TTransportFactory> transportFactory;

  if (transport_type == "http" && server_type != "nonblocking") {
    transportFactory = std::make_shared<THttpServerTransportFactory>();
  } else if (transport_type == "framed") {
    transportFactory = std::make_shared<TFramedTransportFactory>();
  } else {
    transportFactory = std::make_shared<TBufferedTransportFactory>();
  }

  if (zlib) {
    // hmm.. doesn't seem to be a way to make it wrap the others...
    transportFactory = std::make_shared<TZlibTransportFactory>();
  }

  // Server Info
  cout << "Starting \"" << server_type << "\" server (" << transport_type << "/" << protocol_type
       << ") listen on: ";
  if (abstract_namespace) {
    cout << '@';
  }
  cout << domain_socket;
  if (port != 0) {
    cout << port;
  }
  cout << endl;

  // Multiplexed Processor if needed
  if (boost::starts_with(protocol_type, "multi")) {
    std::shared_ptr<SecondHandler> secondHandler(new SecondHandler());
    std::shared_ptr<SecondServiceProcessor> secondProcessor(new SecondServiceProcessor(secondHandler));

    std::shared_ptr<TMultiplexedProcessor> multiplexedProcessor(new TMultiplexedProcessor());
    multiplexedProcessor->registerDefault(testProcessor); // non-multi clients go to the default processor (multi:binary, multic:compact, ...)
    multiplexedProcessor->registerProcessor("ThriftTest", testProcessor);
    multiplexedProcessor->registerProcessor("SecondService", secondProcessor);
    testProcessor = std::dynamic_pointer_cast<TProcessor>(multiplexedProcessor);
  }

  // Server
  std::shared_ptr<apache::thrift::server::TServer> server;

  if (server_type == "simple") {
    server.reset(new TSimpleServer(testProcessor, serverSocket, transportFactory, protocolFactory));
  } else if (server_type == "thread-pool") {

    std::shared_ptr<ThreadFactory> threadFactory
        = std::shared_ptr<ThreadFactory>(new ThreadFactory());

    std::shared_ptr<ThreadManager> threadManager = ThreadManager::newSimpleThreadManager(workers);
    threadManager->threadFactory(threadFactory);
    threadManager->start();

    server.reset(new TThreadPoolServer(testProcessor,
                                       serverSocket,
                                       transportFactory,
                                       protocolFactory,
                                       threadManager));
  } else if (server_type == "threaded") {
    server.reset(
        new TThreadedServer(testProcessor, serverSocket, transportFactory, protocolFactory));
  } else if (server_type == "nonblocking") {
    if (transport_type == "http") {
      std::shared_ptr<TestHandlerAsync> testHandlerAsync(new TestHandlerAsync(testHandler));
      std::shared_ptr<TAsyncProcessor> testProcessorAsync(
          new ThriftTestAsyncProcessor(testHandlerAsync));
      std::shared_ptr<TAsyncBufferProcessor> testBufferProcessor(
          new TAsyncProtocolProcessor(testProcessorAsync, protocolFactory));

      // not loading nonblockingServer into "server" because
      // TEvhttpServer doesn't inherit from TServer, and doesn't
      // provide a stop method.
      TEvhttpServer nonblockingServer(testBufferProcessor, port);
      nonblockingServer.serve();
    } else if (transport_type == "framed") {
      std::shared_ptr<transport::TNonblockingServerTransport> nbSocket;
      nbSocket.reset(
          ssl ? new transport::TNonblockingSSLServerSocket(port, sslSocketFactory)
              : new transport::TNonblockingServerSocket(port));
      server.reset(new TNonblockingServer(testProcessor, protocolFactory, nbSocket));
    } else {
      cerr << "server-type nonblocking requires transport of http or framed" << endl;
      exit(1);
    }
  }

  if (server.get() != nullptr) {
    if (protocol_type == "header") {
      // Tell the server to use the same protocol for input / output
      // if using header
      server->setOutputProtocolFactory(std::shared_ptr<TProtocolFactory>());
    }
    
    apache::thrift::concurrency::ThreadFactory factory;
    factory.setDetached(false);
    std::shared_ptr<apache::thrift::concurrency::Runnable> serverThreadRunner(server);
    std::shared_ptr<apache::thrift::concurrency::Thread> thread
        = factory.newThread(serverThreadRunner);

#ifdef HAVE_SIGNAL_H
    signal(SIGINT, signal_handler);
#endif

    thread->start();
    gMonitor.waitForever();         // wait for a shutdown signal
    
#ifdef HAVE_SIGNAL_H
    signal(SIGINT, SIG_DFL);
#endif

    server->stop();
    thread->join();
    server.reset();
  }

  cout << "done." << endl;
  return 0;
}

