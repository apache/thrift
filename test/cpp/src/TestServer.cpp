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

#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include <thrift/concurrency/ThreadManager.h>
#include <thrift/concurrency/PlatformThreadFactory.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/protocol/TCompactProtocol.h>
#include <thrift/protocol/TJSONProtocol.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/server/TThreadedServer.h>
#include <thrift/server/TThreadPoolServer.h>
#include <thrift/async/TEvhttpServer.h>
#include <thrift/async/TAsyncBufferProcessor.h>
#include <thrift/async/TAsyncProtocolProcessor.h>
#include <thrift/server/TNonblockingServer.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TSSLServerSocket.h>
#include <thrift/transport/TSSLSocket.h>
#include <thrift/transport/THttpServer.h>
#include <thrift/transport/THttpTransport.h>
#include <thrift/transport/TTransportUtils.h>
#include "ThriftTest.h"

#include <iostream>
#include <stdexcept>
#include <sstream>

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <thrift/cxxfunctional.h>

#include <signal.h>
#if _WIN32
   #include <thrift/windows/TWinsockSingleton.h>
#endif

using namespace std;

using namespace apache::thrift;
using namespace apache::thrift::concurrency;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::server;
using namespace apache::thrift::async;

using namespace thrift::test;

// Length of argv[0] - Length of script dir
#define EXECUTABLE_FILE_NAME_LENGTH 19

class TestHandler : public ThriftTestIf {
 public:
  TestHandler() {}

  void testVoid() {
    printf("testVoid()\n");
  }

  void testString(string& out, const string &thing) {
    printf("testString(\"%s\")\n", thing.c_str());
    out = thing;
  }

  int8_t testByte(const int8_t thing) {
    printf("testByte(%d)\n", (int)thing);
    return thing;
  }

  int32_t testI32(const int32_t thing) {
    printf("testI32(%d)\n", thing);
    return thing;
  }

  int64_t testI64(const int64_t thing) {
    printf("testI64(%" PRId64 ")\n", thing);
    return thing;
  }

  double testDouble(const double thing) {
    printf("testDouble(%f)\n", thing);
    return thing;
  }

  void testStruct(Xtruct& out, const Xtruct &thing) {
    printf("testStruct({\"%s\", %d, %d, %" PRId64 "})\n", thing.string_thing.c_str(), (int)thing.byte_thing, thing.i32_thing, thing.i64_thing);
    out = thing;
  }

  void testNest(Xtruct2& out, const Xtruct2& nest) {
    const Xtruct &thing = nest.struct_thing;
    printf("testNest({%d, {\"%s\", %d, %d, %" PRId64 "}, %d})\n", (int)nest.byte_thing, thing.string_thing.c_str(), (int)thing.byte_thing, thing.i32_thing, thing.i64_thing, nest.i32_thing);
    out = nest;
  }

  void testMap(map<int32_t, int32_t> &out, const map<int32_t, int32_t> &thing) {
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

  void testStringMap(map<std::string, std::string> &out, const map<std::string, std::string> &thing) {
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

  void testSet(set<int32_t> &out, const set<int32_t> &thing) {
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

  void testList(vector<int32_t> &out, const vector<int32_t> &thing) {
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

  Numberz::type testEnum(const Numberz::type thing) {
    printf("testEnum(%d)\n", thing);
    return thing;
  }

  UserId testTypedef(const UserId thing) {
    printf("testTypedef(%" PRId64 ")\n", thing);
    return thing;
  }

  void testMapMap(map<int32_t, map<int32_t,int32_t> > &mapmap, const int32_t hello) {
    printf("testMapMap(%d)\n", hello);

    map<int32_t,int32_t> pos;
    map<int32_t,int32_t> neg;
    for (int i = 1; i < 5; i++) {
      pos.insert(make_pair(i,i));
      neg.insert(make_pair(-i,-i));
    }

    mapmap.insert(make_pair(4, pos));
    mapmap.insert(make_pair(-4, neg));

  }

  void testInsanity(map<UserId, map<Numberz::type,Insanity> > &insane, const Insanity &argument) {
    (void) argument;
    printf("testInsanity()\n");

    Xtruct hello;
    hello.string_thing = "Hello2";
    hello.byte_thing = 2;
    hello.i32_thing = 2;
    hello.i64_thing = 2;

    Xtruct goodbye;
    goodbye.string_thing = "Goodbye4";
    goodbye.byte_thing = 4;
    goodbye.i32_thing = 4;
    goodbye.i64_thing = 4;

    Insanity crazy;
    crazy.userMap.insert(make_pair(Numberz::EIGHT, 8));
    crazy.xtructs.push_back(goodbye);

    Insanity looney;
    crazy.userMap.insert(make_pair(Numberz::FIVE, 5));
    crazy.xtructs.push_back(hello);

    map<Numberz::type, Insanity> first_map;
    map<Numberz::type, Insanity> second_map;

    first_map.insert(make_pair(Numberz::TWO, crazy));
    first_map.insert(make_pair(Numberz::THREE, crazy));

    second_map.insert(make_pair(Numberz::SIX, looney));

    insane.insert(make_pair(1, first_map));
    insane.insert(make_pair(2, second_map));

    printf("return");
    printf(" = {");
    map<UserId, map<Numberz::type,Insanity> >::const_iterator i_iter;
    for (i_iter = insane.begin(); i_iter != insane.end(); ++i_iter) {
      printf("%" PRId64 " => {", i_iter->first);
      map<Numberz::type,Insanity>::const_iterator i2_iter;
      for (i2_iter = i_iter->second.begin();
           i2_iter != i_iter->second.end();
           ++i2_iter) {
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
          printf("{\"%s\", %d, %d, %" PRId64 "}, ", x->string_thing.c_str(), (int)x->byte_thing, x->i32_thing, x->i64_thing);
        }
        printf("}");

        printf("}, ");
      }
      printf("}, ");
    }
    printf("}\n");


  }

  void testMulti(Xtruct &hello, const int8_t arg0, const int32_t arg1, const int64_t arg2, const std::map<int16_t, std::string>  &arg3, const Numberz::type arg4, const UserId arg5) {
    (void) arg3;
    (void) arg4;
    (void) arg5;

    printf("testMulti()\n");

    hello.string_thing = "Hello2";
    hello.byte_thing = arg0;
    hello.i32_thing = arg1;
    hello.i64_thing = (int64_t)arg2;
  }

  void testException(const std::string &arg)
    throw(Xception, apache::thrift::TException)
  {
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

  void testMultiException(Xtruct &result, const std::string &arg0, const std::string &arg1) throw(Xception, Xception2) {

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

  void testOneway(const int32_t sleepFor) {
    printf("testOneway(%d): Sleeping...\n", sleepFor);
    THRIFT_SLEEP_SEC(sleepFor);
    printf("testOneway(%d): done sleeping!\n", sleepFor);
  }
};


class TestProcessorEventHandler : public TProcessorEventHandler {
  virtual void* getContext(const char* fn_name, void* serverContext) {
    (void) serverContext;
    return new std::string(fn_name);
  }
  virtual void freeContext(void* ctx, const char* fn_name) {
    (void) fn_name;
    delete static_cast<std::string*>(ctx);
  }
  virtual void preRead(void* ctx, const char* fn_name) {
    communicate("preRead", ctx, fn_name);
  }
  virtual void postRead(void* ctx, const char* fn_name, uint32_t bytes) {
    (void) bytes;
    communicate("postRead", ctx, fn_name);
  }
  virtual void preWrite(void* ctx, const char* fn_name) {
    communicate("preWrite", ctx, fn_name);
  }
  virtual void postWrite(void* ctx, const char* fn_name, uint32_t bytes) {
    (void) bytes;
    communicate("postWrite", ctx, fn_name);
  }
  virtual void asyncComplete(void* ctx, const char* fn_name) {
    communicate("asyncComplete", ctx, fn_name);
  }
  virtual void handlerError(void* ctx, const char* fn_name) {
    communicate("handlerError", ctx, fn_name);
  }

  void communicate(const char* event, void* ctx, const char* fn_name) {
    std::cout << event << ": " << *static_cast<std::string*>(ctx) << " = " << fn_name << std::endl;
  }
};


class TestHandlerAsync : public ThriftTestCobSvIf {
public:
  TestHandlerAsync(boost::shared_ptr<TestHandler>& handler) : _delegate(handler) {}
  virtual ~TestHandlerAsync() {}

  virtual void testVoid(tcxx::function<void()> cob) {
    _delegate->testVoid();
    cob();
  }

  virtual void testString(tcxx::function<void(std::string const& _return)> cob, const std::string& thing) {
    std::string res;
    _delegate->testString(res, thing);
    cob(res);
  }

  virtual void testByte(tcxx::function<void(int8_t const& _return)> cob, const int8_t thing) {
    int8_t res = _delegate->testByte(thing);
    cob(res);
  }

  virtual void testI32(tcxx::function<void(int32_t const& _return)> cob, const int32_t thing) {
    int32_t res = _delegate->testI32(thing);
    cob(res);
  }

  virtual void testI64(tcxx::function<void(int64_t const& _return)> cob, const int64_t thing) {
    int64_t res = _delegate->testI64(thing);
    cob(res);
  }

  virtual void testDouble(tcxx::function<void(double const& _return)> cob, const double thing) {
    double res = _delegate->testDouble(thing);
    cob(res);
  }

  virtual void testStruct(tcxx::function<void(Xtruct const& _return)> cob, const Xtruct& thing) {
    Xtruct res;
    _delegate->testStruct(res, thing);
    cob(res);
  }

  virtual void testNest(tcxx::function<void(Xtruct2 const& _return)> cob, const Xtruct2& thing) {
    Xtruct2 res;
    _delegate->testNest(res, thing);
    cob(res);
  }

  virtual void testMap(tcxx::function<void(std::map<int32_t, int32_t>  const& _return)> cob, const std::map<int32_t, int32_t> & thing) {
    std::map<int32_t, int32_t> res;
    _delegate->testMap(res, thing);
    cob(res);
  }

  virtual void testStringMap(tcxx::function<void(std::map<std::string, std::string>  const& _return)> cob, const std::map<std::string, std::string> & thing) {
    std::map<std::string, std::string> res;
    _delegate->testStringMap(res, thing);
    cob(res);
  }

  virtual void testSet(tcxx::function<void(std::set<int32_t>  const& _return)> cob, const std::set<int32_t> & thing) {
    std::set<int32_t> res;
    _delegate->testSet(res, thing);
    cob(res);
  }

  virtual void testList(tcxx::function<void(std::vector<int32_t>  const& _return)> cob, const std::vector<int32_t> & thing) {
    std::vector<int32_t> res;
    _delegate->testList(res, thing);
    cob(res);
  }

  virtual void testEnum(tcxx::function<void(Numberz::type const& _return)> cob, const Numberz::type thing) {
    Numberz::type res = _delegate->testEnum(thing);
    cob(res);
  }

  virtual void testTypedef(tcxx::function<void(UserId const& _return)> cob, const UserId thing) {
    UserId res = _delegate->testTypedef(thing);
    cob(res);
  }

  virtual void testMapMap(tcxx::function<void(std::map<int32_t, std::map<int32_t, int32_t> >  const& _return)> cob, const int32_t hello) {
    std::map<int32_t, std::map<int32_t, int32_t> > res;
    _delegate->testMapMap(res, hello);
    cob(res);
  }

  virtual void testInsanity(tcxx::function<void(std::map<UserId, std::map<Numberz::type, Insanity> >  const& _return)> cob, const Insanity& argument) {
    std::map<UserId, std::map<Numberz::type, Insanity> > res;
    _delegate->testInsanity(res, argument);
    cob(res);
 }

  virtual void testMulti(tcxx::function<void(Xtruct const& _return)> cob, const int8_t arg0, const int32_t arg1, const int64_t arg2, const std::map<int16_t, std::string> & arg3, const Numberz::type arg4, const UserId arg5) {
    Xtruct res;
    _delegate->testMulti(res, arg0, arg1, arg2, arg3, arg4, arg5);
    cob(res);
  }

  virtual void testException(tcxx::function<void()> cob, tcxx::function<void(::apache::thrift::TDelayedException* _throw)> exn_cob, const std::string& arg) {
    try {
      _delegate->testException(arg);
    } catch(const apache::thrift::TException& e) {
      exn_cob(apache::thrift::TDelayedException::delayException(e));
      return;
    }
    cob();
  }

  virtual void testMultiException(tcxx::function<void(Xtruct const& _return)> cob, tcxx::function<void(::apache::thrift::TDelayedException* _throw)> exn_cob, const std::string& arg0, const std::string& arg1) {
    Xtruct res;
    try {
      _delegate->testMultiException(res, arg0, arg1);
    } catch(const apache::thrift::TException& e) {
      exn_cob(apache::thrift::TDelayedException::delayException(e));
      return;
    }
    cob(res);
  }

  virtual void testOneway(tcxx::function<void()> cob, const int32_t secondsToSleep) {
    _delegate->testOneway(secondsToSleep);
    cob();
  }

protected:
  boost::shared_ptr<TestHandler> _delegate;
};


int main(int argc, char **argv) {

  string file_path = boost::filesystem::system_complete(argv[0]).string();
  string dir_path = file_path.substr(0, file_path.size()-EXECUTABLE_FILE_NAME_LENGTH);

  #if _WIN32
  transport::TWinsockSingleton::create();
#endif
  int port = 9090;
  bool ssl = false;
  string transport_type = "buffered";
  string protocol_type = "binary";
  string server_type = "simple";
  string domain_socket = "";
  size_t workers = 4;


  boost::program_options::options_description desc("Allowed options");
  desc.add_options()
      ("help,h", "produce help message")
      ("port", boost::program_options::value<int>(&port)->default_value(port), "Port number to listen")
	  ("domain-socket", boost::program_options::value<string>(&domain_socket)->default_value(domain_socket),
	    "Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)")
      ("server-type", boost::program_options::value<string>(&server_type)->default_value(server_type),
        "type of server, \"simple\", \"thread-pool\", \"threaded\", or \"nonblocking\"")
      ("transport", boost::program_options::value<string>(&transport_type)->default_value(transport_type),
        "transport: buffered, framed, http")
      ("protocol", boost::program_options::value<string>(&protocol_type)->default_value(protocol_type),
        "protocol: binary, compact, json")
	  ("ssl", "Encrypted Transport using SSL")
	  ("processor-events", "processor-events")
      ("workers,n", boost::program_options::value<size_t>(&workers)->default_value(workers),
        "Number of thread pools workers. Only valid for thread-pool server type")
  ;

  boost::program_options::variables_map vm;
  boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
  boost::program_options::notify(vm);

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
          throw invalid_argument("Unknown server type "+server_type);
      }
    }

    if (!protocol_type.empty()) {
      if (protocol_type == "binary") {
      } else if (protocol_type == "compact") {
      } else if (protocol_type == "json") {
      } else {
          throw invalid_argument("Unknown protocol type "+protocol_type);
      }
    }

    if (!transport_type.empty()) {
      if (transport_type == "buffered") {
      } else if (transport_type == "framed") {
      } else if (transport_type == "http") {
      } else {
          throw invalid_argument("Unknown transport type "+transport_type);
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

  // Dispatcher
  boost::shared_ptr<TProtocolFactory> protocolFactory;
  if (protocol_type == "json") {
    boost::shared_ptr<TProtocolFactory> jsonProtocolFactory(new TJSONProtocolFactory());
    protocolFactory = jsonProtocolFactory;
  } else if (protocol_type == "compact") {
    boost::shared_ptr<TProtocolFactory> compactProtocolFactory(new TCompactProtocolFactory());
    protocolFactory = compactProtocolFactory;
  } else {
    boost::shared_ptr<TProtocolFactory> binaryProtocolFactory(new TBinaryProtocolFactoryT<TBufferBase>());
    protocolFactory = binaryProtocolFactory;
  }

  // Processor
  boost::shared_ptr<TestHandler> testHandler(new TestHandler());
  boost::shared_ptr<ThriftTestProcessor> testProcessor(new ThriftTestProcessor(testHandler));

  if (vm.count("processor-events")) {
    testProcessor->setEventHandler(boost::shared_ptr<TProcessorEventHandler>(
          new TestProcessorEventHandler()));
  }

  // Transport
  boost::shared_ptr<TSSLSocketFactory> sslSocketFactory;
  boost::shared_ptr<TServerSocket> serverSocket;

  if (ssl) {
    sslSocketFactory = boost::shared_ptr<TSSLSocketFactory>(new TSSLSocketFactory());
    sslSocketFactory->loadCertificate((dir_path + "../keys/server.crt").c_str());
    sslSocketFactory->loadPrivateKey((dir_path + "../keys/server.key").c_str());
    sslSocketFactory->ciphers("ALL:!ADH:!LOW:!EXP:!MD5:@STRENGTH");
    serverSocket = boost::shared_ptr<TServerSocket>(new TSSLServerSocket(port, sslSocketFactory));
  } else {
    if (domain_socket != "") {
      unlink(domain_socket.c_str());
      serverSocket = boost::shared_ptr<TServerSocket>(new TServerSocket(domain_socket));
      port = 0;
    } else {
      serverSocket = boost::shared_ptr<TServerSocket>(new TServerSocket(port));
    }
  }

  // Factory
  boost::shared_ptr<TTransportFactory> transportFactory;

  if (transport_type == "http" && server_type != "nonblocking") {
    boost::shared_ptr<TTransportFactory> httpTransportFactory(new THttpServerTransportFactory());
    transportFactory = httpTransportFactory;
  } else if (transport_type == "framed") {
    boost::shared_ptr<TTransportFactory> framedTransportFactory(new TFramedTransportFactory());
    transportFactory = framedTransportFactory;
  } else {
    boost::shared_ptr<TTransportFactory> bufferedTransportFactory(new TBufferedTransportFactory());
    transportFactory = bufferedTransportFactory;
  }

  // Server Info
  cout << "Starting \"" << server_type << "\" server ("
    << transport_type << "/" << protocol_type << ") listen on: " << domain_socket;
  if (port != 0) {
    cout << port;
  }
  cout << endl;

  // Server
  boost::shared_ptr<apache::thrift::server::TServer> server;

  if (server_type == "simple") {
    server.reset(new TSimpleServer(testProcessor,
                               serverSocket,
                               transportFactory,
                               protocolFactory));
  } else if (server_type == "thread-pool") {

    boost::shared_ptr<ThreadManager> threadManager =
      ThreadManager::newSimpleThreadManager(workers);

    boost::shared_ptr<PlatformThreadFactory> threadFactory =
      boost::shared_ptr<PlatformThreadFactory>(new PlatformThreadFactory());

    threadManager->threadFactory(threadFactory);

    threadManager->start();

    server.reset(new TThreadPoolServer(testProcessor,
                                       serverSocket,
                                       transportFactory,
                                       protocolFactory,
                                       threadManager));
  } else if (server_type == "threaded") {

    server.reset(new TThreadedServer(testProcessor,
                                   serverSocket,
                                   transportFactory,
                                   protocolFactory));
  } else if (server_type == "nonblocking") {
    if(transport_type == "http") {
      boost::shared_ptr<TestHandlerAsync> testHandlerAsync(new TestHandlerAsync(testHandler));
      boost::shared_ptr<TAsyncProcessor> testProcessorAsync(new ThriftTestAsyncProcessor(testHandlerAsync));
      boost::shared_ptr<TAsyncBufferProcessor> testBufferProcessor(new TAsyncProtocolProcessor(testProcessorAsync, protocolFactory));

      // not loading nonblockingServer into "server" because
      // TEvhttpServer doesn't inherit from TServer, and doesn't
      // provide a stop method.
      TEvhttpServer nonblockingServer(testBufferProcessor, port);
      nonblockingServer.serve();
    } else {
      server.reset(new TNonblockingServer(testProcessor, port));
    }
  }

  if(server.get() != NULL)
  {
    apache::thrift::concurrency::PlatformThreadFactory factory;
    factory.setDetached(false);
    boost::shared_ptr<apache::thrift::concurrency::Runnable> serverThreadRunner(server);
    boost::shared_ptr<apache::thrift::concurrency::Thread> thread = factory.newThread(serverThreadRunner);
    thread->start();

    // HACK: cross language test suite is unable to handle cin properly
    //       that's why we stay in a endless loop here
    while(1){}
    // FIXME: find another way to stop the server (e.g. a signal)
    // cout<<"Press enter to stop the server."<<endl;
    // cin.ignore(); //wait until a key is pressed

    server->stop();
    thread->join();
    server.reset();
  }

  cout << "done." << endl;
  return 0;
}
