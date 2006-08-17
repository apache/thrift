#include <concurrency/ThreadManager.h>
#include <concurrency/PosixThreadFactory.h>
#include <concurrency/Monitor.h>
#include <concurrency/Util.h>
#include <protocol/TBinaryProtocol.h>
#include <server/TSimpleServer.h>
#include <server/TThreadPoolServer.h>
#include <transport/TServerSocket.h>
#include <transport/TSocket.h>
#include <transport/TBufferedTransport.h>
#include "StressTest.h"

#include <iostream>
#include <set>
#include <stdexcept>
#include <sstream>

using namespace std;

using namespace facebook::thrift;
using namespace facebook::thrift::protocol;
using namespace facebook::thrift::transport;
using namespace facebook::thrift::server;

using namespace test::stress;

class Server : public ServiceServerIf {
 public:
  Server(shared_ptr<TProtocol> protocol) :
    ServiceServerIf(protocol) {}

  void echoVoid() {return;}
  uint8_t echoByte(uint8_t arg) {return arg;}
  uint16_t echoU16(uint16_t arg) {return arg;}
  uint32_t echoU32(uint32_t arg) {return arg;}
  uint64_t echoU64(uint64_t arg) {return arg;}
};

class ClientThread: public Runnable {
public:

  ClientThread(shared_ptr<TTransport>transport, shared_ptr<ServiceClient> client, Monitor& monitor, size_t& workerCount, size_t loopCount) :
    _transport(transport),
    _client(client),
    _monitor(monitor),
    _workerCount(workerCount),
    _loopCount(loopCount)
  {}

  void run() {

    // Wait for all worker threads to start 

    {Synchronized s(_monitor);
	while(_workerCount == 0) {
	  _monitor.wait();
	}
    }

    _startTime = Util::currentTime();

    _transport->open();

    //uint64_t arg = 0;
    //uint64_t result = 0;

    for(size_t ix = 0; ix < _loopCount; ix++) {
      //      result = _client->echoU64(arg);
      //      assert(result == arg);
      _client->echoVoid();
      //arg++;
    }
    
    _endTime = Util::currentTime();

    _transport->close();
    
    _done = true;
      
    {Synchronized s(_monitor);

      _workerCount--;

      if(_workerCount == 0) {
	
	_monitor.notify();
      }
    }
  }
  
private:
  shared_ptr<TTransport> _transport;
  shared_ptr<ServiceClient> _client;
  Monitor& _monitor;
  size_t& _workerCount;
  size_t _loopCount;
  long long _startTime;
  long long _endTime;
  bool _done;
  Monitor _sleep;
};
    

int main(int argc, char **argv) {

  int port = 9090;
  string serverType = "thread-pool";
  string protocolType = "binary";
  size_t workerCount = 4;
  size_t clientCount = 10;
  size_t loopCount = 10000;

  ostringstream usage;

  usage <<
    argv[0] << " [--port=<port number>] [--server-type=<server-type>] [--protocol-type=<protocol-type>] [--workers=<worker-count>]" << endl <<

    "\t\tserver-type\t\ttype of server, \"simple\" or \"thread-pool\".  Default is " << serverType << endl <<

    "\t\tprotocol-type\t\ttype of protocol, \"binary\", \"ascii\", or \"xml\".  Default is " << protocolType << endl <<

    "\t\tworkers\t\tNumber of thread pools workers.  Only valid for thread-pool server type.  Default is " << workerCount << endl;
    
  map<string, string>  args;
  
  for(int ix = 1; ix < argc; ix++) {

    string arg(argv[ix]);

    if(arg.compare(0,2, "--") == 0) {

      size_t end = arg.find_first_of("=", 2);

      if(end != string::npos) {
	args[string(arg, 2, end - 2)] = string(arg, end + 1);
      } else {
	args[string(arg, 2, end - 2)] = "true";
      }
      ix++;
    } else {
      throw invalid_argument("Unexcepted command line token: "+arg);
    }
  }

  try {

    if(!args["port"].empty()) {
      port = atoi(args["port"].c_str());
    }

    if(!args["server-type"].empty()) {
      serverType = args["server-type"];
      
      if(serverType == "simple") {

      } else if(serverType == "thread-pool") {

      } else {

	throw invalid_argument("Unknown server type "+serverType);
      }
    }

    if(!args["workers"].empty()) {
      workerCount = atoi(args["workers"].c_str());
    }

    if(!args["clients"].empty()) {
      clientCount = atoi(args["clients"].c_str());
    }

    if(!args["loop"].empty()) {
      loopCount = atoi(args["loop"].c_str());
    }
  } catch(exception& e) {
    cerr << e.what() << endl;
    cerr << usage;
  }

  // Dispatcher
  shared_ptr<TBinaryProtocol> binaryProtocol(new TBinaryProtocol);

  shared_ptr<Server> server(new Server(binaryProtocol));

  // Options
  shared_ptr<TServerOptions> serverOptions(new TServerOptions());

  // Transport
  shared_ptr<TServerSocket> serverSocket(new TServerSocket(port));

  // ThreadFactory

  shared_ptr<PosixThreadFactory> threadFactory = shared_ptr<PosixThreadFactory>(new PosixThreadFactory());

  shared_ptr<Thread> serverThread;

  if(serverType == "simple") {

    serverThread = threadFactory->newThread(shared_ptr<Runnable>(new TSimpleServer(server, serverOptions, serverSocket)));

  } else if(serverType == "thread-pool") {

    shared_ptr<ThreadManager> threadManager = ThreadManager::newSimpleThreadManager(workerCount);

    threadManager->threadFactory(threadFactory);

    threadManager->start();

    serverThread = threadFactory->newThread(shared_ptr<TServer>(new TThreadPoolServer(server,
										      serverOptions,
										      serverSocket,
										      threadManager)));
  }

  cout << "Starting the server on port " << port << endl;

  serverThread->start();

  Monitor monitor;

  size_t threadCount = 0;

  set<shared_ptr<Thread> > clientThreads;

  for(size_t ix = 0; ix < clientCount; ix++) {
    
    shared_ptr<TSocket> socket(new TSocket("127.0.01", port));
    shared_ptr<TBufferedTransport> bufferedSocket(new TBufferedTransport(socket, 2048));
    shared_ptr<TBinaryProtocol> binaryProtocol(new TBinaryProtocol());
    shared_ptr<ServiceClient> serviceClient(new ServiceClient(bufferedSocket, binaryProtocol));
    
    clientThreads.insert(threadFactory->newThread(shared_ptr<ClientThread>(new ClientThread(bufferedSocket, serviceClient, monitor, threadCount, loopCount))));
  }
  
  for(std::set<shared_ptr<Thread> >::const_iterator thread = clientThreads.begin(); thread != clientThreads.end(); thread++) {
    (*thread)->start();
  }

  cout << endl;
  
  {Synchronized s(monitor);
    threadCount = clientCount;

    cout << "Launch "<< clientCount << " client threads" << endl;
    monitor.notifyAll();
    
    while(threadCount > 0) {
      monitor.wait();
    }
  }

  printf("done.\n");
  return 0;
}
