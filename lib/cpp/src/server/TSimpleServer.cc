#include "server/TSimpleServer.h"
#include "transport/TTransportException.h"
#include <string>
#include <iostream>
using namespace std;

namespace facebook { namespace thrift { namespace server { 

/**
 * A simple single-threaded application server. Perfect for unit tests!
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
void TSimpleServer::serve() {

  shared_ptr<TTransport> client;
  pair<shared_ptr<TTransport>,shared_ptr<TTransport> > io;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TSimpleServer::run() listen(): " << ttx.getMessage() << endl;
    return;
  }

  // Fetch client from server
  try {
    while (true) {
      client = serverTransport_->accept();
      io = transportFactory_->getIOTransports(client);
      try {
        while (processor_->process(io.first, io.second)) {}
      } catch (TTransportException& ttx) {
        cerr << "TSimpleServer client died: " << ttx.getMessage() << endl;
      }
      io.first->close();
      io.second->close();
      client->close();
    }
  } catch (TTransportException& ttx) {
    cerr << "TServerTransport died on accept: " << ttx.getMessage() << endl;
  }

  // TODO(mcslee): Could this be a timeout case? Or always the real thing?
}

}}} // facebook::thrift::server
