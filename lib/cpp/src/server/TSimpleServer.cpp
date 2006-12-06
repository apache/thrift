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
  pair<shared_ptr<TTransport>,shared_ptr<TTransport> > iot;
  pair<shared_ptr<TProtocol>,shared_ptr<TProtocol> > iop;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TSimpleServer::run() listen(): " << ttx.what() << endl;
    return;
  }

  // Fetch client from server
  try {
    while (true) {
      client = serverTransport_->accept();
      iot = transportFactory_->getIOTransports(client);
      iop = protocolFactory_->getIOProtocols(iot.first, iot.second);
      try {
        while (processor_->process(iop.first, iop.second)) {
          // Peek ahead, is the remote side closed?
          if (!iot.first->peek()) {
            break;
          }
        }
      } catch (TTransportException& ttx) {
        cerr << "TSimpleServer client died: " << ttx.what() << endl;
      } catch (TException& tx) {
        cerr << "TSimpleServer exception: " << tx.what() << endl;
      }
      iot.first->close();
      iot.second->close();
      client->close();
    }
  } catch (TTransportException& ttx) {
    cerr << "TServerTransport died on accept: " << ttx.what() << endl;
  } catch (TException& tx) {
    cerr << "Some kind of accept exception: " << tx.what() << endl;
  }

  // TODO(mcslee): Could this be a timeout case? Or always the real thing?
}

}}} // facebook::thrift::server
