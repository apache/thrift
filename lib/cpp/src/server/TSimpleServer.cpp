// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

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
  shared_ptr<TTransport> inputTransport;
  shared_ptr<TTransport> outputTransport;
  shared_ptr<TProtocol> inputProtocol;
  shared_ptr<TProtocol> outputProtocol;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TSimpleServer::run() listen(): " << ttx.what() << endl;
    return;
  }

  // Fetch client from server
  while (true) {
    try {
      client = serverTransport_->accept();
      inputTransport = inputTransportFactory_->getTransport(client);
      outputTransport = outputTransportFactory_->getTransport(client);
      inputProtocol = inputProtocolFactory_->getProtocol(inputTransport);
      outputProtocol = outputProtocolFactory_->getProtocol(outputTransport);
      try {
        while (processor_->process(inputProtocol, outputProtocol)) {
          // Peek ahead, is the remote side closed?
          if (!inputTransport->peek()) {
            break;
          }
        }
      } catch (TTransportException& ttx) {
        cerr << "TSimpleServer client died: " << ttx.what() << endl;
      } catch (TException& tx) {
        cerr << "TSimpleServer exception: " << tx.what() << endl;
      }
      inputTransport->close();
      outputTransport->close();
      client->close();    
    } catch (TTransportException& ttx) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TServerTransport died on accept: " << ttx.what() << endl;
      continue;
    } catch (TException& tx) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "Some kind of accept exception: " << tx.what() << endl;
      continue;
    } catch (string s) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadPoolServer: Unknown exception: " << s << endl;
      break;
    }
  }

  // TODO(mcslee): Could this be a timeout case? Or always the real thing?
}

}}} // facebook::thrift::server
