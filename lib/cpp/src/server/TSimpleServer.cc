#include "server/TSimpleServer.h"
#include "transport/TBufferedTransport.h"
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
void TSimpleServer::run() {

  shared_ptr<TTransport> client;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TSimpleServer::run() listen(): " << ttx.getMessage() << endl;
    return;
  }

  // Fetch client from server
  while (true) {
    try {
      client = serverTransport_->accept();
      if (client != NULL) {
        // Process for as long as we can keep the processor happy!
        shared_ptr<TBufferedTransport> bufferedClient(new TBufferedTransport(client));
        while (processor_->process(bufferedClient)) {}
      }
    } catch (TTransportException& ttx) {
      if (client != NULL) {
        cerr << "TSimpleServer client died: " << ttx.getMessage() << endl;
      }
    }
  
    // Clean up the client
    if (client != NULL) {

      // Ensure no resource leaks
      client->close();
     }
  }

  // TODO(mcslee): Could this be a timeout case? Or always the real thing?
}

}}} // facebook::thrift::server
