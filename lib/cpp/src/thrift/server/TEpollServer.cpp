/*
 * TEpollServer use epoll and multi process
 */

#include <server/TEpollServer.h>
#include <thrift/transport/TTransportException.h>
#include <string>
#include <iostream>



namespace apache { namespace thrift { namespace server {

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using boost::shared_ptr;

//listen option ...
void TEpollServer::listen() {
    // Start the server listening
  serverTransport_->listen();

	// Run the preServe event
	if (eventHandler_ != NULL) {
	  eventHandler_->preServe();
	}
}

void TEpollServer::epoll() {
    boost::static_pointer_cast<TServerEpollSocket>(serverTransport_)->FaddWatch(true);
}

/**
 * A simple single-threaded application server. Perfect for unit tests!
 *
 */
void TEpollServer::serve() {
  // Fetch client from server
  while (!stop_) {
    shared_ptr<TTransport> client;
    shared_ptr<TTransport> inputTransport;
    shared_ptr<TTransport> outputTransport;
    shared_ptr<TProtocol> inputProtocol;
    shared_ptr<TProtocol> outputProtocol;

    try {
        client = serverTransport_->accept(); 
    } catch (TTransportException& ttx) {
        continue;
    }

    try {
      inputTransport = inputTransportFactory_->getTransport(client);
      outputTransport = outputTransportFactory_->getTransport(client);
      inputProtocol = inputProtocolFactory_->getProtocol(inputTransport);
      outputProtocol = outputProtocolFactory_->getProtocol(outputTransport);
    } catch (TTransportException& ttx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
          string errStr = string("TServerTransport died on accept: ") + ttx.what();
          GlobalOutput(errStr.c_str());
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      string errStr = string("Some kind of accept exception: ") + tx.what();
      GlobalOutput(errStr.c_str());
      continue;
    } catch (string s) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      string errStr = string("Some kind of accept exception: ") + s;
      GlobalOutput(errStr.c_str());
      break;
    }

    // Get the processor
    shared_ptr<TProcessor> processor = getProcessor(inputProtocol,
                                                    outputProtocol, client);

    void* connectionContext = NULL;
    if (eventHandler_ != NULL) {
      connectionContext = eventHandler_->createContext(inputProtocol, outputProtocol);
    }

    try {
        //try to process maybe sucess or fail, or client closed.
        if (eventHandler_ != NULL) {
          eventHandler_->processContext(connectionContext, client);
        }

        if (processor->process(inputProtocol, outputProtocol,
                                connectionContext)) {
            //modify the epoll event.
            boost::static_pointer_cast<TServerEpollSocket>(serverTransport_)->FmodWatch(client);
            continue;
        }        

    } catch (const TTransportException& ttx) {
      string errStr = string("TEpollServer client died: ") + ttx.what();
      //GlobalOutput(errStr.c_str());
    } catch (const std::exception& x) {
      /*GlobalOutput.printf("TEpollServer exception: %s: %s",
                          typeid(x).name(), x.what());*/
    } catch (...) {
      //GlobalOutput("TEpollServer uncaught exception.");
    }
    if (eventHandler_ != NULL) {
      eventHandler_->deleteContext(connectionContext, inputProtocol, outputProtocol);
    }

    try {
      inputTransport->close();
    } catch (const TTransportException& ttx) {
      string errStr = string("TEpollServer input close failed: ")
        + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      outputTransport->close();
    } catch (const TTransportException& ttx) {
      string errStr = string("TEpollServer output close failed: ")
        + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
        boost::static_pointer_cast<TServerEpollSocket>(serverTransport_)->clearClient(client);
    } catch (const TTransportException& ttx) {
      string errStr = string("TEpollServer client close failed: ")
        + ttx.what();
      GlobalOutput(errStr.c_str());
    }
  }

  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TTransportException &ttx) {
      string errStr = string("TServerTransport failed on close: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }
}

}}} // apache::thrift::server
