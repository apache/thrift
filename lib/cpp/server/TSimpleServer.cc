#include "server/TSimpleServer.h"
#include <string>
using namespace std;

void TSimpleServer::run() {
  TTransport* client;

  // Start the server listening
  if (serverTransport_->listen() == false) {
    // TODO(mcslee): Log error here
    fprintf(stderr, "TSimpleServer::run(): Call to listen failed\n");
    return;
  }

  // Fetch client from server
  while (true) {
    // fprintf(stderr, "Listening for connection\n");
    if ((client = serverTransport_->accept()) == NULL) {
      // fprintf(stderr, "Got NULL connection, exiting.\n");
      break;
    }

    while (true) {
      // Read header from client
      // fprintf(stderr, "Reading 4 byte header from client.\n");
      string in;
      if (client->read(in, 4) <= 0) {
        // fprintf(stderr, "Size header negative. Exception!\n");
        break;
      }
      
      // Read payload from client
      int32_t size = *(int32_t*)(in.data());
      // fprintf(stderr, "Reading %d byte payload from client.\n", size);
      if (client->read(in, size) < size) {
        // fprintf(stderr, "Didn't get enough data!!!\n");
        break;
      }
      
      // Pass payload to dispatcher
      // TODO(mcslee): Wrap this in try/catch and return exceptions
      string out = dispatcher_->dispatch(in);
      
      size = out.size();
      
      // Write size of response packet
      client->write(string((char*)&size, 4));
      
      // Write response payload
      client->write(out);
    }
  
    // Clean up that client
    // fprintf(stderr, "Closing and cleaning up client\n");
    client->close();
    delete client;
  }

  // TODO(mcslee): Is this a timeout case or the real thing?
}
