#include "TSimpleClient.h"
using std::string;

TSimpleClient::TSimpleClient(TTransport* transport) :
  transport_(transport) {}

bool TSimpleClient::open() {
  return transport_->open();
}

void TSimpleClient::close() {
  transport_->close();
}

std::string TSimpleClient::dispatch(const string& s) {
  // Write size header
  int32_t size = s.size();
  // fprintf(stderr, "Writing size header %d to server\n", size);
  transport_->write(string((char*)&size, 4));

  // Write data payload
  // fprintf(stderr, "Writing %d byte payload to server\n", (int)s.size());
  transport_->write(s);

  // Read response size
  // fprintf(stderr, "Reading 4-byte response size header\n");
  string response;
  transport_->read(response, 4);
  size = *(int32_t*)response.data();

  // Read response data
  if (size < 0) {
    // TODO(mcslee): Handle exception
    // fprintf(stderr, "Exception case! Response size < 0\n");
    return "";
  } else {
    // fprintf(stderr, "Reading %d byte response payload\n", size);
    transport_->read(response, size);
    // TODO(mcslee): Check that we actually read enough data
    // fprintf(stderr, "Done reading payload, returning.\n");
    return response;
  }
}

