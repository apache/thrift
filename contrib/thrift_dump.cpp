// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <cstdlib>
#include <iostream>

#include <transport/TBufferTransports.h>
#include <transport/TFDTransport.h>
#include <protocol/TBinaryProtocol.h>
#include <protocol/TDebugProtocol.h>
#include <protocol/TProtocolTap.h>

using namespace std;
using boost::shared_ptr;
using namespace apache::thrift::transport;
using namespace apache::thrift::protocol;

void usage() {
  fprintf(stderr,
      "usage: thrift_dump {-b|-f|-s} < input > ouput\n"
      "  -b TBufferedTransport messages\n"
      "  -f TFramedTransport messages\n"
      "  -s Raw structures\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    usage();
  }

  shared_ptr<TTransport> stdin_trans(new TFDTransport(STDIN_FILENO));
  shared_ptr<TTransport> itrans;

  if (argv[1] == std::string("-b") || argv[1] == std::string("-s")) {
    itrans.reset(new TBufferedTransport(stdin_trans));
  } else if (argv[1] == std::string("-f")) {
    itrans.reset(new TFramedTransport(stdin_trans));
  } else {
    usage();
  }

  shared_ptr<TProtocol> iprot(new TBinaryProtocol(itrans));
  shared_ptr<TProtocol> oprot(
      new TDebugProtocol(
        shared_ptr<TTransport>(new TBufferedTransport(
          shared_ptr<TTransport>(new TFDTransport(STDOUT_FILENO))))));

  TProtocolTap tap(iprot, oprot);

  try {
    if (argv[1] == std::string("-s")) {
      for (;;) {
        tap.skip(T_STRUCT);
      }
    } else {
      std::string name;
      TMessageType messageType;
      int32_t seqid;
      for (;;) {
        tap.readMessageBegin(name, messageType, seqid);
        tap.skip(T_STRUCT);
        tap.readMessageEnd();
      }
    }
  } catch (TProtocolException exn) {
    cout << "Protocol Exception: " << exn.what() << endl;
  } catch (...) {
  }

  cout << endl;

  return 0;
}
