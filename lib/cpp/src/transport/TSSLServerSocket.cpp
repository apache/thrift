// Copyright (c) 2009- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "TSSLServerSocket.h"
#include "TSSLSocket.h"

namespace apache { namespace thrift { namespace transport {

using namespace boost;

/**
 * SSL server socket implementation.
 *
 * @author Ping Li <pingli@facebook.com>
 */
TSSLServerSocket::TSSLServerSocket(int port,
                                   shared_ptr<TSSLSocketFactory> factory):
                                   TServerSocket(port), factory_(factory) {
  factory_->server(true);
}

TSSLServerSocket::TSSLServerSocket(int port, int sendTimeout, int recvTimeout,
                                   shared_ptr<TSSLSocketFactory> factory):
                                   TServerSocket(port, sendTimeout, recvTimeout),
                                   factory_(factory) {
  factory_->server(true);
}

shared_ptr<TSocket> TSSLServerSocket::createSocket(int client) {
  return factory_->createSocket(client);
}

}}}
