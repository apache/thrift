#ifndef T_SERVER_SOCKET_H
#define T_SERVER_SOCKET_H

#include <transport/TServerTransport.h>
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

class TSocket;

/**
 * Server socket implementation of TServerTransport. Wrapper around a unix
 * socket listen and accept calls.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServerSocket : public TServerTransport {
 public:
  TServerSocket(int port);
  ~TServerSocket();

  void listen();
  void close();

 protected:
  shared_ptr<TTransport> acceptImpl();

 private:

  int port_;
  int serverSocket_;
  int acceptBacklog_;
};

}}} // facebook::thrift::transport

#endif
