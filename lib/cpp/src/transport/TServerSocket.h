#ifndef T_SERVER_SOCKET_H
#define T_SERVER_SOCKET_H

#include "transport/TServerTransport.h"

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
  TTransport* acceptImpl();

 private:

  int port_;
  int serverSocket_;
  int acceptBacklog_;
};

#endif
