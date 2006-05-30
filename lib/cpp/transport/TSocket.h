#ifndef T_SOCKET_H
#define T_SOCKET_H

#include <string>

#include "transport/TTransport.h"
#include "transport/TServerSocket.h"

class TSocketOptions;

/**
 * TCP Socket implementation of the TTransport interface.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TSocket : public TTransport {
  friend TTransport* TServerSocket::accept();

 public:
  TSocket(std::string host, int port);
  ~TSocket();

  bool open();
  void close();
  int  read (std::string &s, uint32_t size);
  void write(const std::string& s);

  bool setLinger(bool on, int linger);
  bool setNoDelay(bool noDelay);

 private:
  TSocket(int socket);
  TSocketOptions *options_;
  std::string host_;
  int port_;
  int socket_;
};

#endif
