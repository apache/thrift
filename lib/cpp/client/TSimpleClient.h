#ifndef T_SIMPLE_CLIENT_H
#define T_SIMPLE_CLIENT_H

#include "client/TClient.h"
#include "transport/TTransport.h"

class TSimpleClient : public TClient {
 public:
  TSimpleClient(TTransport* transport);
  ~TSimpleClient() {}

  bool open();
  void close();
  std::string dispatch(const std::string& in);

 protected:
  TTransport* transport_;
};

#endif

