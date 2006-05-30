#ifndef T_CLIENT_H
#define T_CLIENT_H

#include "TDispatcher.h"

class TClient : public TDispatcher {
 public:
  virtual ~TClient() {}
  virtual bool open() = 0;
  virtual void close() = 0;
 protected:
  TClient() {}
};

#endif

