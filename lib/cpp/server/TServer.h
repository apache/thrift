#ifndef T_SERVER_H
#define T_SERVER_H

#include "TDispatcher.h"

class TServerOptions;

/**
 * Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServer {
 public:
  virtual ~TServer() {}
  virtual void run() = 0;

 protected:
  TServer(TDispatcher* dispatcher, TServerOptions* options) :
    dispatcher_(dispatcher), options_(options) {}
    
  TDispatcher* dispatcher_;
  TServerOptions* options_;
};

/**
 * Class to encapsulate all generic server options.
 */
class TServerOptions {
 public:
  // TODO(mcslee): Fill in getters/setters here
 protected:
  // TODO(mcslee): Fill data members in here
};

#endif
