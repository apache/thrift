#ifndef T_SERVER_H
#define T_SERVER_H

#include "TProcessor.h"

namespace facebook { namespace thrift { namespace server { 

using namespace facebook::thrift;

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
  TServer(TProcessor* processor, TServerOptions* options) :
    processor_(processor), options_(options) {}
    
  TProcessor* processor_;
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

}}} // facebook::thrift::server

#endif
