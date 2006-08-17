#ifndef T_SERVER_H
#define T_SERVER_H

#include <TProcessor.h>
#include <concurrency/Thread.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace server { 

using namespace facebook::thrift;
using namespace boost;

class TServerOptions;

/**
 * Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServer : public concurrency::Runnable {
public:
  virtual ~TServer() {}
  virtual void run() = 0;
  
protected:
  TServer(shared_ptr<TProcessor> processor, shared_ptr<TServerOptions> options) :
    processor_(processor), options_(options) {}
  
  shared_ptr<TProcessor> processor_;
  shared_ptr<TServerOptions> options_;
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
