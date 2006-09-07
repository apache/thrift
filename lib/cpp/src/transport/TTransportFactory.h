#ifndef _THRIFT_TRANSPORT_TTRANSPORTFACTORY_H_
#define _THRIFT_TRANSPORT_TTRANSPORTFACTORY_H_ 1

#include <transport/TTransport.h>
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

/**
 * Generic factory class to make an input and output transport out of a
 * source transport. Commonly used inside servers to make input and output
 * streams out of raw clients.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TTransportFactory {
 public:
  TTransportFactory() {}

  virtual ~TTransportFactory() {}

  /**
   * Default implementation does nothing, just returns the transport given.
   */
  virtual std::pair<boost::shared_ptr<TTransport>, boost::shared_ptr<TTransport> > getIOTransports(boost::shared_ptr<TTransport> trans) {
    return std::make_pair(trans, trans);
  }

};

}}}

#endif // #ifndef _THRIFT_TRANSPORT_TTRANSPORTFACTORY_H_
