#ifndef _THRIFT_TRANSPORT_TBUFFEREDTRANSPORTFACTORY_H_
#define _THRIFT_TRANSPORT_TBUFFEREDTRANSPORTFACTORY_H_ 1

#include <transport/TTransportFactory.h>
#include <transport/TBufferedTransport.h>
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

/**
 * Wraps a transport into a buffered one.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBufferedTransportFactory : public TTransportFactory {
 public:
  TBufferedTransportFactory() {}

  virtual ~TBufferedTransportFactory() {}

  /**
   * Wraps the transport into a buffered one.
   */
  virtual std::pair<boost::shared_ptr<TTransport>, boost::shared_ptr<TTransport> > getIOTransports(boost::shared_ptr<TTransport> trans) {
    boost::shared_ptr<TTransport> buffered(new TBufferedTransport(trans));
    return std::make_pair(buffered, buffered);
  }

};

}}}

#endif // #ifndef _THRIFT_TRANSPORT_TTRANSPORTFACTORY_H_
