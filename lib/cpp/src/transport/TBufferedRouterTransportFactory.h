#ifndef _THRIFT_TRANSPORT_TBUFFEREDROUTERTRANSPORTFACTORY_H_
#define _THRIFT_TRANSPORT_TBUFFEREDROUTERTRANSPORTFACTORY_H_ 1

#include <transport/TTransportFactory.h>
#include <transport/TBufferedRouterTransport.h>
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

/**
 * Wraps a transport into a bufferedRouter instance.
 *
 * @author Aditya Agarwal <aditya@facebook.com>
 */
class TBufferedRouterTransportFactory : public TTransportFactory {
 public:
  TBufferedRouterTransportFactory(boost::shared_ptr<TTransport> rTrans): rTrans_(rTrans) {}

  virtual ~TBufferedRouterTransportFactory() {}

  /**
   * Wraps the transport into a buffered one.
   */
  virtual std::pair<boost::shared_ptr<TTransport>, boost::shared_ptr<TTransport> > getIOTransports(boost::shared_ptr<TTransport> trans) {
    boost::shared_ptr<TTransport> buffered(new TBufferedRouterTransport(trans, rTrans_));
    return std::make_pair(buffered, buffered);
  }

 private:
  boost::shared_ptr<TTransport> rTrans_;
};

}}}

#endif // #ifndef _THRIFT_TRANSPORT_TBUFFEREDROUTERTRANSPORTFACTORY_H_
