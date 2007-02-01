#ifndef _THRIFT_TRANSPORT_TBUFFEREDROUTERTRANSPORT_H_
#define _THRIFT_TRANSPORT_TBUFFEREDROUTERTRANSPORT_H_ 1

#include "TTransport.h"
#include "Thrift.h"
#include <string>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 
                         
using namespace boost;

/**
 * BufferedRouterTransport. Funcationally equivalent to TBufferedTransport
 * but routes the request to another Transport (typical use case is to route
 * the request to TFileTransport to store the request on disk). The
 * underlying buffer expands to a keep a copy of the entire request/response.
 *
 * @author Aditya Agarwal <aditya@facebook.com>
 */
class TBufferedRouterTransport : public TTransport {
 public:
  TBufferedRouterTransport(shared_ptr<TTransport> trans, shared_ptr<TTransport> rtrans) :
    trans_(trans),
    rtrans_(rtrans),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(512), wLen_(0) {

    rBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * rBufSize_);
    wBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * wBufSize_);
  }
    
  TBufferedRouterTransport(shared_ptr<TTransport> trans, shared_ptr<TTransport> rtrans, uint32_t sz) :
    trans_(trans),
    rtrans_(rtrans),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(sz), wLen_(0) {

    rBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * rBufSize_);
    wBuf_ = (uint8_t*) malloc(sizeof(uint8_t) * wBufSize_);
  }

  ~TBufferedRouterTransport() {
    free(rBuf_);
    free(wBuf_);
  }

  bool isOpen() {
    return trans_->isOpen();
  }
  
  bool peek() {    
    if (rPos_ >= rLen_) {
      // Double the size of the underlying buffer if it is full
      if (rLen_ == rBufSize_) {
        rBufSize_ *=2;
        rBuf_ = (uint8_t *)realloc(rBuf_, sizeof(uint8_t) * rBufSize_);
      }
    
      // try to fill up the buffer
      rLen_ += trans_->read(rBuf_+rPos_, rBufSize_ - rPos_);
    }
    return (rLen_ > rPos_);
  }


  void open() {
    trans_->open();
  }

  void close() {
    trans_->close();
  }
  
  uint32_t read(uint8_t* buf, uint32_t len);

  void readEnd() {
    rtrans_->write(rBuf_, rLen_);

    // reset state
    rLen_ = 0;
    rPos_ = 0;
  }

  void write(const uint8_t* buf, uint32_t len);

  void flush();

 protected:
  shared_ptr<TTransport> trans_;
  shared_ptr<TTransport> rtrans_;

  uint8_t* rBuf_;
  uint32_t rBufSize_;
  uint32_t rPos_;
  uint32_t rLen_;

  uint8_t* wBuf_;
  uint32_t wBufSize_;
  uint32_t wLen_;
};


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
  virtual boost::shared_ptr<TTransport> getTransport(boost::shared_ptr<TTransport> trans) {
    return boost::shared_ptr<TTransport>(new TBufferedRouterTransport(trans, rTrans_));
  }

 private:
  boost::shared_ptr<TTransport> rTrans_;
};

}}} // facebook::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TBUFFEREDROUTERTRANSPORT_H_
