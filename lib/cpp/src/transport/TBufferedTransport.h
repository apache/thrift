#ifndef _THRIFT_TRANSPORT_TBUFFEREDTRANSPORT_H_
#define _THRIFT_TRANSPORT_TBUFFEREDTRANSPORT_H_ 1

#include "TTransport.h"
#include <string>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

using namespace boost;

/**
 * Buffered transport. For reads it will read more data than is requested
 * and will serve future data out of a local buffer. For writes, data is
 * stored to an in memory buffer before being written out.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBufferedTransport : public TTransport {
 public:
  TBufferedTransport(shared_ptr<TTransport> transport) :
    transport_(transport),
    rBufSize_(512), rPos_(0), rLen_(0),
    wBufSize_(512), wLen_(0) {
    rBuf_ = new uint8_t[rBufSize_];
    wBuf_ = new uint8_t[wBufSize_];
  }

  TBufferedTransport(shared_ptr<TTransport> transport, uint32_t sz) :
    transport_(transport),
    rBufSize_(sz), rPos_(0), rLen_(0),
    wBufSize_(sz), wLen_(0) {
    rBuf_ = new uint8_t[rBufSize_];
    wBuf_ = new uint8_t[wBufSize_];
  }

  TBufferedTransport(shared_ptr<TTransport> transport, uint32_t rsz, uint32_t wsz) :
    transport_(transport),
    rBufSize_(rsz), rPos_(0), rLen_(0),
    wBufSize_(wsz), wLen_(0) {
    rBuf_ = new uint8_t[rBufSize_];
    wBuf_ = new uint8_t[wBufSize_];
  }

  ~TBufferedTransport() {
    delete [] rBuf_;
    delete [] wBuf_;
  }

  bool isOpen() {
    return transport_->isOpen();
  }
  
  void open() {
    transport_->open();
  }

  void close() {
    transport_->close();
  }

  uint32_t readAll(uint8_t* buf, uint32_t len) {
    return transport_->readAll(buf, len);
  }
  
  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

  void flush();

 protected:
  shared_ptr<TTransport> transport_;
  uint8_t* rBuf_;
  uint32_t rBufSize_;
  uint32_t rPos_;
  uint32_t rLen_;

  uint8_t* wBuf_;
  uint32_t wBufSize_;
  uint32_t wLen_;
};

}}} // facebook::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TBUFFEREDTRANSPORT_H_
