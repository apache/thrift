#ifndef _THRIFT_TRANSPORT_TFRAMEDTRANSPORT_H_
#define _THRIFT_TRANSPORT_TFRAMEDTRANSPORT_H_ 1

#include "TTransport.h"
#include <string>
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

using namespace boost;

/**
 * Framed transport. All writes go into an in-memory buffer until flush is
 * called, at which point the transport writes the length of the entire
 * binary chunk followed by the data payload. This allows the receiver on the
 * other end to always do fixed-length reads.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TFramedTransport : public TTransport {
 public:
  TFramedTransport(shared_ptr<TTransport> transport) :
    transport_(transport),
    rPos_(0), rLen_(0),
    wBufSize_(512), wLen_(0) {
    rBuf_ = NULL;
    wBuf_ = new uint8_t[wBufSize_];
  }

  TFramedTransport(shared_ptr<TTransport> transport, uint32_t sz) :
    transport_(transport),
    rPos_(0), rLen_(0),
    wBufSize_(sz), wLen_(0) {
    rBuf_ = NULL;
    wBuf_ = new uint8_t[wBufSize_];
  }

  ~TFramedTransport() {
    if (rBuf_ != NULL) {
      delete [] rBuf_;
    }
    if (wBuf_ != NULL) {
      delete [] wBuf_;
    }
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
  
  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

  void flush();

 protected:
  shared_ptr<TTransport> transport_;
  uint8_t* rBuf_;
  uint32_t rPos_;
  uint32_t rLen_;

  uint8_t* wBuf_;
  uint32_t wBufSize_;
  uint32_t wLen_;

  /**
   * Reads a frame of input from the underlying stream.
   */
  void readFrame();
};

}}} // facebook::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TFRAMEDTRANSPORT_H_
