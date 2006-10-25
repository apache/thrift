#include "TBufferedTransport.h"
using std::string;

namespace facebook { namespace thrift { namespace transport { 

uint32_t TBufferedTransport::read(uint8_t* buf, uint32_t len) {
  uint32_t need = len;

  // We don't have enough data yet
  if (rLen_-rPos_ < need) {
    // Copy out whatever we have
    if (rLen_-rPos_ > 0) {
      memcpy(buf, rBuf_+rPos_, rLen_-rPos_);
      need -= rLen_-rPos_;
      buf += rLen_-rPos_;
    }    
    // Get more from underlying transport up to buffer size
    // TODO: should this be a readAll?
    rLen_ = transport_->read(rBuf_, rBufSize_);
    rPos_ = 0;
  }
  
  // Hand over whatever we have
  uint32_t give = need;
  if (rLen_-rPos_ < give) {
    give = rLen_-rPos_;
  }
  memcpy(buf, rBuf_+rPos_, give);
  rPos_ += give;
  need -= give;
  return (len - need);
}

void TBufferedTransport::write(const uint8_t* buf, uint32_t len) {
  if (len == 0) {
    return;
  }

  if (len + wLen_ >= wBufSize_) {
    uint32_t copy = wBufSize_ - wLen_;
    memcpy(wBuf_ + wLen_, buf, copy);
    transport_->write(wBuf_, wBufSize_);
    
    wLen_ = len - copy;
    if (wLen_ > 0) {
      memcpy(wBuf_, buf+copy, wLen_);
    }
  } else {
    memcpy(wBuf_+wLen_, buf, len);
    wLen_ += len;
  }
}

void TBufferedTransport::flush()  {
  // Write out any data waiting in the write buffer
  if (wLen_ > 0) {
    transport_->write(wBuf_, wLen_);
    wLen_ = 0;
  }

  // Flush the underlying transport
  transport_->flush();
}

}}} // facebook::thrift::transport
