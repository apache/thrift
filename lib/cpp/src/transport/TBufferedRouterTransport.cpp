#include "TBufferedRouterTransport.h"
#include "Thrift.h"
using std::string;

namespace facebook { namespace thrift { namespace transport { 

uint32_t TBufferedRouterTransport::read(uint8_t* buf, uint32_t len) {
  uint32_t need = len;
  
  // We don't have enough data yet
  if (rLen_-rPos_ < need) {
    // Copy out whatever we have
    if (rLen_-rPos_ > 0) {
      memcpy(buf, rBuf_+rPos_, rLen_-rPos_);
      need -= rLen_-rPos_;
      buf += rLen_-rPos_;
      rPos_ = rLen_;
    }

    // Double the size of the underlying buffer if it is full
    if (rLen_ == rBufSize_) {
      rBufSize_ *=2;
      rBuf_ = (uint8_t *)realloc(rBuf_, sizeof(uint8_t) * rBufSize_);
    }
    
    // try to fill up the buffer
    rLen_ += trans_->read(rBuf_+rPos_, rBufSize_ - rPos_);
  }


  // Hand over whatever we have
  uint32_t give = need;
  if (rLen_-rPos_ < give) {
    give = rLen_-rPos_;
  }
  if (give > 0) {
    memcpy(buf, rBuf_+rPos_, give);
    rPos_ += give;
    need -= give;
  }

  return (len - need);
}

void TBufferedRouterTransport::write(const uint8_t* buf, uint32_t len) {
  if (len == 0) {
    return;
  }

  if (len + wLen_ >= wBufSize_) {
    uint32_t copy = wBufSize_ - wLen_;
    memcpy(wBuf_ + wLen_, buf, copy);
    trans_->write(wBuf_+wPos_, wBufSize_-wPos_);
    wLen_ += copy;
    wPos_ = wLen_;

    uint32_t left = len-copy;
    if (left > 0) {
      // double the size of the write buffer
      wBuf_ = (uint8_t *)realloc(wBuf_, sizeof(uint8_t) * wBufSize_ * 2);
      memcpy(wBuf_ + wLen_, buf+copy, left);
      wLen_ += left;
      wBufSize_*=2;
    }
  } else {
    memcpy(wBuf_+wLen_, buf, len);
    wLen_ += len;
  }
}

void TBufferedRouterTransport::flush()  {
  // Write out any data waiting in the write buffer
  if (wLen_-wPos_ > 0) {
    trans_->write(wBuf_+wPos_, wLen_-wPos_);
    wPos_ = wLen_;
  }

  // Flush the underlying transport
  trans_->flush();
}

}}} // facebook::thrift::transport
