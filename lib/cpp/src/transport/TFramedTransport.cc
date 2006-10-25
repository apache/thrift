#include <transport/TFramedTransport.h>
#include <netinet/in.h>

using std::string;

namespace facebook { namespace thrift { namespace transport { 

uint32_t TFramedTransport::read(uint8_t* buf, uint32_t len) {
  if (!read_) {
    return transport_->read(buf, len);
  }

  uint32_t need = len;

  // We don't have enough data yet
  if (rLen_-rPos_ < need) {
    // Copy out whatever we have
    if (rLen_-rPos_ > 0) {
      memcpy(buf, rBuf_+rPos_, rLen_-rPos_);
      need -= rLen_-rPos_;
      buf += rLen_-rPos_;
    }

    // Read another chunk
    readFrame();
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

void TFramedTransport::readFrame() {
  // Get rid of the old frame
  if (rBuf_ != NULL) {
    delete [] rBuf_;
    rBuf_ = NULL;
  }

  // Read in the next chunk size
  int32_t sz;
  transport_->readAll((uint8_t*)&sz, 4);
  sz = (int32_t)ntohl(sz);

  if (sz < 0) {
    throw new TTransportException("Frame size has negative value");
  }

  // Read the frame payload, reset markers
  rBuf_ = new uint8_t[sz];
  transport_->readAll(rBuf_, sz);
  rPos_ = 0;
  rLen_ = sz;
}

void TFramedTransport::write(const uint8_t* buf, uint32_t len) {
  if (len == 0) {
    return;
  }

  // Shortcut out if not write mode
  if (!write_) {
    transport_->write(buf, len);
    return;
  }

  // Need to grow the buffer
  if (len + wLen_ >= wBufSize_) {

    // Double buffer size until sufficient
    while (wBufSize_ < len + wLen_) {
      wBufSize_ *= 2;
    }

    // Allocate new buffer
    uint8_t* wBuf2 = new uint8_t[wBufSize_];

    // Copy the old buffer to the new one
    memcpy(wBuf2, wBuf_, wLen_);
   
    // Now point buf to the new one
    delete [] wBuf_;
    wBuf_ = wBuf2;
  }

  // Copy data into buffer
  memcpy(wBuf_ + wLen_, buf, len);
  wLen_ += len;
}

void TFramedTransport::flush()  {
  if (!write_) {
    transport_->flush();
    return;
  }

  // Write frame size
  int32_t sz = wLen_;
  sz = (int32_t)htonl(sz);

  transport_->write((const uint8_t*)&sz, 4);
  
  // Write frame body
  if (sz > 0) {
    transport_->write(wBuf_, wLen_);
  }

  // All done
  wLen_ = 0;

  // Flush the underlying
  transport_->flush();
}

}}} // facebook::thrift::transport
