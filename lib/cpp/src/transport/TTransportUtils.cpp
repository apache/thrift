#include <transport/TTransportUtils.h>

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

uint32_t TMemoryBuffer::read(uint8_t* buf, uint32_t len) {
  // Check avaible data for reading
  uint32_t avail = wPos_ - rPos_;

  // Device how much to give
  uint32_t give = len;
  if (avail < len) {
    give = avail;
  }

  // Copy into buffer and increment rPos_
  memcpy(buf, buffer_ + rPos_, give);
  rPos_ += give;
  
  return give;
}

void TMemoryBuffer::write(const uint8_t* buf, uint32_t len) {
  // Check available space
  uint32_t avail = bufferSize_ - wPos_;

  // Grow the buffer
  if (len > avail) {
    if (!owner_) {
      throw TTransportException("Insufficient space in external MemoryBuffer");
    }
    while (len > avail) {
      bufferSize_ *= 2;
      buffer_ = (uint8_t*)realloc(buffer_, bufferSize_);
      if (buffer_ == NULL) {
        throw TTransportException("Out of memory.");
      }
    }
  }

  // Copy into the buffer and increment wPos_
  memcpy(buffer_ + wPos_, buf, len);
  wPos_ += len;
}

}}} // facebook::thrift::transport
