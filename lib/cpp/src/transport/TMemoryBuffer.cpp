#include "TMemoryBuffer.h"

namespace facebook { namespace thrift { namespace transport { 

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
