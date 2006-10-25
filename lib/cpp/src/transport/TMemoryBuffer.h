#ifndef _THRIFT_TRANSPORT_TMEMORYBUFFER_H_
#define _THRIFT_TRANSPORT_TMEMORYBUFFER_H_ 1

#include "TTransport.h"
#include <string>

namespace facebook { namespace thrift { namespace transport { 

/**
 * A memory buffer is a tranpsort that simply reads from and writes to an
 * in memory buffer. Anytime you call write on it, the data is simply placed
 * into a buffer, and anytime you call read, data is read from that buffer.
 *
 * The buffers are allocated using C constructs malloc,realloc, and the size
 * doubles as necessary.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TMemoryBuffer : public TTransport {
 public:
  TMemoryBuffer() {
    owner_ = true;
    bufferSize_ = 1024;
    buffer_ = (uint8_t*)malloc(bufferSize_);
    if (buffer_ == NULL) {
      throw TTransportException("Out of memory");
    }
    wPos_ = 0;
    rPos_ = 0;
  }

  TMemoryBuffer(uint32_t sz) {
    owner_ = true;
    bufferSize_ = sz;
    buffer_ = (uint8_t*)malloc(bufferSize_);
    if (buffer_ == NULL) {
      throw TTransportException("Out of memory");
    }
    wPos_ = 0;
    rPos_ = 0;
  }

  TMemoryBuffer(uint8_t* buf, int sz) {
    owner_ = false;
    buffer_ = buf;
    bufferSize_ = sz;
    wPos_ = sz;
    rPos_ = 0;
  }

  ~TMemoryBuffer() {
    if (owner_) {
      if (buffer_ != NULL) {
        free(buffer_);
        buffer_ = NULL;
      }
    }
  }

  bool isOpen() {
    return true;
  }

 
  void open() {}

  void close() {}

  void getBuffer(uint8_t** bufPtr, uint32_t* sz) {
    *bufPtr = buffer_;
    *sz = wPos_;
  }

  void resetBuffer() {
    wPos_ = 0;
    rPos_ = 0;
  }

  void resetBuffer(uint8_t* buf, uint32_t sz) {
    if (owner_) {
      if (buffer_ != NULL) {
        free(buffer_);
      }
    }
    owner_ = false;
    buffer_ = buf;
    bufferSize_ = sz;
    wPos_ = sz;
    rPos_ = 0;
  }

  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

 private:
  // Data buffer
  uint8_t* buffer_;
  
  // Allocated buffer size
  uint32_t bufferSize_;

  // Where the write is at
  uint32_t wPos_;
  
  // Where the reader is at
  uint32_t rPos_;

  // Is this object the owner of the buffer?
  bool owner_;

};

}}} // facebook::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TBUFFEREDTRANSPORT_H_
