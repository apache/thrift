// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TRANSPORT_TFDTRANSPORT_H_
#define _THRIFT_TRANSPORT_TFDTRANSPORT_H_ 1

#include <string>
#include <sys/time.h>

#include "TTransport.h"
#include "TServerSocket.h"

namespace apache { namespace thrift { namespace transport {

/**
 * Dead-simple wrapper around a file descriptor.
 *
 */
class TFDTransport : public TTransport {
 public:
  enum ClosePolicy
  { NO_CLOSE_ON_DESTROY = 0
  , CLOSE_ON_DESTROY = 1
  };

  TFDTransport(int fd, ClosePolicy close_policy = NO_CLOSE_ON_DESTROY)
    : fd_(fd)
    , close_policy_(close_policy)
  {}

  ~TFDTransport() {
    if (close_policy_ == CLOSE_ON_DESTROY) {
      close();
    }
  }

  bool isOpen() { return fd_ >= 0; }

  void open() {}

  void close();

  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);

  void setFD(int fd) { fd_ = fd; }
  int getFD() { return fd_; }

 protected:
  int fd_;
  ClosePolicy close_policy_;
};

}}} // apache::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TFDTRANSPORT_H_
