// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <cerrno>
#include <exception>

#include <transport/TFDTransport.h>

#include <unistd.h>

using namespace std;

namespace apache { namespace thrift { namespace transport {

void TFDTransport::close() {
  if (!isOpen()) {
    return;
  }

  int rv = ::close(fd_);
  int errno_copy = errno;
  fd_ = -1;
  // Have to check uncaught_exception because this is called in the destructor.
  if (rv < 0 && !std::uncaught_exception()) {
    throw TTransportException(TTransportException::UNKNOWN,
                              "TFDTransport::close()",
                              errno_copy);
  }
}

uint32_t TFDTransport::read(uint8_t* buf, uint32_t len) {
  ssize_t rv = ::read(fd_, buf, len);
  if (rv < 0) {
    int errno_copy = errno;
    throw TTransportException(TTransportException::UNKNOWN,
                              "TFDTransport::read()",
                              errno_copy);
  }
  return rv;
}

void TFDTransport::write(const uint8_t* buf, uint32_t len) {
  while (len > 0) {
    ssize_t rv = ::write(fd_, buf, len);

    if (rv < 0) {
      int errno_copy = errno;
      throw TTransportException(TTransportException::UNKNOWN,
                                "TFDTransport::write()",
                                errno_copy);
    } else if (rv == 0) {
      throw TTransportException(TTransportException::END_OF_FILE,
                                "TFDTransport::write()");
    }

    buf += rv;
    len -= rv;
  }
}

}}} // apache::thrift::transport
