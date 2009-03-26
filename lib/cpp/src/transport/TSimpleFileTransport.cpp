// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "TSimpleFileTransport.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

namespace apache { namespace thrift { namespace transport {

TSimpleFileTransport::
TSimpleFileTransport(const std::string& path, bool read, bool write)
    : TFDTransport(-1, TFDTransport::CLOSE_ON_DESTROY) {
  int flags = 0;
  if (read && write) {
    flags = O_RDWR;
  } else if (read) {
    flags = O_RDONLY;
  } else if (write) {
    flags = O_WRONLY;
  } else {
    throw TTransportException("Neither READ nor WRITE specified");
  }
  if (write) {
    flags |= O_CREAT | O_APPEND;
  }
  int fd = ::open(path.c_str(),
                  flags,
                  S_IRUSR | S_IWUSR| S_IRGRP | S_IROTH);
  if (fd < 0) {
    throw TTransportException("failed to open file for writing: " + path);
  }
  setFD(fd);
  open();
}

}}} // apache::thrift::transport
