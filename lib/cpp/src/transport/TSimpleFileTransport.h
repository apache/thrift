// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TRANSPORT_TSIMPLEFILETRANSPORT_H_
#define _THRIFT_TRANSPORT_TSIMPLEFILETRANSPORT_H_ 1

#include "TFDTransport.h"

namespace apache { namespace thrift { namespace transport {

/**
 * Dead-simple wrapper around a file.
 *
 * Writeable files are opened with O_CREAT and O_APPEND
 */
class TSimpleFileTransport : public TFDTransport {
 public:
  TSimpleFileTransport(const std::string& path,
                       bool read =  true,
                       bool write = false);
};

}}} // apache::thrift::transport

#endif //  _THRIFT_TRANSPORT_TSIMPLEFILETRANSPORT_H_
