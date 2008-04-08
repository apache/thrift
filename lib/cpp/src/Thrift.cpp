// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <Thrift.h>
#include <protocol/TProtocol.h>

namespace facebook { namespace thrift {

TOutput GlobalOutput;

std::string TOutput::strerror_s(int errno_copy) {
#ifndef HAVE_STRERROR_R
  return "errno = " + lexical_cast<string>(errno_copy);
#else  // HAVE_STRERROR_R

  char b_errbuf[1024] = { '\0' };
#ifdef STRERROR_R_CHAR_P
  char *b_error = strerror_r(errno_copy, b_errbuf, sizeof(b_errbuf));
#else
  char *b_error = b_errbuf;
  int rv = strerror_r(errno_copy, b_errbuf, sizeof(b_errbuf));
  if (rv == -1) {
    // strerror_r failed.  omgwtfbbq.
    return "XSI-compliant strerror_r() failed with errno = " +
      lexical_cast<string>(errno_copy);
  }
#endif
  // Can anyone prove that explicit cast is probably not necessary
  // to ensure that the string object is constructed before
  // b_error becomes invalid?
  return std::string(b_error);

#endif  // HAVE_STRERROR_R
}

uint32_t TApplicationException::read(facebook::thrift::protocol::TProtocol* iprot) {
  uint32_t xfer = 0;
  std::string fname;
  facebook::thrift::protocol::TType ftype;
  int16_t fid;

  xfer += iprot->readStructBegin(fname);

  while (true) {
    xfer += iprot->readFieldBegin(fname, ftype, fid);
    if (ftype == facebook::thrift::protocol::T_STOP) {
      break;
    }
    switch (fid) {
    case 1:
      if (ftype == facebook::thrift::protocol::T_STRING) {
        xfer += iprot->readString(message_);
      } else {
        xfer += iprot->skip(ftype);
      }
      break;
    case 2:
      if (ftype == facebook::thrift::protocol::T_I32) {
        int32_t type;
        xfer += iprot->readI32(type);
        type_ = (TApplicationExceptionType)type;
      } else {
        xfer += iprot->skip(ftype);
      }
      break;
    default:
      xfer += iprot->skip(ftype);
      break;
    }
    xfer += iprot->readFieldEnd();
  }

  xfer += iprot->readStructEnd();
  return xfer;
}

uint32_t TApplicationException::write(facebook::thrift::protocol::TProtocol* oprot) const {
  uint32_t xfer = 0;
  xfer += oprot->writeStructBegin("TApplicationException");
  xfer += oprot->writeFieldBegin("message", facebook::thrift::protocol::T_STRING, 1);
  xfer += oprot->writeString(message_);
  xfer += oprot->writeFieldEnd();
  xfer += oprot->writeFieldBegin("type", facebook::thrift::protocol::T_I32, 2);
  xfer += oprot->writeI32(type_);
  xfer += oprot->writeFieldEnd();
  xfer += oprot->writeFieldStop();
  xfer += oprot->writeStructEnd();
  return xfer;
}

}} // facebook::thrift
