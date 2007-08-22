// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <transport/TTransportException.h>
#include <boost/lexical_cast.hpp>
#include <cstring>
#include <config.h>

using std::string;
using boost::lexical_cast;

namespace facebook { namespace thrift { namespace transport { 

string TTransportException::strerror_s(int errno_copy) {
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
  return string(b_error);

#endif  // HAVE_STRERROR_R
}

}}} // facebook::thrift::transport
