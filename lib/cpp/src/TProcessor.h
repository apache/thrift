// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TPROCESSOR_H_
#define _THRIFT_TPROCESSOR_H_ 1

#include <string>
#include <protocol/TProtocol.h>
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { 

using namespace boost;

using namespace facebook::thrift::protocol;

/**
 * A processor is a generic object that acts upon two streams of data, one
 * an input and the other an output. The definition of this object is loose,
 * though the typical case is for some sort of server that either generates
 * responses to an input stream or forwards data from one pipe onto another.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TProcessor {
 public:
  virtual ~TProcessor() {}

  virtual bool process(shared_ptr<TProtocol> in,
                       shared_ptr<TProtocol> out) = 0;

  bool process(shared_ptr<TProtocol> io) {
    return process(io, io);
  }

 protected:
  TProcessor() {}
};

}} // facebook::thrift

#endif // #ifndef _THRIFT_PROCESSOR_H_
