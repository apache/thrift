// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_TRANSPORT_TSHORTREADTRANSPORT_H_
#define _THRIFT_TRANSPORT_TSHORTREADTRANSPORT_H_ 1

#include <cstdlib>

#include <transport/TTransport.h>

namespace apache { namespace thrift { namespace transport { namespace test {

/**
 * This class is only meant for testing.  It wraps another transport.
 * Calls to read are passed through with some probability.  Otherwise,
 * the read amount is randomly reduced before being passed through.
 *
 */
class TShortReadTransport : public TTransport {
 public:
  TShortReadTransport(boost::shared_ptr<TTransport> transport, double full_prob)
    : transport_(transport)
    , fullProb_(full_prob)
  {}

  bool isOpen() {
    return transport_->isOpen();
  }

  bool peek() {
    return transport_->peek();
  }

  void open() {
    transport_->open();
  }

  void close() {
    transport_->close();
  }

  uint32_t read(uint8_t* buf, uint32_t len) {
    if (len == 0) {
      return 0;
    }

    if (rand()/(double)RAND_MAX >= fullProb_) {
      len = 1 + rand()%len;
    }
    return transport_->read(buf, len);
  }

  void write(const uint8_t* buf, uint32_t len) {
    transport_->write(buf, len);
  }

  void flush() {
    transport_->flush();
  }

  const uint8_t* borrow(uint8_t* buf, uint32_t* len) {
    return transport_->borrow(buf, len);
  }

  void consume(uint32_t len) {
    return transport_->consume(len);
  }

  boost::shared_ptr<TTransport> getUnderlyingTransport() {
    return transport_;
  }

 protected:
  boost::shared_ptr<TTransport> transport_;
  double fullProb_;
};

}}}} // apache::thrift::transport::test

#endif // #ifndef _THRIFT_TRANSPORT_TSHORTREADTRANSPORT_H_
