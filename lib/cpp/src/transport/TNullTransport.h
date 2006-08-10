#ifndef T_NULL_TRANSPORT
#define T_NULL_TRANSPORT

#include "TTransport.h"

namespace facebook { namespace thrift { namespace transport { 

/**
 * The null transport is a dummy transport that doesn't actually do anything.
 * It's sort of an analogy to /dev/null, you can never read anything from it
 * and it will let you write anything you want to it, though it won't actually
 * go anywhere.
 * 
 * @author Mark Slee <mcslee@facebook.com>
 */
class TNullTransport : public TTransport {
 public:
  TNullTransport() {}
  ~TNullTransport() {}

  bool isOpen() { return true; }
  void open() { }
  void write(const std::string& s) {}
};

}}} // facebook::thrift::transport

#endif
