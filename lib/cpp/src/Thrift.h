#ifndef _THRIFT_THRIFT_H_
#define _THRIFT_THRIFT_H_ 1

#include <netinet/in.h>
#include <inttypes.h>
#include <string>
#include <map>
#include <list>
#include <set>
#include <vector>
#include <exception>

#include "TLogging.h"

namespace facebook { namespace thrift {

class TException : public std::exception {
public:
  TException() {}

  TException(const std::string message) :
    message_(message) {}

  virtual ~TException() throw() {}

  const char* what() const throw() {
    if (message_.empty()) {
      return "Default TException.";
    } else {
      return message_.c_str();
    }
  }

private:
  std::string message_;

};

}} // facebook::thrift

#endif // #ifndef _THRIFT_THRIFT_H_
