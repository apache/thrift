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

class Exception : public std::exception {
public:
  Exception(const std::string message) :
    message_(message) {}

  ~Exception() throw () {}

  const char* what() {
    return message_.c_str();
  }

private:
  const std::string message_;

};

}} // facebook::thrift

#endif // #ifndef _THRIFT_THRIFT_H_
