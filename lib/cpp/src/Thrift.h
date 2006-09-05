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

namespace facebook { namespace thrift {

class Exception : public std::exception {
private:
  const std::string _message;

public:
  Exception(const std::string message) : _message(message) {}
  ~Exception() throw () {}
  const char* what() {return _message.c_str();}
};

}} // facebook::thrift

#endif // #ifndef _THRIFT_THRIFT_H_
