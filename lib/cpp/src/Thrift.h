#ifndef THRIFT_H
#define THRIFT_H

#include <netinet/in.h>
#include <inttypes.h>
#include <string>
#include <map>
#include <list>
#include <set>
#include <exception>

namespace facebook {namespace thrift {

class Exception : public std::exception {
private:
  const std::string _message;

public:
  Exception(const std::string message) : _message(message) {}
  ~Exception() throw () {}
  const char* what() {return _message.c_str();}
};

}} // facebook::thrift

#endif
