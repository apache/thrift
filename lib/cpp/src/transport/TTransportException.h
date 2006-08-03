#ifndef T_TRANSPORT_EXCEPTION_H
#define T_TRANSPORT_EXCEPTION_H

#include <string>

namespace facebook { namespace thrift { namespace transport { 

/**
 * Error codes for the various types of exceptions.
 */
enum TTransportExceptionType {
  TTX_UNKNOWN = 0,
  TTX_NOT_OPEN = 1,
  TTX_TIMED_OUT = 2,
};

/**
 * Class to encapsulate all the possible types of transport errors that may
 * occur in various transport systems. This provides a sort of generic
 * wrapper around the shitty UNIX E_ error codes that lets a common code
 * base of error handling to be used for various types of transports, i.e.
 * pipes etc.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TTransportException {
 public:
  TTransportException() :
    type_(TTX_UNKNOWN), message_() {}

  TTransportException(TTransportExceptionType type) :
    type_(type), message_() {}

  TTransportException(std::string message) :
    type_(TTX_UNKNOWN), message_(message) {}

  TTransportException(TTransportExceptionType type, std::string message) :
    type_(type), message_(message) {}

  ~TTransportException() {}

  /**
   * Returns an error code that provides information about the type of error
   * that has occurred.
   *
   * @return Error code
   */
  TTransportExceptionType getType() { return type_; }
 
  /**
   * Returns an informative message about what caused this error.
   *
   * @return Error string
   */
  const std::string& getMessage() { return message_; }

 protected:
  /** Error code */
  TTransportExceptionType type_;

  /** Description */
  std::string message_;
};

}}} // facebook::thrift::transport

#endif
