#ifndef _THRIFT_CONCURRENCY_EXCEPTION_H_
#define _THRIFT_CONCURRENCY_EXCEPTION_H_ 1

#include <exception>

namespace facebook { namespace thrift { namespace concurrency { 

class NoSuchTaskException : public std::exception {};

class UncancellableTaskException : public std::exception {};

class InvalidArgumentException : public std::exception {};

class IllegalStateException : public std::exception {};

class TimedOutException : public std::exception {};

}}} // facebook::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_EXCEPTION_H_
