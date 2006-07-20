#if !defined(_concurrency_Exception_h_)
#define _concurrency_Exception_h_ 1

#include <exception>

namespace facebook { namespace thrift { namespace concurrency { 

class NoSuchTaskException : public std::exception {};

class UncancellableTaskException : public std::exception {};

class InvalidArgumentException : public std::exception {};

class IllegalStateException : public std::exception {};

class TimedOutException : public std::exception {};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_Exception_h_)
