// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_CONCURRENCY_EXCEPTION_H_
#define _THRIFT_CONCURRENCY_EXCEPTION_H_ 1

#include <exception>
#include <Thrift.h>

namespace apache { namespace thrift { namespace concurrency {

class NoSuchTaskException : public apache::thrift::TException {};

class UncancellableTaskException : public apache::thrift::TException {};

class InvalidArgumentException : public apache::thrift::TException {};

class IllegalStateException : public apache::thrift::TException {};

class TimedOutException : public apache::thrift::TException {
public:
  TimedOutException():TException("TimedOutException"){};
  TimedOutException(const std::string& message ) :
    TException(message) {}
};

class TooManyPendingTasksException : public apache::thrift::TException {
public:
  TooManyPendingTasksException():TException("TooManyPendingTasksException"){};
  TooManyPendingTasksException(const std::string& message ) :
    TException(message) {}
};

class SystemResourceException : public apache::thrift::TException {
public:
    SystemResourceException() {}

    SystemResourceException(const std::string& message) :
        TException(message) {}
};

}}} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_EXCEPTION_H_
