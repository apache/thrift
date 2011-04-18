/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#ifndef _THRIFT_THRIFT_H_
#define _THRIFT_THRIFT_H_ 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#include <assert.h>

#include <sys/types.h>
#include <netinet/in.h>
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#include <string>
#include <map>
#include <list>
#include <set>
#include <vector>
#include <exception>
#include <typeinfo>

#include "TLogging.h"

namespace apache { namespace thrift {

class TEnumIterator : public std::iterator<std::forward_iterator_tag, std::pair<int, const char*> > {
 public:
  TEnumIterator(int n,
                int* enums,
                const char** names) :
      ii_(0), n_(n), enums_(enums), names_(names) {
  }

  int operator ++() {
    return ++ii_;
  }

  bool operator !=(const TEnumIterator& end) {
    assert(end.n_ == -1);
    return (ii_ != n_);
  }

  std::pair<int, const char*> operator*() const {
    return std::make_pair(enums_[ii_], names_[ii_]);
  }

 private:
  int ii_;
  const int n_;
  int* enums_;
  const char** names_;
};

class TOutput {
 public:
  TOutput() : f_(&errorTimeWrapper) {}

  inline void setOutputFunction(void (*function)(const char *)){
    f_ = function;
  }

  inline void operator()(const char *message){
    f_(message);
  }

  // It is important to have a const char* overload here instead of
  // just the string version, otherwise errno could be corrupted
  // if there is some problem allocating memory when constructing
  // the string.
  void perror(const char *message, int errno_copy);
  inline void perror(const std::string &message, int errno_copy) {
    perror(message.c_str(), errno_copy);
  }

  void printf(const char *message, ...);

  inline static void errorTimeWrapper(const char* msg) {
    time_t now;
    char dbgtime[26];
    time(&now);
    ctime_r(&now, dbgtime);
    dbgtime[24] = 0;
    fprintf(stderr, "Thrift: %s %s\n", dbgtime, msg);
  }

  /** Just like strerror_r but returns a C++ string object. */
  static std::string strerror_s(int errno_copy);

 private:
  void (*f_)(const char *);
};

extern TOutput GlobalOutput;

class TException : public std::exception {
 public:
  TException():
    message_() {}

  TException(const std::string& message) :
    message_(message) {}

  virtual ~TException() throw() {}

  virtual const char* what() const throw() {
    if (message_.empty()) {
      return "Default TException.";
    } else {
      return message_.c_str();
    }
  }

 protected:
  std::string message_;

};


// Forward declare this structure used by TDenseProtocol
namespace reflection { namespace local {
struct TypeSpec;
}}

class TDelayedException {
 public:
  template <class E> static TDelayedException* delayException(const E& e);
  virtual void throw_it() = 0;
  virtual ~TDelayedException() {};
};

template <class E> class TExceptionWrapper : public TDelayedException {
 public:
  TExceptionWrapper(const E& e) : e_(e) {}
  virtual void throw_it() {
    E temp(e_);
    delete this;
    throw temp;
  }
 private:
  E e_;
};

template <class E>
TDelayedException* TDelayedException::delayException(const E& e) {
  return new TExceptionWrapper<E>(e);
}

#if T_GLOBAL_DEBUG_VIRTUAL > 1
void profile_virtual_call(const std::type_info& info);
void profile_generic_protocol(const std::type_info& template_type,
                              const std::type_info& prot_type);
void profile_print_info(FILE *f);
void profile_print_info();
void profile_write_pprof(FILE* gen_calls_f, FILE* virtual_calls_f);
#endif

}} // apache::thrift

#endif // #ifndef _THRIFT_THRIFT_H_
