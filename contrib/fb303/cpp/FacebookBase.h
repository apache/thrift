// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _FACEBOOK_TB303_FACEBOOKBASE_H_
#define _FACEBOOK_TB303_FACEBOOKBASE_H_ 1

#include "FacebookService.h"

#include "server/TServer.h"
#include "concurrency/Mutex.h"

#include <time.h>
#include <string>
#include <map>

namespace facebook { namespace fb303 {

using apache::thrift::concurrency::Mutex;
using apache::thrift::concurrency::ReadWriteMutex;
using apache::thrift::server::TServer;

struct ReadWriteInt : ReadWriteMutex {int64_t value;};
struct ReadWriteCounterMap : ReadWriteMutex,
                             std::map<std::string, ReadWriteInt> {};

/**
 * Base Facebook service implementation in C++.
 *
 */
class FacebookBase : virtual public FacebookServiceIf {
 protected:
  FacebookBase(std::string name);
  virtual ~FacebookBase() {}

 public:
  void getName(std::string& _return);
  virtual void getVersion(std::string& _return) { _return = ""; }

  virtual fb_status getStatus() = 0;
  virtual void getStatusDetails(std::string& _return) { _return = ""; }

  void setOption(const std::string& key, const std::string& value);
  void getOption(std::string& _return, const std::string& key);
  void getOptions(std::map<std::string, std::string> & _return);

  int64_t aliveSince();

  virtual void reinitialize() {}

  virtual void shutdown() {
    if (server_.get() != NULL) {
      server_->stop();
    }
  }

  int64_t incrementCounter(const std::string& key, int64_t amount = 1);
  int64_t setCounter(const std::string& key, int64_t value);

  void getCounters(std::map<std::string, int64_t>& _return);
  int64_t getCounter(const std::string& key);

  /**
   * Set server handle for shutdown method
   */
  void setServer(boost::shared_ptr<TServer> server) {
    server_ = server;
  }

  void getCpuProfile(std::string& _return, int32_t durSecs) { _return = ""; }

 private:

  std::string name_;
  int64_t aliveSince_;

  std::map<std::string, std::string> options_;
  Mutex optionsLock_;

  ReadWriteCounterMap counters_;

  boost::shared_ptr<TServer> server_;

};

}} // facebook::tb303

#endif // _FACEBOOK_TB303_FACEBOOKBASE_H_
