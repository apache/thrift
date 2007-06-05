// Copyright (c) 2007- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <algorithm>
#include <iostream>

#include "TSocketPool.h"

namespace facebook { namespace thrift { namespace transport { 

using namespace std;

/**
 * TSocketPool implementation.
 *
 * @author Jason Sobel <jsobel@facebook.com>
 */

TSocketPool::TSocketPool(const vector<string> &hosts,
                         const vector<int> &ports) : TSocket(),
  numRetries_(1),
  retryInterval_(60),
  maxConsecutiveFailures_(1),
  randomize_(true),
  alwaysTryLast_(true)
{
  if (hosts.size() != ports.size()) {
    GlobalOutput("TSocketPool::TSocketPool: hosts.size != ports.size");
    throw TTransportException(TTransportException::BAD_ARGS);
  }

  for (unsigned int i = 0; i < hosts.size(); ++i) {
    servers_.push_back(pair<string, int>(hosts[i], ports[i]));
  }
}

TSocketPool::TSocketPool(const vector<pair<string, int> > servers) : TSocket(),
  servers_(servers),
  numRetries_(1),
  retryInterval_(60),
  maxConsecutiveFailures_(1),
  randomize_(true),
  alwaysTryLast_(true)
{
}

TSocketPool::~TSocketPool() {
  close();
}

void TSocketPool::setNumRetries(int numRetries) {
  numRetries_ = numRetries;
}

void TSocketPool::setRetryInterval(int retryInterval) {
  retryInterval_ = retryInterval;
}


void TSocketPool::setMaxConsecutiveFailures(int maxConsecutiveFailures) {
  maxConsecutiveFailures_ = maxConsecutiveFailures;
}

void TSocketPool::setRandomize(bool randomize) {
  randomize_ = randomize;
}

void TSocketPool::setAlwaysTryLast(bool alwaysTryLast) {
  alwaysTryLast_ = alwaysTryLast;
}

/* TODO: without apc we ignore a lot of functionality from the php version */
void TSocketPool::open() {
  if (randomize_) {
    std::random_shuffle(servers_.begin(), servers_.end());
  }

  for (unsigned int i = 0; i < servers_.size(); ++i) {
    host_ = servers_[i].first;
    port_ = servers_[i].second;

    for (int j = 0; j < numRetries_; ++j) {
      try {
        TSocket::open();

        // success
        return;
      } catch (TException e) {
        // connection failed
      }
    }
  }

  GlobalOutput("TSocketPool::open: all connections failed");
  throw TTransportException(TTransportException::NOT_OPEN);
}

}}} // facebook::thrift::transport
