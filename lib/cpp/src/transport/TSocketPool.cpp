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
 * TSocketPoolServer implementation
 *
 * @author Akhil Wable <akhil@facebook.com>
 */
TSocketPoolServer::TSocketPoolServer()
  : host_(""),
    port_(0),
    lastFailTime_(0),
    consecutiveFailures_(0) {}

/**
 * Constructor for TSocketPool server
 */
TSocketPoolServer::TSocketPoolServer(const std::string &host, int port)
  : host_(host),
    port_(port),
    lastFailTime_(0),
    consecutiveFailures_(0) {}

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
    addServer(hosts[i], ports[i]);
  }
}

TSocketPool::TSocketPool(const std::vector<pair<string, int> >& servers) : TSocket(),
  numRetries_(1),
  retryInterval_(60),
  maxConsecutiveFailures_(1),
  randomize_(true),
  alwaysTryLast_(true)
{
  for (unsigned i = 0; i < servers.size(); ++i) {
    addServer(servers[i].first, servers[i].second);
  }
}

TSocketPool::TSocketPool(const std::vector<TSocketPoolServer>& servers) : TSocket(),
  servers_(servers),
  numRetries_(1),
  retryInterval_(60),
  maxConsecutiveFailures_(1),
  randomize_(true),
  alwaysTryLast_(true)
{
}

TSocketPool::TSocketPool(const string& host, int port) : TSocket(),
  numRetries_(1),
  retryInterval_(60),
  maxConsecutiveFailures_(1),
  randomize_(true),
  alwaysTryLast_(true)
{
  addServer(host, port);
}

TSocketPool::~TSocketPool() {
  close();
}

void TSocketPool::addServer(const string& host, int port) {
  servers_.push_back(TSocketPoolServer(host, port));
}

std::vector<TSocketPoolServer> TSocketPool::getServers() {
  return servers_;
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

  unsigned int numServers = servers_.size();
  for (unsigned int i = 0; i < numServers; ++i) {

    TSocketPoolServer &server = servers_[i];
    bool retryIntervalPassed = (server.lastFailTime_ == 0);
    bool isLastServer = alwaysTryLast_ ? (i == (numServers - 1)) : false;

    host_ = server.host_;
    port_ = server.port_;

    if (server.lastFailTime_ > 0) {
      // The server was marked as down, so check if enough time has elapsed to retry
      int elapsedTime = time(NULL) - server.lastFailTime_;
      if (elapsedTime > retryInterval_) {
        retryIntervalPassed = true;
      }
    }

    if (retryIntervalPassed || isLastServer) {
      for (int j = 0; j < numRetries_; ++j) {
        try {
          TSocket::open();

          // reset lastFailTime_ is required
          if (server.lastFailTime_) {
            server.lastFailTime_ = 0;
          }

          // success
          return;
        } catch (TException e) {
          // connection failed
        }
      }
    }

    ++server.consecutiveFailures_;
    if (server.consecutiveFailures_ > maxConsecutiveFailures_) {
      // Mark server as down
      server.consecutiveFailures_ = 0;
      server.lastFailTime_ = time(NULL);
    }
  }

  GlobalOutput("TSocketPool::open: all connections failed");
  throw TTransportException(TTransportException::NOT_OPEN);
}

}}} // facebook::thrift::transport
