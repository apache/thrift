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

#include <algorithm>
#include <iostream>

#include "TSocketPool.h"

namespace apache { namespace thrift { namespace transport {

using namespace std;

using boost::shared_ptr;

/**
 * TSocketPoolServer implementation
 *
 */
TSocketPoolServer::TSocketPoolServer()
  : host_(""),
    port_(0),
    socket_(-1),
    lastFailTime_(0),
    consecutiveFailures_(0) {}

/**
 * Constructor for TSocketPool server
 */
TSocketPoolServer::TSocketPoolServer(const string &host, int port)
  : host_(host),
    port_(port),
    socket_(-1),
    lastFailTime_(0),
    consecutiveFailures_(0) {}

/**
 * TSocketPool implementation.
 *
 */

TSocketPool::TSocketPool() : TSocket(),
  numRetries_(1),
  retryInterval_(60),
  maxConsecutiveFailures_(1),
  randomize_(true),
  alwaysTryLast_(true) {
}

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

TSocketPool::TSocketPool(const vector<pair<string, int> >& servers) : TSocket(),
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

TSocketPool::TSocketPool(const vector< shared_ptr<TSocketPoolServer> >& servers) : TSocket(),
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
  vector< shared_ptr<TSocketPoolServer> >::const_iterator iter = servers_.begin();
  vector< shared_ptr<TSocketPoolServer> >::const_iterator iterEnd = servers_.end();
  for (; iter != iterEnd; ++iter) {
    setCurrentServer(*iter);
    TSocketPool::close();
  }
}

void TSocketPool::addServer(const string& host, int port) {
  servers_.push_back(shared_ptr<TSocketPoolServer>(new TSocketPoolServer(host, port)));
}

void TSocketPool::setServers(const vector< shared_ptr<TSocketPoolServer> >& servers) {
  servers_ = servers;
}

void TSocketPool::getServers(vector< shared_ptr<TSocketPoolServer> >& servers) {
  servers = servers_;
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

void TSocketPool::setCurrentServer(const shared_ptr<TSocketPoolServer> &server) {
  currentServer_ = server;
  host_ = server->host_;
  port_ = server->port_;
  socket_ = server->socket_;
}

/* TODO: without apc we ignore a lot of functionality from the php version */
void TSocketPool::open() {
  if (randomize_) {
    random_shuffle(servers_.begin(), servers_.end());
  }

  unsigned int numServers = servers_.size();
  for (unsigned int i = 0; i < numServers; ++i) {

    shared_ptr<TSocketPoolServer> &server = servers_[i];
    bool retryIntervalPassed = (server->lastFailTime_ == 0);
    bool isLastServer = alwaysTryLast_ ? (i == (numServers - 1)) : false;

    // Impersonate the server socket
    setCurrentServer(server);

    if (isOpen()) {
      // already open means we're done
      return;
    }

    if (server->lastFailTime_ > 0) {
      // The server was marked as down, so check if enough time has elapsed to retry
      int elapsedTime = time(NULL) - server->lastFailTime_;
      if (elapsedTime > retryInterval_) {
        retryIntervalPassed = true;
      }
    }

    if (retryIntervalPassed || isLastServer) {
      for (int j = 0; j < numRetries_; ++j) {
        try {
          TSocket::open();

          // Copy over the opened socket so that we can keep it persistent
          server->socket_ = socket_;

          // reset lastFailTime_ is required
          if (server->lastFailTime_) {
            server->lastFailTime_ = 0;
          }

          // success
          return;
        } catch (TException e) {
          string errStr = "TSocketPool::open failed "+getSocketInfo()+": "+e.what();
          GlobalOutput(errStr.c_str());
          // connection failed
        }
      }

      ++server->consecutiveFailures_;
      if (server->consecutiveFailures_ > maxConsecutiveFailures_) {
        // Mark server as down
        server->consecutiveFailures_ = 0;
        server->lastFailTime_ = time(NULL);
      }
    }
  }

  GlobalOutput("TSocketPool::open: all connections failed");
  throw TTransportException(TTransportException::NOT_OPEN);
}

void TSocketPool::close() {
  if (isOpen()) {
    TSocket::close();
    currentServer_->socket_ = -1;
  }
}

}}} // apache::thrift::transport
