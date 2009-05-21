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

#ifndef _THRIFT_SERVER_TNONBLOCKINGSERVER_H_
#define _THRIFT_SERVER_TNONBLOCKINGSERVER_H_ 1

#include <Thrift.h>
#include <server/TServer.h>
#include <transport/TBufferTransports.h>
#include <concurrency/ThreadManager.h>
#include <stack>
#include <string>
#include <errno.h>
#include <cstdlib>
#include <unistd.h>
#include <event.h>

namespace apache { namespace thrift { namespace server {

using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::protocol::TProtocol;
using apache::thrift::concurrency::Runnable;
using apache::thrift::concurrency::ThreadManager;

// Forward declaration of class
class TConnection;

/**
 * This is a non-blocking server in C++ for high performance that operates a
 * single IO thread. It assumes that all incoming requests are framed with a
 * 4 byte length indicator and writes out responses using the same framing.
 *
 * It does not use the TServerTransport framework, but rather has socket
 * operations hardcoded for use with select.
 *
 */
class TNonblockingServer : public TServer {
 private:

  // Listen backlog
  static const int LISTEN_BACKLOG = 1024;

  // Default limit on size of idle connection pool
  static const size_t CONNECTION_STACK_LIMIT = 1024;

  // Maximum size of buffer allocated to idle connection
  static const uint32_t IDLE_BUFFER_MEM_LIMIT = 8192;

  // Server socket file descriptor
  int serverSocket_;

  // Port server runs on
  int port_;

  // For processing via thread pool, may be NULL
  boost::shared_ptr<ThreadManager> threadManager_;

  // Is thread pool processing?
  bool threadPoolProcessing_;

  // The event base for libevent
  event_base* eventBase_;

  // Event struct, for use with eventBase_
  struct event serverEvent_;

  // Number of TConnection object we've created
  size_t numTConnections_;

  // Limit for how many TConnection objects to cache
  size_t connectionStackLimit_;

  /**
   * Max read buffer size for an idle connection.  When we place an idle
   * TConnection into connectionStack_, we insure that its read buffer is
   * reduced to this size to insure that idle connections don't hog memory.
   */
  uint32_t idleBufferMemLimit_;

  /**
   * This is a stack of all the objects that have been created but that
   * are NOT currently in use. When we close a connection, we place it on this
   * stack so that the object can be reused later, rather than freeing the
   * memory and reallocating a new object later.
   */
  std::stack<TConnection*> connectionStack_;

  void handleEvent(int fd, short which);

 public:
  TNonblockingServer(boost::shared_ptr<TProcessor> processor,
                     int port) :
    TServer(processor),
    serverSocket_(-1),
    port_(port),
    threadPoolProcessing_(false),
    eventBase_(NULL),
    numTConnections_(0),
    connectionStackLimit_(CONNECTION_STACK_LIMIT),
    idleBufferMemLimit_(IDLE_BUFFER_MEM_LIMIT) {}

  TNonblockingServer(boost::shared_ptr<TProcessor> processor,
                     boost::shared_ptr<TProtocolFactory> protocolFactory,
                     int port,
                     boost::shared_ptr<ThreadManager> threadManager = boost::shared_ptr<ThreadManager>()) :
    TServer(processor),
    serverSocket_(-1),
    port_(port),
    threadManager_(threadManager),
    eventBase_(NULL),
    numTConnections_(0),
    connectionStackLimit_(CONNECTION_STACK_LIMIT),
    idleBufferMemLimit_(IDLE_BUFFER_MEM_LIMIT) {
    setInputTransportFactory(boost::shared_ptr<TTransportFactory>(new TTransportFactory()));
    setOutputTransportFactory(boost::shared_ptr<TTransportFactory>(new TTransportFactory()));
    setInputProtocolFactory(protocolFactory);
    setOutputProtocolFactory(protocolFactory);
    setThreadManager(threadManager);
  }

  TNonblockingServer(boost::shared_ptr<TProcessor> processor,
                     boost::shared_ptr<TTransportFactory> inputTransportFactory,
                     boost::shared_ptr<TTransportFactory> outputTransportFactory,
                     boost::shared_ptr<TProtocolFactory> inputProtocolFactory,
                     boost::shared_ptr<TProtocolFactory> outputProtocolFactory,
                     int port,
                     boost::shared_ptr<ThreadManager> threadManager = boost::shared_ptr<ThreadManager>()) :
    TServer(processor),
    serverSocket_(0),
    port_(port),
    threadManager_(threadManager),
    eventBase_(NULL),
    numTConnections_(0),
    connectionStackLimit_(CONNECTION_STACK_LIMIT),
    idleBufferMemLimit_(IDLE_BUFFER_MEM_LIMIT) {
    setInputTransportFactory(inputTransportFactory);
    setOutputTransportFactory(outputTransportFactory);
    setInputProtocolFactory(inputProtocolFactory);
    setOutputProtocolFactory(outputProtocolFactory);
    setThreadManager(threadManager);
  }

  ~TNonblockingServer() {}

  void setThreadManager(boost::shared_ptr<ThreadManager> threadManager) {
    threadManager_ = threadManager;
    threadPoolProcessing_ = (threadManager != NULL);
  }

  boost::shared_ptr<ThreadManager> getThreadManager() {
    return threadManager_;
  }

  /**
   * Get the maximum number of unused TConnection we will hold in reserve.
   *
   * @return the current limit on TConnection pool size.
   */
  size_t getConnectionStackLimit() const {
    return connectionStackLimit_;
  }

  /**
   * Set the maximum number of unused TConnection we will hold in reserve.
   *
   * @param sz the new limit for TConnection pool size.
   */
  void setConnectionStackLimit(size_t sz) {
    connectionStackLimit_ = sz;
  }

  bool isThreadPoolProcessing() const {
    return threadPoolProcessing_;
  }

  void addTask(boost::shared_ptr<Runnable> task) {
    threadManager_->add(task);
  }

  event_base* getEventBase() const {
    return eventBase_;
  }

  void incrementNumConnections() {
    ++numTConnections_;
  }

  void decrementNumConnections() {
    --numTConnections_;
  }

  size_t getNumConnections() {
    return numTConnections_;
  }

  size_t getNumIdleConnections() {
    return connectionStack_.size();
  }

  /**
   * Get the maximum limit of memory allocated to idle TConnection objects.
   *
   * @return # bytes beyond which we will shrink buffers when idle.
   */
  size_t getIdleBufferMemLimit() const {
    return idleBufferMemLimit_;
  }

  /**
   * Set the maximum limit of memory allocated to idle TConnection objects.
   * If a TConnection object goes idle with more than this much memory
   * allocated to its buffer, we shrink it to this value.
   *
   * @param limit of bytes beyond which we will shrink buffers when idle.
   */
  void setIdleBufferMemLimit(size_t limit) {
    idleBufferMemLimit_ = limit;
  }

  TConnection* createConnection(int socket, short flags);

  void returnConnection(TConnection* connection);

  static void eventHandler(int fd, short which, void* v) {
    ((TNonblockingServer*)v)->handleEvent(fd, which);
  }

  void listenSocket();

  void listenSocket(int fd);

  void registerEvents(event_base* base);

  void serve();
};

/**
 * Two states for sockets, recv and send mode
 */
enum TSocketState {
  SOCKET_RECV,
  SOCKET_SEND
};

/**
 * Four states for the nonblocking servr:
 *  1) initialize
 *  2) read 4 byte frame size
 *  3) read frame of data
 *  4) send back data (if any)
 */
enum TAppState {
  APP_INIT,
  APP_READ_FRAME_SIZE,
  APP_READ_REQUEST,
  APP_WAIT_TASK,
  APP_SEND_RESULT
};

/**
 * Represents a connection that is handled via libevent. This connection
 * essentially encapsulates a socket that has some associated libevent state.
 */
class TConnection {
 private:

  class Task;

  // Server handle
  TNonblockingServer* server_;

  // Socket handle
  int socket_;

  // Libevent object
  struct event event_;

  // Libevent flags
  short eventFlags_;

  // Socket mode
  TSocketState socketState_;

  // Application state
  TAppState appState_;

  // How much data needed to read
  uint32_t readWant_;

  // Where in the read buffer are we
  uint32_t readBufferPos_;

  // Read buffer
  uint8_t* readBuffer_;

  // Read buffer size
  uint32_t readBufferSize_;

  // Write buffer
  uint8_t* writeBuffer_;

  // Write buffer size
  uint32_t writeBufferSize_;

  // How far through writing are we?
  uint32_t writeBufferPos_;

  // How many times have we read since our last buffer reset?
  uint32_t numReadsSinceReset_;

  // How many times have we written since our last buffer reset?
  uint32_t numWritesSinceReset_;

  // Task handle
  int taskHandle_;

  // Task event
  struct event taskEvent_;

  // Transport to read from
  boost::shared_ptr<TMemoryBuffer> inputTransport_;

  // Transport that processor writes to
  boost::shared_ptr<TMemoryBuffer> outputTransport_;

  // extra transport generated by transport factory (e.g. BufferedRouterTransport)
  boost::shared_ptr<TTransport> factoryInputTransport_;
  boost::shared_ptr<TTransport> factoryOutputTransport_;

  // Protocol decoder
  boost::shared_ptr<TProtocol> inputProtocol_;

  // Protocol encoder
  boost::shared_ptr<TProtocol> outputProtocol_;

  // Go into read mode
  void setRead() {
    setFlags(EV_READ | EV_PERSIST);
  }

  // Go into write mode
  void setWrite() {
    setFlags(EV_WRITE | EV_PERSIST);
  }

  // Set socket idle
  void setIdle() {
    setFlags(0);
  }

  // Set event flags
  void setFlags(short eventFlags);

  // Libevent handlers
  void workSocket();

  // Close this client and reset
  void close();

 public:

  // Constructor
  TConnection(int socket, short eventFlags, TNonblockingServer *s) {
    readBuffer_ = (uint8_t*)std::malloc(1024);
    if (readBuffer_ == NULL) {
      throw new apache::thrift::TException("Out of memory.");
    }
    readBufferSize_ = 1024;

    numReadsSinceReset_ = 0;
    numWritesSinceReset_ = 0;

    // Allocate input and output tranpsorts
    // these only need to be allocated once per TConnection (they don't need to be
    // reallocated on init() call)
    inputTransport_ = boost::shared_ptr<TMemoryBuffer>(new TMemoryBuffer(readBuffer_, readBufferSize_));
    outputTransport_ = boost::shared_ptr<TMemoryBuffer>(new TMemoryBuffer());

    init(socket, eventFlags, s);
    server_->incrementNumConnections();
  }

  ~TConnection() {
    server_->decrementNumConnections();
  }

  /**
   * Check read buffer against a given limit and shrink it if exceeded.
   *
   * @param limit we limit buffer size to.
   */
  void checkIdleBufferMemLimit(uint32_t limit);

  // Initialize
  void init(int socket, short eventFlags, TNonblockingServer *s);

  // Transition into a new state
  void transition();

  // Handler wrapper
  static void eventHandler(int fd, short /* which */, void* v) {
    assert(fd == ((TConnection*)v)->socket_);
    ((TConnection*)v)->workSocket();
  }

  // Handler wrapper for task block
  static void taskHandler(int fd, short /* which */, void* v) {
    assert(fd == ((TConnection*)v)->taskHandle_);
    if (-1 == ::close(((TConnection*)v)->taskHandle_)) {
      GlobalOutput.perror("TConnection::taskHandler close handle failed, resource leak ", errno);
    }
    ((TConnection*)v)->transition();
  }

};

}}} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
