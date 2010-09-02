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
#include <climits>
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


/// Overload condition actions.
enum TOverloadAction {
  T_OVERLOAD_NO_ACTION,        ///< Don't handle overload */
  T_OVERLOAD_CLOSE_ON_ACCEPT,  ///< Drop new connections immediately */
  T_OVERLOAD_DRAIN_TASK_QUEUE  ///< Drop some tasks from head of task queue */
};

class TNonblockingServer : public TServer {
 private:
  /// Listen backlog
  static const int LISTEN_BACKLOG = 1024;

  /// Default limit on size of idle connection pool
  static const size_t CONNECTION_STACK_LIMIT = 1024;

  /// Maximum size of buffer allocated to idle connection
  static const uint32_t IDLE_BUFFER_MEM_LIMIT = 8192;

  /// Default limit on total number of connected sockets
  static const int MAX_CONNECTIONS = INT_MAX;

  /// Default limit on connections in handler/task processing
  static const int MAX_ACTIVE_PROCESSORS = INT_MAX;

  /// Server socket file descriptor
  int serverSocket_;

  /// Port server runs on
  int port_;

  /// For processing via thread pool, may be NULL
  boost::shared_ptr<ThreadManager> threadManager_;

  /// Is thread pool processing?
  bool threadPoolProcessing_;

  /// The event base for libevent
  event_base* eventBase_;

  /// Event struct, used with eventBase_ for connection events
  struct event serverEvent_;

  /// Event struct, used with eventBase_ for task completion notification
  struct event notificationEvent_;

  /// Number of TConnection object we've created
  size_t numTConnections_;

  /// Number of Connections processing or waiting to process
  size_t numActiveProcessors_;

  /// Limit for how many TConnection objects to cache
  size_t connectionStackLimit_;

  /// Limit for number of connections processing or waiting to process
  size_t maxActiveProcessors_;

  /// Limit for number of open connections
  size_t maxConnections_;

  /// Time in milliseconds before an unperformed task expires (0 == infinite).
  int64_t taskExpireTime_;

  /**
   * Hysteresis for overload state.  This is the fraction of the overload
   * value that needs to be reached before the overload state is cleared;
   * must be <= 1.0.
   */
  double overloadHysteresis_;

  /// Action to take when we're overloaded.
  TOverloadAction overloadAction_;

  /**
   * Max read buffer size for an idle connection.  When we place an idle
   * TConnection into connectionStack_, we insure that its read buffer is
   * reduced to this size to insure that idle connections don't hog memory.
   */
  size_t idleBufferMemLimit_;

  /// Set if we are currently in an overloaded state.
  bool overloaded_;

  /// Count of connections dropped since overload started
  uint32_t nConnectionsDropped_;

  /// Count of connections dropped on overload since server started
  uint64_t nTotalConnectionsDropped_;

  /// File descriptors for pipe used for task completion notification.
  int notificationPipeFDs_[2];

  /**
   * This is a stack of all the objects that have been created but that
   * are NOT currently in use. When we close a connection, we place it on this
   * stack so that the object can be reused later, rather than freeing the
   * memory and reallocating a new object later.
   */
  std::stack<TConnection*> connectionStack_;

  /**
   * Called when server socket had something happen.  We accept all waiting
   * client connections on listen socket fd and assign TConnection objects
   * to handle those requests.
   *
   * @param fd the listen socket.
   * @param which the event flag that triggered the handler.
   */
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
    numActiveProcessors_(0),
    connectionStackLimit_(CONNECTION_STACK_LIMIT),
    maxActiveProcessors_(MAX_ACTIVE_PROCESSORS),
    maxConnections_(MAX_CONNECTIONS),
    taskExpireTime_(0),
    overloadHysteresis_(0.8),
    overloadAction_(T_OVERLOAD_NO_ACTION),
    idleBufferMemLimit_(IDLE_BUFFER_MEM_LIMIT),
    overloaded_(false),
    nConnectionsDropped_(0),
    nTotalConnectionsDropped_(0) {}

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
    numActiveProcessors_(0),
    connectionStackLimit_(CONNECTION_STACK_LIMIT),
    maxActiveProcessors_(MAX_ACTIVE_PROCESSORS),
    maxConnections_(MAX_CONNECTIONS),
    taskExpireTime_(0),
    overloadHysteresis_(0.8),
    overloadAction_(T_OVERLOAD_NO_ACTION),
    idleBufferMemLimit_(IDLE_BUFFER_MEM_LIMIT),
    overloaded_(false),
    nConnectionsDropped_(0),
    nTotalConnectionsDropped_(0) {
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
    serverSocket_(-1),
    port_(port),
    threadManager_(threadManager),
    eventBase_(NULL),
    numTConnections_(0),
    numActiveProcessors_(0),
    connectionStackLimit_(CONNECTION_STACK_LIMIT),
    maxActiveProcessors_(MAX_ACTIVE_PROCESSORS),
    maxConnections_(MAX_CONNECTIONS),
    taskExpireTime_(0),
    overloadHysteresis_(0.8),
    overloadAction_(T_OVERLOAD_NO_ACTION),
    idleBufferMemLimit_(IDLE_BUFFER_MEM_LIMIT),
    overloaded_(false),
    nConnectionsDropped_(0),
    nTotalConnectionsDropped_(0) {
    setInputTransportFactory(inputTransportFactory);
    setOutputTransportFactory(outputTransportFactory);
    setInputProtocolFactory(inputProtocolFactory);
    setOutputProtocolFactory(outputProtocolFactory);
    setThreadManager(threadManager);
  }

  ~TNonblockingServer();

  void setThreadManager(boost::shared_ptr<ThreadManager> threadManager);

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
    threadManager_->add(task, 0LL, taskExpireTime_);
  }

  event_base* getEventBase() const {
    return eventBase_;
  }

  /// Increment our count of the number of connected sockets.
  void incrementNumConnections() {
    ++numTConnections_;
  }

  /// Decrement our count of the number of connected sockets.
  void decrementNumConnections() {
    --numTConnections_;
  }

  /**
   * Return the count of sockets currently connected to.
   *
   * @return count of connected sockets.
   */
  size_t getNumConnections() const {
    return numTConnections_;
  }

  /**
   * Return the count of connection objects allocated but not in use.
   *
   * @return count of idle connection objects.
   */
  size_t getNumIdleConnections() const {
    return connectionStack_.size();
  }

  /**
   * Return count of number of connections which are currently processing.
   * This is defined as a connection where all data has been received and
   * either assigned a task (when threading) or passed to a handler (when
   * not threading), and where the handler has not yet returned.
   *
   * @return # of connections currently processing.
   */
  size_t getNumActiveProcessors() const {
    return numActiveProcessors_;
  }

  /// Increment the count of connections currently processing.
  void incrementActiveProcessors() {
    ++numActiveProcessors_;
  }

  /// Decrement the count of connections currently processing.
  void decrementActiveProcessors() {
    if (numActiveProcessors_ > 0) {
      --numActiveProcessors_;
    }
  }

  /**
   * Get the maximum # of connections allowed before overload.
   *
   * @return current setting.
   */
  size_t getMaxConnections() const {
    return maxConnections_;
  }

  /**
   * Set the maximum # of connections allowed before overload.
   *
   * @param maxConnections new setting for maximum # of connections.
   */
  void setMaxConnections(size_t maxConnections) {
    maxConnections_ = maxConnections;
  }

  /**
   * Get the maximum # of connections waiting in handler/task before overload.
   *
   * @return current setting.
   */
  size_t getMaxActiveProcessors() const {
    return maxActiveProcessors_;
  }

  /**
   * Set the maximum # of connections waiting in handler/task before overload.
   *
   * @param maxActiveProcessors new setting for maximum # of active processes.
   */
  void setMaxActiveProcessors(size_t maxActiveProcessors) {
    maxActiveProcessors_ = maxActiveProcessors;
  }

  /**
   * Get fraction of maximum limits before an overload condition is cleared.
   *
   * @return hysteresis fraction
   */
  double getOverloadHysteresis() const {
    return overloadHysteresis_;
  }

  /**
   * Set fraction of maximum limits before an overload condition is cleared.
   * A good value would probably be between 0.5 and 0.9.
   *
   * @param hysteresisFraction fraction <= 1.0.
   */
  void setOverloadHysteresis(double hysteresisFraction) {
    if (hysteresisFraction <= 1.0 && hysteresisFraction > 0.0) {
      overloadHysteresis_ = hysteresisFraction;
    }
  }

  /**
   * Get the action the server will take on overload.
   *
   * @return a TOverloadAction enum value for the currently set action.
   */
  TOverloadAction getOverloadAction() const {
    return overloadAction_;
  }

  /**
   * Set the action the server is to take on overload.
   *
   * @param overloadAction a TOverloadAction enum value for the action.
   */
  void setOverloadAction(TOverloadAction overloadAction) {
    overloadAction_ = overloadAction;
  }

  /**
   * Get the time in milliseconds after which a task expires (0 == infinite).
   *
   * @return a 64-bit time in milliseconds.
   */
  int64_t getTaskExpireTime() const {
    return taskExpireTime_;
  }

  /**
   * Set the time in milliseconds after which a task expires (0 == infinite).
   *
   * @param taskExpireTime a 64-bit time in milliseconds.
   */
  void setTaskExpireTime(int64_t taskExpireTime) {
    taskExpireTime_ = taskExpireTime;
  }

  /**
   * Determine if the server is currently overloaded.
   * This function checks the maximums for open connections and connections
   * currently in processing, and sets an overload condition if they are
   * exceeded.  The overload will persist until both values are below the
   * current hysteresis fraction of their maximums.
   *
   * @return true if an overload condition exists, false if not.
   */
  bool serverOverloaded();

  /** Pop and discard next task on threadpool wait queue.
   *
   * @return true if a task was discarded, false if the wait queue was empty.
   */
  bool drainPendingTask();

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

  /**
   * Return an initialized connection object.  Creates or recovers from
   * pool a TConnection and initializes it with the provided socket FD
   * and flags.
   *
   * @param socket FD of socket associated with this connection.
   * @param flags initial lib_event flags for this connection.
   * @return pointer to initialized TConnection object.
   */
  TConnection* createConnection(int socket, short flags);

  /**
   * Returns a connection to pool or deletion.  If the connection pool
   * (a stack) isn't full, place the connection object on it, otherwise
   * just delete it.
   *
   * @param connection the TConection being returned.
   */
  void returnConnection(TConnection* connection);

  /**
   * Callback function that the threadmanager calls when a task reaches
   * its expiration time.  It is needed to clean up the expired connection.
   *
   * @param task the runnable associated with the expired task.
   */
  void expireClose(boost::shared_ptr<Runnable> task);

  /**
   * C-callable event handler for listener events.  Provides a callback
   * that libevent can understand which invokes server->handleEvent().
   *
   * @param fd the descriptor the event occured on.
   * @param which the flags associated with the event.
   * @param v void* callback arg where we placed TNonblockingServer's "this".
   */
  static void eventHandler(int fd, short which, void* v) {
    ((TNonblockingServer*)v)->handleEvent(fd, which);
  }

  /// Creates a socket to listen on and binds it to the local port.
  void listenSocket();

  /**
   * Takes a socket created by listenSocket() and sets various options on it
   * to prepare for use in the server.
   *
   * @param fd descriptor of socket to be initialized/
   */
  void listenSocket(int fd);

  /// Create the pipe used to notify I/O process of task completion.
  void createNotificationPipe();

  /**
   * Get notification pipe send descriptor.
   *
   * @return write fd for pipe.
   */
  int getNotificationSendFD() const {
    return notificationPipeFDs_[1];
  }

  /**
   * Get notification pipe receive descriptor.
   *
   * @return read fd of pipe.
   */
  int getNotificationRecvFD() const {
    return notificationPipeFDs_[0];
  }

  /**
   * Register the core libevent events onto the proper base.
   *
   * @param base pointer to the event base to be initialized.
   */
  void registerEvents(event_base* base);

  /**
   * Main workhorse function, starts up the server listening on a port and
   * loops over the libevent handler.
   */
  void serve();
};

/// Two states for sockets, recv and send mode
enum TSocketState {
  SOCKET_RECV,
  SOCKET_SEND
};

/**
 * Five states for the nonblocking servr:
 *  1) initialize
 *  2) read 4 byte frame size
 *  3) read frame of data
 *  4) send back data (if any)
 *  5) force immediate connection close
 */
enum TAppState {
  APP_INIT,
  APP_READ_FRAME_SIZE,
  APP_READ_REQUEST,
  APP_WAIT_TASK,
  APP_SEND_RESULT,
  APP_CLOSE_CONNECTION
};

/**
 * Represents a connection that is handled via libevent. This connection
 * essentially encapsulates a socket that has some associated libevent state.
 */
class TConnection {
 private:

  /// Starting size for new connection buffer
  static const int STARTING_CONNECTION_BUFFER_SIZE = 1024;

  /// Server handle
  TNonblockingServer* server_;

  /// Socket handle
  int socket_;

  /// Libevent object
  struct event event_;

  /// Libevent flags
  short eventFlags_;

  /// Socket mode
  TSocketState socketState_;

  /// Application state
  TAppState appState_;

  /// How much data needed to read
  uint32_t readWant_;

  /// Where in the read buffer are we
  uint32_t readBufferPos_;

  /// Read buffer
  uint8_t* readBuffer_;

  /// Read buffer size
  uint32_t readBufferSize_;

  /// Write buffer
  uint8_t* writeBuffer_;

  /// Write buffer size
  uint32_t writeBufferSize_;

  /// How far through writing are we?
  uint32_t writeBufferPos_;

  /// How many times have we read since our last buffer reset?
  uint32_t numReadsSinceReset_;

  /// How many times have we written since our last buffer reset?
  uint32_t numWritesSinceReset_;

  /// Task handle
  int taskHandle_;

  /// Task event
  struct event taskEvent_;

  /// Transport to read from
  boost::shared_ptr<TMemoryBuffer> inputTransport_;

  /// Transport that processor writes to
  boost::shared_ptr<TMemoryBuffer> outputTransport_;

  /// extra transport generated by transport factory (e.g. BufferedRouterTransport)
  boost::shared_ptr<TTransport> factoryInputTransport_;
  boost::shared_ptr<TTransport> factoryOutputTransport_;

  /// Protocol decoder
  boost::shared_ptr<TProtocol> inputProtocol_;

  /// Protocol encoder
  boost::shared_ptr<TProtocol> outputProtocol_;

  /// Go into read mode
  void setRead() {
    setFlags(EV_READ | EV_PERSIST);
  }

  /// Go into write mode
  void setWrite() {
    setFlags(EV_WRITE | EV_PERSIST);
  }

  /// Set socket idle
  void setIdle() {
    setFlags(0);
  }

  /**
   * Set event flags for this connection.
   *
   * @param eventFlags flags we pass to libevent for the connection.
   */
  void setFlags(short eventFlags);

  /**
   * Libevent handler called (via our static wrapper) when the connection
   * socket had something happen.  Rather than use the flags libevent passed,
   * we use the connection state to determine whether we need to read or
   * write the socket.
   */
  void workSocket();

  /// Close this connection and free or reset its resources.
  void close();

 public:

  class Task;

  /// Constructor
  TConnection(int socket, short eventFlags, TNonblockingServer *s) {
    readBuffer_ = (uint8_t*)std::malloc(STARTING_CONNECTION_BUFFER_SIZE);
    if (readBuffer_ == NULL) {
      throw new apache::thrift::TException("Out of memory.");
    }
    readBufferSize_ = STARTING_CONNECTION_BUFFER_SIZE;

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
    std::free(readBuffer_);
    server_->decrementNumConnections();
  }

  /**
   * Check read buffer against a given limit and shrink it if exceeded.
   *
   * @param limit we limit buffer size to.
   */
  void checkIdleBufferMemLimit(size_t limit);

  /// Initialize
  void init(int socket, short eventFlags, TNonblockingServer *s);

  /**
   * This is called when the application transitions from one state into
   * another. This means that it has finished writing the data that it needed
   * to, or finished receiving the data that it needed to.
   */
  void transition();

  /**
   * C-callable event handler for connection events.  Provides a callback
   * that libevent can understand which invokes connection_->workSocket().
   *
   * @param fd the descriptor the event occured on.
   * @param which the flags associated with the event.
   * @param v void* callback arg where we placed TConnection's "this".
   */
  static void eventHandler(int fd, short /* which */, void* v) {
    assert(fd == ((TConnection*)v)->socket_);
    ((TConnection*)v)->workSocket();
  }

  /**
   * C-callable event handler for signaling task completion.  Provides a
   * callback that libevent can understand that will read a connection
   * object's address from a pipe and call connection->transition() for
   * that object.
   *
   * @param fd the descriptor the event occured on.
   */
  static void taskHandler(int fd, short /* which */, void* /* v */) {
    TConnection* connection;
    ssize_t nBytes;
    while ((nBytes = read(fd, (void*)&connection, sizeof(TConnection*)))
        == sizeof(TConnection*)) {
      connection->transition();
    }
    if (nBytes > 0) {
      throw TException("TConnection::taskHandler unexpected partial read");
    }
    if (errno != EWOULDBLOCK && errno != EAGAIN) {
      GlobalOutput.perror("TConnection::taskHandler read failed, resource leak", errno);
    }
  }

  /**
   * Notification to server that processing has ended on this request.
   * Can be called either when processing is completed or when a waiting
   * task has been preemptively terminated (on overload).
   *
   * @return true if successful, false if unable to notify (check errno).
   */
  bool notifyServer() {
    TConnection* connection = this;
    if (write(server_->getNotificationSendFD(), (const void*)&connection,
             sizeof(TConnection*)) != sizeof(TConnection*)) {
      return false;
    }

    return true;
  }

  /// Force connection shutdown for this connection.
  void forceClose() {
    appState_ = APP_CLOSE_CONNECTION;
    if (!notifyServer()) {
      throw TException("TConnection::forceClose: failed write on notify pipe");
    }
  }

  /// return the server this connection was initialized for.
  TNonblockingServer* getServer() {
    return server_;
  }

  /// get state of connection.
  TAppState getState() {
    return appState_;
  }
};

}}} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
