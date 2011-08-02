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

#include "TNonblockingServer.h"
#include <concurrency/Exception.h>
#include <transport/TSocket.h>

#include <iostream>

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#include <netinet/tcp.h>
#endif

#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include <fcntl.h>
#include <errno.h>
#include <assert.h>

#ifndef AF_LOCAL
#define AF_LOCAL AF_UNIX
#endif

namespace apache { namespace thrift { namespace server {

using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::concurrency;
using namespace std;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransportException;

class TConnection::Task: public Runnable {
 public:
  Task(boost::shared_ptr<TProcessor> processor,
       boost::shared_ptr<TProtocol> input,
       boost::shared_ptr<TProtocol> output,
       TConnection* connection) :
    processor_(processor),
    input_(input),
    output_(output),
    connection_(connection),
    serverEventHandler_(connection_->getServerEventHandler()),
    connectionContext_(connection_->getConnectionContext()) {}

  void run() {
    try {
      for (;;) {
        if (serverEventHandler_ != NULL) {
          serverEventHandler_->processContext(connectionContext_, connection_->getTSocket());
        }
        if (!processor_->process(input_, output_, connectionContext_) ||
            !input_->getTransport()->peek()) {
          break;
        }
      }
    } catch (TTransportException& ttx) {
      cerr << "TNonblockingServer client died: " << ttx.what() << endl;
    } catch (TException& x) {
      cerr << "TNonblockingServer exception: " << x.what() << endl;
    } catch (bad_alloc&) {
      cerr << "TNonblockingServer caught bad_alloc exception.";
      exit(-1);
    } catch (...) {
      cerr << "TNonblockingServer uncaught exception." << endl;
    }

    // Signal completion back to the libevent thread via a pipe
    if (!connection_->notifyServer()) {
      throw TException("TNonblockingServer::Task::run: failed write on notify pipe");
    }
  }

  TConnection* getTConnection() {
    return connection_;
  }

 private:
  boost::shared_ptr<TProcessor> processor_;
  boost::shared_ptr<TProtocol> input_;
  boost::shared_ptr<TProtocol> output_;
  TConnection* connection_;
  boost::shared_ptr<TServerEventHandler> serverEventHandler_;
  void* connectionContext_;
};

void TConnection::init(int socket, short eventFlags, TNonblockingServer* s,
                       const sockaddr* addr, socklen_t addrLen) {
  tSocket_->setSocketFD(socket);
  tSocket_->setCachedAddress(addr, addrLen);

  server_ = s;
  appState_ = APP_INIT;
  eventFlags_ = 0;

  readBufferPos_ = 0;
  readWant_ = 0;

  writeBuffer_ = NULL;
  writeBufferSize_ = 0;
  writeBufferPos_ = 0;
  largestWriteBufferSize_ = 0;

  socketState_ = SOCKET_RECV_FRAMING;
  appState_ = APP_INIT;
  callsForResize_ = 0;

  // Set flags, which also registers the event
  setFlags(eventFlags);

  // get input/transports
  factoryInputTransport_ = s->getInputTransportFactory()->getTransport(inputTransport_);
  factoryOutputTransport_ = s->getOutputTransportFactory()->getTransport(outputTransport_);

  // Create protocol
  inputProtocol_ = s->getInputProtocolFactory()->getProtocol(factoryInputTransport_);
  outputProtocol_ = s->getOutputProtocolFactory()->getProtocol(factoryOutputTransport_);

  // Set up for any server event handler
  serverEventHandler_ = server_->getEventHandler();
  if (serverEventHandler_ != NULL) {
    connectionContext_ = serverEventHandler_->createContext(inputProtocol_, outputProtocol_);
  } else {
    connectionContext_ = NULL;
  }
}

void TConnection::workSocket() {
  int got=0, left=0, sent=0;
  uint32_t fetch = 0;

  switch (socketState_) {
  case SOCKET_RECV_FRAMING:
    union {
      uint8_t buf[sizeof(uint32_t)];
      int32_t size;
    } framing;

    // if we've already received some bytes we kept them here
    framing.size = readWant_;
    // determine size of this frame
    try {
      // Read from the socket
      fetch = tSocket_->read(&framing.buf[readBufferPos_],
                             uint32_t(sizeof(framing.size) - readBufferPos_));
      if (fetch == 0) {
        // Whenever we get here it means a remote disconnect
        close();
        return;
      }
      readBufferPos_ += fetch;
    } catch (TTransportException& te) {
      GlobalOutput.printf("TConnection::workSocket(): %s", te.what());
      close();

      return;
    }

    if (readBufferPos_ < sizeof(framing.size)) {
      // more needed before frame size is known -- save what we have so far
      readWant_ = framing.size;
      return;
    }

    readWant_ = ntohl(framing.size);
    if (static_cast<int>(readWant_) <= 0) {
      GlobalOutput.printf("TConnection:workSocket() Negative frame size %d, remote side not using TFramedTransport?", static_cast<int>(readWant_));
      close();
      return;
    }
    // size known; now get the rest of the frame
    transition();
    return;

  case SOCKET_RECV:
    // It is an error to be in this state if we already have all the data
    assert(readBufferPos_ < readWant_);

    try {
      // Read from the socket
      fetch = readWant_ - readBufferPos_;
      got = tSocket_->read(readBuffer_ + readBufferPos_, fetch);
    }
    catch (TTransportException& te) {
      GlobalOutput.printf("TConnection::workSocket(): %s", te.what());
      close();

      return;
    }
        
    if (got > 0) {
      // Move along in the buffer
      readBufferPos_ += got;

      // Check that we did not overdo it
      assert(readBufferPos_ <= readWant_);

      // We are done reading, move onto the next state
      if (readBufferPos_ == readWant_) {
        transition();
      }
      return;
    }

    // Whenever we get down here it means a remote disconnect
    close();

    return;

  case SOCKET_SEND:
    // Should never have position past size
    assert(writeBufferPos_ <= writeBufferSize_);

    // If there is no data to send, then let us move on
    if (writeBufferPos_ == writeBufferSize_) {
      GlobalOutput("WARNING: Send state with no data to send\n");
      transition();
      return;
    }

    try {
      left = writeBufferSize_ - writeBufferPos_;
      sent = tSocket_->write_partial(writeBuffer_ + writeBufferPos_, left);
    }
    catch (TTransportException& te) {
      GlobalOutput.printf("TConnection::workSocket(): %s ", te.what());
      close();
      return;
    }

    writeBufferPos_ += sent;

    // Did we overdo it?
    assert(writeBufferPos_ <= writeBufferSize_);

    // We are done!
    if (writeBufferPos_ == writeBufferSize_) {
      transition();
    }

    return;

  default:
    GlobalOutput.printf("Unexpected Socket State %d", socketState_);
    assert(0);
  }
}

/**
 * This is called when the application transitions from one state into
 * another. This means that it has finished writing the data that it needed
 * to, or finished receiving the data that it needed to.
 */
void TConnection::transition() {
  // Switch upon the state that we are currently in and move to a new state
  switch (appState_) {

  case APP_READ_REQUEST:
    // We are done reading the request, package the read buffer into transport
    // and get back some data from the dispatch function
    inputTransport_->resetBuffer(readBuffer_, readBufferPos_);
    outputTransport_->resetBuffer();
    // Prepend four bytes of blank space to the buffer so we can
    // write the frame size there later.
    outputTransport_->getWritePtr(4);
    outputTransport_->wroteBytes(4);

    server_->incrementActiveProcessors();

    if (server_->isThreadPoolProcessing()) {
      // We are setting up a Task to do this work and we will wait on it

      // Create task and dispatch to the thread manager
      boost::shared_ptr<Runnable> task =
        boost::shared_ptr<Runnable>(new Task(server_->getProcessor(),
                                             inputProtocol_,
                                             outputProtocol_,
                                             this));
      // The application is now waiting on the task to finish
      appState_ = APP_WAIT_TASK;

        try {
          server_->addTask(task);
        } catch (IllegalStateException & ise) {
          // The ThreadManager is not ready to handle any more tasks (it's probably shutting down).
          GlobalOutput.printf("IllegalStateException: Server::process() %s", ise.what());
          close();
        }

      // Set this connection idle so that libevent doesn't process more
      // data on it while we're still waiting for the threadmanager to
      // finish this task
      setIdle();
      return;
    } else {
      try {
        // Invoke the processor
        server_->getProcessor()->process(inputProtocol_, outputProtocol_, NULL);
      } catch (TTransportException &ttx) {
        GlobalOutput.printf("TTransportException: Server::process() %s", ttx.what());
        server_->decrementActiveProcessors();
        close();
        return;
      } catch (TException &x) {
        GlobalOutput.printf("TException: Server::process() %s", x.what());
        server_->decrementActiveProcessors();
        close();
        return;
      } catch (...) {
        GlobalOutput.printf("Server::process() unknown exception");
        server_->decrementActiveProcessors();
        close();
        return;
      }
    }

    // Intentionally fall through here, the call to process has written into
    // the writeBuffer_

  case APP_WAIT_TASK:
    // We have now finished processing a task and the result has been written
    // into the outputTransport_, so we grab its contents and place them into
    // the writeBuffer_ for actual writing by the libevent thread

    server_->decrementActiveProcessors();
    // Get the result of the operation
    outputTransport_->getBuffer(&writeBuffer_, &writeBufferSize_);

    // If the function call generated return data, then move into the send
    // state and get going
    // 4 bytes were reserved for frame size
    if (writeBufferSize_ > 4) {

      // Move into write state
      writeBufferPos_ = 0;
      socketState_ = SOCKET_SEND;

      // Put the frame size into the write buffer
      int32_t frameSize = (int32_t)htonl(writeBufferSize_ - 4);
      memcpy(writeBuffer_, &frameSize, 4);

      // Socket into write mode
      appState_ = APP_SEND_RESULT;
      setWrite();

      // Try to work the socket immediately
      // workSocket();

      return;
    }

    // In this case, the request was oneway and we should fall through
    // right back into the read frame header state
    goto LABEL_APP_INIT;

  case APP_SEND_RESULT:
    // it's now safe to perform buffer size housekeeping.
    if (writeBufferSize_ > largestWriteBufferSize_) {
      largestWriteBufferSize_ = writeBufferSize_;
    }
    if (server_->getResizeBufferEveryN() > 0
        && ++callsForResize_ >= server_->getResizeBufferEveryN()) {
      checkIdleBufferMemLimit(server_->getIdleReadBufferLimit(),
                              server_->getIdleWriteBufferLimit());
      callsForResize_ = 0;
    }

    // N.B.: We also intentionally fall through here into the INIT state!

  LABEL_APP_INIT:
  case APP_INIT:

    // Clear write buffer variables
    writeBuffer_ = NULL;
    writeBufferPos_ = 0;
    writeBufferSize_ = 0;

    // Into read4 state we go
    socketState_ = SOCKET_RECV_FRAMING;
    appState_ = APP_READ_FRAME_SIZE;

    readBufferPos_ = 0;

    // Register read event
    setRead();

    // Try to work the socket right away
    // workSocket();

    return;

  case APP_READ_FRAME_SIZE:
    // We just read the request length
    // Double the buffer size until it is big enough
    if (readWant_ > readBufferSize_) {
      if (readBufferSize_ == 0) {
        readBufferSize_ = 1;
      }
      uint32_t newSize = readBufferSize_;
      while (readWant_ > newSize) {
        newSize *= 2;
      }

      uint8_t* newBuffer = (uint8_t*)std::realloc(readBuffer_, newSize);
      if (newBuffer == NULL) {
        // nothing else to be done...
        throw std::bad_alloc();
      }
      readBuffer_ = newBuffer;
      readBufferSize_ = newSize;
    }

    readBufferPos_= 0;

    // Move into read request state
    socketState_ = SOCKET_RECV;
    appState_ = APP_READ_REQUEST;

    // Work the socket right away
    // workSocket();

    return;

  case APP_CLOSE_CONNECTION:
    server_->decrementActiveProcessors();
    close();
    return;

  default:
    GlobalOutput.printf("Unexpected Application State %d", appState_);
    assert(0);
  }
}

void TConnection::setFlags(short eventFlags) {
  // Catch the do nothing case
  if (eventFlags_ == eventFlags) {
    return;
  }

  // Delete a previously existing event
  if (eventFlags_ != 0) {
    if (event_del(&event_) == -1) {
      GlobalOutput("TConnection::setFlags event_del");
      return;
    }
  }

  // Update in memory structure
  eventFlags_ = eventFlags;

  // Do not call event_set if there are no flags
  if (!eventFlags_) {
    return;
  }

  /*
   * event_set:
   *
   * Prepares the event structure &event to be used in future calls to
   * event_add() and event_del().  The event will be prepared to call the
   * eventHandler using the 'sock' file descriptor to monitor events.
   *
   * The events can be either EV_READ, EV_WRITE, or both, indicating
   * that an application can read or write from the file respectively without
   * blocking.
   *
   * The eventHandler will be called with the file descriptor that triggered
   * the event and the type of event which will be one of: EV_TIMEOUT,
   * EV_SIGNAL, EV_READ, EV_WRITE.
   *
   * The additional flag EV_PERSIST makes an event_add() persistent until
   * event_del() has been called.
   *
   * Once initialized, the &event struct can be used repeatedly with
   * event_add() and event_del() and does not need to be reinitialized unless
   * the eventHandler and/or the argument to it are to be changed.  However,
   * when an ev structure has been added to libevent using event_add() the
   * structure must persist until the event occurs (assuming EV_PERSIST
   * is not set) or is removed using event_del().  You may not reuse the same
   * ev structure for multiple monitored descriptors; each descriptor needs
   * its own ev.
   */
  event_set(&event_, tSocket_->getSocketFD(), eventFlags_,
            TConnection::eventHandler, this);
  event_base_set(server_->getEventBase(), &event_);

  // Add the event
  if (event_add(&event_, 0) == -1) {
    GlobalOutput("TConnection::setFlags(): could not event_add");
  }
}

/**
 * Closes a connection
 */
void TConnection::close() {
  // Delete the registered libevent
  if (event_del(&event_) == -1) {
    GlobalOutput.perror("TConnection::close() event_del", errno);
  }

  if (serverEventHandler_ != NULL) {
    serverEventHandler_->deleteContext(connectionContext_, inputProtocol_, outputProtocol_);
  }

  // Close the socket
  tSocket_->close();

  // close any factory produced transports
  factoryInputTransport_->close();
  factoryOutputTransport_->close();

  // Give this object back to the server that owns it
  server_->returnConnection(this);
}

void TConnection::checkIdleBufferMemLimit(size_t readLimit,
                                          size_t writeLimit) {
  if (readLimit > 0 && readBufferSize_ > readLimit) {
    free(readBuffer_);
    readBuffer_ = NULL;
    readBufferSize_ = 0;
  }

  if (writeLimit > 0 && largestWriteBufferSize_ > writeLimit) {
    // just start over
    outputTransport_->resetBuffer(server_->getWriteBufferDefaultSize());
    largestWriteBufferSize_ = 0;
  }
}

TNonblockingServer::~TNonblockingServer() {
  // TODO: We currently leak any active TConnection objects.
  // Since we're shutting down and destroying the event_base, the TConnection
  // objects will never receive any additional callbacks.  (And even if they
  // did, it would be bad, since they keep a pointer around to the server,
  // which is being destroyed.)

  // Clean up unused TConnection objects in connectionStack_
  while (!connectionStack_.empty()) {
    TConnection* connection = connectionStack_.top();
    connectionStack_.pop();
    delete connection;
  }

  if (eventBase_ && ownEventBase_) {
    event_base_free(eventBase_);
  }

  if (serverSocket_ >= 0) {
    close(serverSocket_);
  }
}

/**
 * Creates a new connection either by reusing an object off the stack or
 * by allocating a new one entirely
 */
TConnection* TNonblockingServer::createConnection(int socket, short flags,
                                                  const sockaddr* addr,
                                                  socklen_t addrLen) {
  // Check the stack
  if (connectionStack_.empty()) {
    return new TConnection(socket, flags, this, addr, addrLen);
  } else {
    TConnection* result = connectionStack_.top();
    connectionStack_.pop();
    result->init(socket, flags, this, addr, addrLen);
    return result;
  }
}

/**
 * Returns a connection to the stack
 */
void TNonblockingServer::returnConnection(TConnection* connection) {
  if (connectionStackLimit_ &&
      (connectionStack_.size() >= connectionStackLimit_)) {
    delete connection;
  } else {
    connection->checkIdleBufferMemLimit(idleReadBufferLimit_, idleWriteBufferLimit_);
    connectionStack_.push(connection);
  }
}

/**
 * Server socket had something happen.  We accept all waiting client
 * connections on fd and assign TConnection objects to handle those requests.
 */
void TNonblockingServer::handleEvent(int fd, short which) {
  (void) which;
  // Make sure that libevent didn't mess up the socket handles
  assert(fd == serverSocket_);

  // Server socket accepted a new connection
  socklen_t addrLen;
  sockaddr_storage addrStorage;
  sockaddr* addrp = (sockaddr*)&addrStorage;
  addrLen = sizeof(addrStorage);

  // Going to accept a new client socket
  int clientSocket;

  // Accept as many new clients as possible, even though libevent signaled only
  // one, this helps us to avoid having to go back into the libevent engine so
  // many times
  while ((clientSocket = ::accept(fd, addrp, &addrLen)) != -1) {
    // If we're overloaded, take action here
    if (overloadAction_ != T_OVERLOAD_NO_ACTION && serverOverloaded()) {
      nConnectionsDropped_++;
      nTotalConnectionsDropped_++;
      if (overloadAction_ == T_OVERLOAD_CLOSE_ON_ACCEPT) {
        close(clientSocket);
        return;
      } else if (overloadAction_ == T_OVERLOAD_DRAIN_TASK_QUEUE) {
        if (!drainPendingTask()) {
          // Nothing left to discard, so we drop connection instead.
          close(clientSocket);
          return;
        }
      }
    }
    // Explicitly set this socket to NONBLOCK mode
    int flags;
    if ((flags = fcntl(clientSocket, F_GETFL, 0)) < 0 ||
        fcntl(clientSocket, F_SETFL, flags | O_NONBLOCK) < 0) {
      GlobalOutput.perror("thriftServerEventHandler: set O_NONBLOCK (fcntl) ", errno);
      close(clientSocket);
      return;
    }

    // Create a new TConnection for this client socket.
    TConnection* clientConnection =
      createConnection(clientSocket, EV_READ | EV_PERSIST, addrp, addrLen);

    // Fail fast if we could not create a TConnection object
    if (clientConnection == NULL) {
      GlobalOutput.printf("thriftServerEventHandler: failed TConnection factory");
      close(clientSocket);
      return;
    }

    // Put this client connection into the proper state
    clientConnection->transition();

    // addrLen is written by the accept() call, so needs to be set before the next call.
    addrLen = sizeof(addrStorage);
  }

  // Done looping accept, now we have to make sure the error is due to
  // blocking. Any other error is a problem
  if (errno != EAGAIN && errno != EWOULDBLOCK) {
    GlobalOutput.perror("thriftServerEventHandler: accept() ", errno);
  }
}

/**
 * Creates a socket to listen on and binds it to the local port.
 */
void TNonblockingServer::listenSocket() {
  int s;
  struct addrinfo hints, *res, *res0;
  int error;

  char port[sizeof("65536") + 1];
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE | AI_ADDRCONFIG;
  sprintf(port, "%d", port_);

  // Wildcard address
  error = getaddrinfo(NULL, port, &hints, &res0);
  if (error) {
    string errStr = "TNonblockingServer::serve() getaddrinfo " + string(gai_strerror(error));
    GlobalOutput(errStr.c_str());
    return;
  }

  // Pick the ipv6 address first since ipv4 addresses can be mapped
  // into ipv6 space.
  for (res = res0; res; res = res->ai_next) {
    if (res->ai_family == AF_INET6 || res->ai_next == NULL)
      break;
  }

  // Create the server socket
  s = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if (s == -1) {
    freeaddrinfo(res0);
    throw TException("TNonblockingServer::serve() socket() -1");
  }

  #ifdef IPV6_V6ONLY
  if (res->ai_family == AF_INET6) {
    int zero = 0;
    if (-1 == setsockopt(s, IPPROTO_IPV6, IPV6_V6ONLY, const_cast_sockopt(&zero), sizeof(zero))) {
      GlobalOutput("TServerSocket::listen() IPV6_V6ONLY");
    }
  }
  #endif // #ifdef IPV6_V6ONLY


  int one = 1;

  // Set reuseaddr to avoid 2MSL delay on server restart
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, const_cast_sockopt(&one), sizeof(one));

  if (::bind(s, res->ai_addr, res->ai_addrlen) == -1) {
    close(s);
    freeaddrinfo(res0);
    throw TException("TNonblockingServer::serve() bind");
  }

  // Done with the addr info
  freeaddrinfo(res0);

  // Set up this file descriptor for listening
  listenSocket(s);
}

/**
 * Takes a socket created by listenSocket() and sets various options on it
 * to prepare for use in the server.
 */
void TNonblockingServer::listenSocket(int s) {
  // Set socket to nonblocking mode
  int flags;
  if ((flags = fcntl(s, F_GETFL, 0)) < 0 ||
      fcntl(s, F_SETFL, flags | O_NONBLOCK) < 0) {
    close(s);
    throw TException("TNonblockingServer::serve() O_NONBLOCK");
  }

  int one = 1;
  struct linger ling = {0, 0};

  // Keepalive to ensure full result flushing
  setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, const_cast_sockopt(&one), sizeof(one));

  // Turn linger off to avoid hung sockets
  setsockopt(s, SOL_SOCKET, SO_LINGER, const_cast_sockopt(&ling), sizeof(ling));

  // Set TCP nodelay if available, MAC OS X Hack
  // See http://lists.danga.com/pipermail/memcached/2005-March/001240.html
  #ifndef TCP_NOPUSH
  setsockopt(s, IPPROTO_TCP, TCP_NODELAY, const_cast_sockopt(&one), sizeof(one));
  #endif

  #ifdef TCP_LOW_MIN_RTO
  if (TSocket::getUseLowMinRto()) {
    setsockopt(s, IPPROTO_TCP, TCP_LOW_MIN_RTO, const_cast_sockopt(&one), sizeof(one));
  }
  #endif

  if (listen(s, LISTEN_BACKLOG) == -1) {
    close(s);
    throw TException("TNonblockingServer::serve() listen");
  }

  // Cool, this socket is good to go, set it as the serverSocket_
  serverSocket_ = s;
}

void TNonblockingServer::createNotificationPipe() {
  if(evutil_socketpair(AF_LOCAL, SOCK_STREAM, 0, notificationPipeFDs_) == -1) {
    GlobalOutput.perror("TNonblockingServer::createNotificationPipe ", EVUTIL_SOCKET_ERROR());
    throw TException("can't create notification pipe");
  }
  if(evutil_make_socket_nonblocking(notificationPipeFDs_[0])<0 ||
     evutil_make_socket_nonblocking(notificationPipeFDs_[1])<0) {
    close(notificationPipeFDs_[0]);
    close(notificationPipeFDs_[1]);
    throw TException("TNonblockingServer::createNotificationPipe() O_NONBLOCK");
  }
}

/**
 * Register the core libevent events onto the proper base.
 */
void TNonblockingServer::registerEvents(event_base* base, bool ownEventBase) {
  assert(serverSocket_ != -1);
  assert(!eventBase_);
  eventBase_ = base;
  ownEventBase_ = ownEventBase;

  // Print some libevent stats
  GlobalOutput.printf("libevent %s method %s",
          event_get_version(),
          event_get_method());

  // Register the server event
  event_set(&serverEvent_,
            serverSocket_,
            EV_READ | EV_PERSIST,
            TNonblockingServer::eventHandler,
            this);
  event_base_set(eventBase_, &serverEvent_);

  // Add the event and start up the server
  if (-1 == event_add(&serverEvent_, 0)) {
    throw TException("TNonblockingServer::serve(): coult not event_add");
  }
  if (threadPoolProcessing_) {
    // Create an event to be notified when a task finishes
    event_set(&notificationEvent_,
              getNotificationRecvFD(),
              EV_READ | EV_PERSIST,
              TConnection::taskHandler,
              this);

    // Attach to the base
    event_base_set(eventBase_, &notificationEvent_);

    // Add the event and start up the server
    if (-1 == event_add(&notificationEvent_, 0)) {
      throw TException("TNonblockingServer::serve(): notification event_add fail");
    }
  }
}

void TNonblockingServer::setThreadManager(boost::shared_ptr<ThreadManager> threadManager) {
  threadManager_ = threadManager;
  if (threadManager != NULL) {
    threadManager->setExpireCallback(std::tr1::bind(&TNonblockingServer::expireClose, this, std::tr1::placeholders::_1));
    threadPoolProcessing_ = true;
  } else {
    threadPoolProcessing_ = false;
  }
}

bool  TNonblockingServer::serverOverloaded() {
  size_t activeConnections = numTConnections_ - connectionStack_.size();
  if (numActiveProcessors_ > maxActiveProcessors_ ||
      activeConnections > maxConnections_) {
    if (!overloaded_) {
      GlobalOutput.printf("thrift non-blocking server overload condition");
      overloaded_ = true;
    }
  } else {
    if (overloaded_ &&
        (numActiveProcessors_ <= overloadHysteresis_ * maxActiveProcessors_) &&
        (activeConnections <= overloadHysteresis_ * maxConnections_)) {
      GlobalOutput.printf("thrift non-blocking server overload ended; %u dropped (%llu total)",
                          nConnectionsDropped_, nTotalConnectionsDropped_);
      nConnectionsDropped_ = 0;
      overloaded_ = false;
    }
  }

  return overloaded_;
}

bool TNonblockingServer::drainPendingTask() {
  if (threadManager_) {
    boost::shared_ptr<Runnable> task = threadManager_->removeNextPending();
    if (task) {
      TConnection* connection =
        static_cast<TConnection::Task*>(task.get())->getTConnection();
      assert(connection && connection->getServer()
             && connection->getState() == APP_WAIT_TASK);
      connection->forceClose();
      return true;
    }
  }
  return false;
}

void TNonblockingServer::expireClose(boost::shared_ptr<Runnable> task) {
  TConnection* connection =
    static_cast<TConnection::Task*>(task.get())->getTConnection();
  assert(connection && connection->getServer()
	 && connection->getState() == APP_WAIT_TASK);
  connection->forceClose();
}

/**
 * Main workhorse function, starts up the server listening on a port and
 * loops over the libevent handler.
 */
void TNonblockingServer::serve() {
  // Init socket
  listenSocket();

  if (threadPoolProcessing_) {
    // Init task completion notification pipe
    createNotificationPipe();
  }

  // Initialize libevent core
  registerEvents(static_cast<event_base*>(event_init()), true);

  // Run the preServe event
  if (eventHandler_ != NULL) {
    eventHandler_->preServe();
  }

  // Run libevent engine, never returns, invokes calls to eventHandler
  event_base_loop(eventBase_, 0);
}

}}} // apache::thrift::server
