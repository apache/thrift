#include "TNonblockingServer.h"

#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>

namespace facebook { namespace thrift { namespace server { 

void TConnection::init(int socket, short eventFlags, TNonblockingServer* s) {
  socket_ = socket;
  server_ = s;
  appState_ = APP_INIT;
  eventFlags_ = 0;

  readBufferPos_ = 0;
  readWant_ = 0;

  writeBuffer_ = NULL;
  writeBufferSize_ = 0;
  writeBufferPos_ = 0;

  socketState_ = SOCKET_RECV;
  appState_ = APP_INIT;
  
  // Set flags, which also registers the event
  setFlags(eventFlags);
}

void TConnection::workSocket() {
  int flags;

  switch (socketState_) {
  case SOCKET_RECV:
    // It is an error to be in this state if we already have all the data
    assert(readBufferPos_ < readWant_);

    // How much space is availble, and how much will we fetch
    uint32_t avail = readBufferSize_ - readBufferPos_;
    uint32_t fetch = readWant_ - readBufferPos_;

    // Double the buffer size until it is big enough
    if (fetch > avail) {
      while (fetch > avail) {
        readBufferSize_ *= 2;
      }
      readBuffer_ = (uint8_t*)realloc(readBuffer_, readBufferSize_);
      if (readBuffer_ == NULL) {
        perror("TConnection::workSocket() realloc");
        close();
        return;
      }
    }

    // Read from the socket
    int got = recv(socket_, readBuffer_ + readBufferPos_, fetch, 0);
    
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
    } else if (got == -1) {
      // Blocking errors are okay, just move on
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        return;
      }

      if (errno != ECONNRESET) {
        perror("TConnection::workSocket() recv -1");
      }
    }

    // Whenever we get down here it means a remote disconnect
    close();
    
    return;

  case SOCKET_SEND:
    // Should never have position past size
    assert(writeBufferPos_ <= writeBufferSize_);

    // If there is no data to send, then let us move on
    if (writeBufferPos_ == writeBufferSize_) {
      fprintf(stderr, "WARNING: Send state with no data to send\n");
      transition();
      return;
    }

    flags = 0;
    #ifdef MSG_NOSIGNAL
    // Note the use of MSG_NOSIGNAL to suppress SIGPIPE errors, instead we
    // check for the EPIPE return condition and close the socket in that case
    flags |= MSG_NOSIGNAL;
    #endif // ifdef MSG_NOSIGNAL

    int left = writeBufferSize_ - writeBufferPos_;
    int sent = send(socket_, writeBuffer_ + writeBufferPos_, left, flags);

    if (sent <= 0) {
      // Blocking errors are okay, just move on
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        return;
      }
      if (errno != EPIPE) {
        perror("TConnection::workSocket() send -1");
      }
      close();
      return;
    }

    writeBufferPos_ += sent;

    // Did we overdo it?
    assert(writeBufferPos_ <= writeBufferSize_);

    // We are  done!
    if (writeBufferPos_ == writeBufferSize_) {
      transition();
    }

    return;

  default:
    fprintf(stderr, "Shit Got Ill. Socket State %d\n", socketState_);
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

    try {
      // Invoke the processor
      server_->getProcessor()->process(inputProtocol_, outputProtocol_);
    } catch (TTransportException &x) {
      fprintf(stderr, "Server::process %s\n", x.getMessage().c_str());
      close();
      return;    
    } catch (...) {
      fprintf(stderr, "Server::process() unknown exception\n");
      close();
      return;
    }

    // Get the result of the operation
    outputTransport_->getBuffer(&writeBuffer_, &writeBufferSize_);

    // If the function call generated return data, then move into the send
    // state and get going
    if (writeBufferSize_ > 0) {

      // Move into write state
      writeBufferPos_ = 0;
      socketState_ = SOCKET_SEND;

      if (server_->getFrameResponses()) {
        // Put the frame size into the write buffer
        appState_ = APP_SEND_FRAME_SIZE;
        frameSize_ = (int32_t)htonl(writeBufferSize_);
        writeBuffer_ = (uint8_t*)&frameSize_;
        writeBufferSize_ = 4;
      } else {
        // Go straight into sending the result, do not frame it
        appState_ = APP_SEND_RESULT;
      }

      // Socket into write mode
      setWrite();

      // Try to work the socket immediately
      workSocket();

      return;
    }

    // In this case, the request was asynchronous and we should fall through
    // right back into the read frame header state
    goto LABEL_APP_INIT;

  case APP_SEND_FRAME_SIZE:

    // Refetch the result of the operation since we put the frame size into
    // writeBuffer_
    outputTransport_->getBuffer(&writeBuffer_, &writeBufferSize_);
    writeBufferPos_ = 0;

    // Now in send result state
    appState_ = APP_SEND_RESULT;

    // Go to work on the socket right away, probably still writeable
    workSocket();

    return;

  case APP_SEND_RESULT:

    // N.B.: We also intentionally fall through here into the INIT state!

  LABEL_APP_INIT:
  case APP_INIT:

    // Clear write buffer variables
    writeBuffer_ = NULL;
    writeBufferPos_ = 0;
    writeBufferSize_ = 0;

    // Set up read buffer for getting 4 bytes
    readBufferPos_ = 0;
    readWant_ = 4;

    // Into read4 state we go
    socketState_ = SOCKET_RECV;
    appState_ = APP_READ_FRAME_SIZE;

    // Register read event
    setRead();

    // Try to work the socket right away
    workSocket();

    return;

  case APP_READ_FRAME_SIZE:
    // We just read the request length, deserialize it
    int sz = *(int32_t*)readBuffer_;
    sz = (int32_t)ntohl(sz);

    if (sz <= 0) {
      fprintf(stderr, "TConnection:transition() Negative frame size %d, remote side not using TFramedTransport?", sz);
      close();
      return;
    }

    // Reset the read buffer
    readWant_ = (uint32_t)sz;
    readBufferPos_= 0;

    // Move into read request state
    appState_ = APP_READ_REQUEST;

    // Work the socket right away
    workSocket();

    return;

  default:
    fprintf(stderr, "Totally Fucked. Application State %d\n", appState_);
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
      perror("TConnection::setFlags event_del");
      return;
    }
  }

  // Update in memory structure
  eventFlags_ = eventFlags;

  /**
   * event_set:
   *
   * Prepares the event structure &event to be used in future calls to
   * event_add() and event_del().  The event will be prepared to call the
   * event_handler using the 'sock' file descriptor to monitor events.
   *
   * The events can be either EV_READ, EV_WRITE, or both, indicating
   * that an application can read or write from the file respectively without
   * blocking.
   *
   * The event_handler will be called with the file descriptor that triggered
   * the event and the type of event which will be one of: EV_TIMEOUT,
   * EV_SIGNAL, EV_READ, EV_WRITE.
   *
   * The additional flag EV_PERSIST makes an event_add() persistent until
   * event_del() has been called.
   *
   * Once initialized, the &event struct can be used repeatedly with
   * event_add() and event_del() and does not need to be reinitialized unless
   * the event_handler and/or the argument to it are to be changed.  However,
   * when an ev structure has been added to libevent using event_add() the
   * structure must persist until the event occurs (assuming EV_PERSIST
   * is not set) or is removed using event_del().  You may not reuse the same
   * ev structure for multiple monitored descriptors; each descriptor needs
   * its own ev.
   */
  event_set(&event_, socket_, eventFlags_, TConnection::eventHandler, this);

  // Add the event
  if (event_add(&event_, 0) == -1) {
    perror("TConnection::setFlags(): coult not event_add");
  }
}

/**
 * Closes a connection
 */
void TConnection::close() {
  // Delete the registered libevent
  if (event_del(&event_) == -1) {
    perror("TConnection::close() event_del");
  }

  // Close the socket
  if (socket_ > 0) {
    ::close(socket_);
  }
  socket_ = 0;

  // Give this object back to the server that owns it
  server_->returnConnection(this);
}

/**
 * Creates a new connection either by reusing an object off the stack or
 * by allocating a new one entirely
 */
TConnection* TNonblockingServer::createConnection(int socket, short flags) {
  // Check the stack
  if (connectionStack_.empty()) {
    return new TConnection(socket, flags, this);
  } else {
    TConnection* result = connectionStack_.top();
    connectionStack_.pop();
    result->init(socket, flags, this);
    return result;
  }
}

/**
 * Returns a connection to the stack
 */
void TNonblockingServer::returnConnection(TConnection* connection) {
  connectionStack_.push(connection);
}

/**
 * Server socket had something happen
 */
void TNonblockingServer::handleEvent(int fd, short which) {
  // Make sure that libevent didn't fuck up the socket handles
  assert(fd == serverSocket_);
  
  // Server socket accepted a new connection
  socklen_t addrLen;
  struct sockaddr addr;
  addrLen = sizeof(addr);   
  
  // Going to accept a new client socket
  int clientSocket;
  
  // Accept as many new clients as possible, even though libevent signaled only
  // one, this helps us to avoid having to go back into the libevent engine so
  // many times
  while ((clientSocket = accept(fd, &addr, &addrLen)) != -1) {

    // Explicitly set this socket to NONBLOCK mode
    int flags;
    if ((flags = fcntl(clientSocket, F_GETFL, 0)) < 0 ||
        fcntl(clientSocket, F_SETFL, flags | O_NONBLOCK) < 0) {
      perror("thriftServerEventHandler: set O_NONBLOCK");
      close(clientSocket);
      return;
    }

    // Create a new TConnection for this client socket.
    TConnection* clientConnection =
      createConnection(clientSocket, EV_READ | EV_PERSIST);

    // Fail fast if we could not create a TConnection object
    if (clientConnection == NULL) {
      fprintf(stderr, "thriftServerEventHandler: failed TConnection factory");
      close(clientSocket);
      return;
    }

    // Put this client connection into the proper state
    clientConnection->transition();
  }
  
  // Done looping accept, now we have to make sure the error is due to
  // blocking. Any other error is a problem
  if (errno != EAGAIN && errno != EWOULDBLOCK) {
    perror("thriftServerEventHandler: accept()");
  }
}

/**
 * Main workhorse function, starts up the server listening on a port and
 * loops over the libevent handler.
 */
void TNonblockingServer::serve() {
  // Initialize libevent
  event_init();

  // Print some libevent stats
  fprintf(stderr,
          "libevent %s method %s\n",
          event_get_version(),
          event_get_method());

  // Create the server socket
  serverSocket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (serverSocket_ == -1) {
    perror("TNonblockingServer::serve() socket() -1");
    return;
  }

  // Set socket to nonblocking mode
  int flags;
  if ((flags = fcntl(serverSocket_, F_GETFL, 0)) < 0 ||
      fcntl(serverSocket_, F_SETFL, flags | O_NONBLOCK) < 0) {
    perror("TNonblockingServer::serve() O_NONBLOCK");
    ::close(serverSocket_);
    return;
  }

  int one = 1;
  struct linger ling = {0, 0};
  
  // Set reuseaddr to avoid 2MSL delay on server restart
  setsockopt(serverSocket_, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

  // Keepalive to ensure full result flushing
  setsockopt(serverSocket_, SOL_SOCKET, SO_KEEPALIVE, &one, sizeof(one));

  // Turn linger off to avoid hung sockets
  setsockopt(serverSocket_, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling));

  // Set TCP nodelay if available, MAC OS X Hack
  // See http://lists.danga.com/pipermail/memcached/2005-March/001240.html
  #ifndef TCP_NOPUSH
  setsockopt(serverSocket_, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
  #endif

  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port_);
  addr.sin_addr.s_addr = INADDR_ANY;

  if (bind(serverSocket_, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
    perror("TNonblockingServer::serve() bind");
    close(serverSocket_);
    return;
  }

  if (listen(serverSocket_, LISTEN_BACKLOG) == -1) {
    perror("TNonblockingServer::serve() listen");
    close(serverSocket_);
    return;
  }

  // Register the server event
  struct event serverEvent;
  event_set(&serverEvent,
            serverSocket_,
            EV_READ | EV_PERSIST,
            TNonblockingServer::eventHandler,
            this);

  // Add the event and start up the server
  if (event_add(&serverEvent, 0) == -1) {
    perror("TNonblockingServer::serve(): coult not event_add");
    return;
  }

  // Run libevent engine, never returns, invokes calls to event_handler
  event_loop(0);
}

}}} // facebook::thrift::server
