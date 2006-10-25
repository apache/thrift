#ifndef _THRIFT_SERVER_TNONBLOCKINGSERVER_H_
#define _THRIFT_SERVER_TNONBLOCKINGSERVER_H_ 1

#include "Thrift.h"
#include "server/TServer.h"
#include "transport/TMemoryBuffer.h"
#include <stack>
#include <event.h>

namespace facebook { namespace thrift { namespace server { 

using boost::shared_ptr;

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
 * @author Mark Slee <mcslee@facebook.com>
 */
class TNonblockingServer : public TServer {
 private:

  // Listen backlog
  static const int LISTEN_BACKLOG = 1024;

  // Server socket file descriptor
  int serverSocket_;

  // Port server runs on
  int port_;

  // Whether to frame responses
  bool frameResponses_;

  /**
   * This is a stack of all the objects that have been created but that
   * are NOT currently in use. When we close a connection, we place it on this
   * stack so that the object can be reused later, rather than freeing the
   * memory and reallocating a new object later.
   */
  std::stack<TConnection*> connectionStack_;

  void handleEvent(int fd, short which);

 public:
  TNonblockingServer(shared_ptr<TProcessor> processor,
                     shared_ptr<TServerOptions> options,
                     int port) :
    TServer(processor, options),
    serverSocket_(0),
    port_(port),
    frameResponses_(true) {}
    
  ~TNonblockingServer() {}

  void setFrameResponses(bool frameResponses) {
    frameResponses_ = frameResponses;
  }

  bool getFrameResponses() {
    return frameResponses_;
  }

  TConnection* createConnection(int socket, short flags);

  void returnConnection(TConnection* connection);

  static void eventHandler(int fd, short which, void* v) {
    ((TNonblockingServer*)v)->handleEvent(fd, which);
  }

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
  APP_SEND_FRAME_SIZE,
  APP_SEND_RESULT
};

/**
 * Represents a connection that is handled via libevent. This connection
 * essentially encapsulates a socket that has some associated libevent state.
 */
class TConnection {
 private:

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

  // Frame size
  int32_t frameSize_;

  // Transport to read from
  shared_ptr<TMemoryBuffer> inputTransport_;

  // Transport that processor writes to
  shared_ptr<TMemoryBuffer> outputTransport_;

  // Go into read mode
  void setRead() {
    setFlags(EV_READ | EV_PERSIST);
  }

  // Go into write mode
  void setWrite() {
    setFlags(EV_WRITE | EV_PERSIST);
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
    readBuffer_ = (uint8_t*)malloc(1024);
    if (readBuffer_ == NULL) {
      throw new facebook::thrift::Exception("Out of memory.");
    }
    readBufferSize_ = 1024;
    
    // Allocate input and output tranpsorts
    inputTransport_ = shared_ptr<TMemoryBuffer>(new TMemoryBuffer(readBuffer_, readBufferSize_));
    outputTransport_ = shared_ptr<TMemoryBuffer>(new TMemoryBuffer());
    
    init(socket, eventFlags, s);
  }

  // Initialize
  void init(int socket, short eventFlags, TNonblockingServer *s);

  // Transition into a new state
  void transition();

  // Handler wrapper
  static void eventHandler(int fd, short which, void* v) {
    assert(fd = ((TConnection*)v)->socket_);
    ((TConnection*)v)->workSocket();
  }
};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
