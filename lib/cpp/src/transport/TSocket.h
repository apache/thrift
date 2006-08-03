#ifndef T_SOCKET_H
#define T_SOCKET_H

#include <string>

#include "transport/TTransport.h"
#include "transport/TServerSocket.h"

namespace facebook { namespace thrift { namespace transport { 

/**
 * TCP Socket implementation of the TTransport interface.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TSocket : public TTransport {
  /**
   * We allow the TServerSocket acceptImpl() method to access the private
   * members of a socket so that it can access the TSocket(int socket)
   * constructor which creates a socket object from the raw UNIX socket
   * handle.
   */
  friend class TServerSocket;

 public:
  /**
   * Constructs a new socket. Note that this does NOT actually connect the
   * socket.
   *
   * @param host An IP address or hostname to connect to
   * @param port The port to connect on
   */
  TSocket(std::string host, int port);

  /**
   * Destroyes the socket object, closing it if necessary.
   */
  ~TSocket();

  /**
   * Whether the socket is alive.
   *
   * @return Is the socket alive?
   */
  bool isOpen();

  /**
   * Creates and opens the UNIX socket.
   *
   * @throws TTransportException If the socket could not connect
   */
  void open();

  /**
   * Shuts down communications on the socket.
   */
  void close();

  /**
   * Reads from the underlying socket.
   */
  uint32_t read(uint8_t* buf, uint32_t len);

  /**
   * Writes to the underlying socket.
   */
  void write(const uint8_t* buf, uint32_t len);

  /**
   * Controls whether the linger option is set on the socket.
   *
   * @param on      Whether SO_LINGER is on
   * @param linger  If linger is active, the number of seconds to linger for
   */
  void setLinger(bool on, int linger);

  /**
   * Whether to enable/disable Nagle's algorithm.
   *
   * @param noDelay Whether or not to disable the algorithm.
   * @return 
   */
  void setNoDelay(bool noDelay);

 private:
  /**
   * Constructor to create socket from raw UNIX handle. Never called directly
   * but used by the TServerSocket class.
   */
  TSocket(int socket);

  /** Host to connect to */
  std::string host_;

  /** Port number to connect on */
  int port_;

  /** Underlying UNIX socket handle */
  int socket_;
};

}}} // facebook::thrift::transport
#endif
