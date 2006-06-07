#ifndef T_SERVER_TRANSPORT_H
#define T_SERVER_TRANSPORT_H

#include "transport/TTransport.h"
#include "transport/TTransportException.h"

/**
 * Server transport framework. A server needs to have some facility for
 * creating base transports to read/write from.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServerTransport {
 public:
  virtual ~TServerTransport() {}

  /**
   * Starts the server transport listening for new connections. Prior to this
   * call most transports will not return anything when accept is called.
   *
   * @throws TTransportException if we were unable to listen
   */
  virtual void listen() {}

  /**
   * Gets a new dynamically allocated transport object and passes it to the
   * caller. Note that it is the explicit duty of the caller to free the
   * allocated object. The returned TTransport object must always be in the
   * opened state. NULL should never be returned, instead an Exception should
   * always be thrown.
   *
   * @return A new TTransport object
   * @throws TTransportException if there is an error
   */
  TTransport* accept() {
    TTransport* result = acceptImpl();
    if (result == NULL) {
      throw TTransportException("accept() may not return NULL");
    }
    return result;
  }

  /**
   * Closes this transport such that future calls to accept will do nothing.
   */
  virtual void close() = 0;

 protected:
  TServerTransport() {}

  /**
   * Subclasses should implement this function for accept.
   *
   * @return A newly allocated TTransport object
   * @throw TTransportException If an error occurs
   */
  virtual TTransport* acceptImpl() = 0;

};

#endif
