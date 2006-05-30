#ifndef T_SERVER_TRANSPORT_H
#define T_SERVER_TRANSPORT_H

#include "TTransport.h"

/**
 * Server transport framework. A server needs to have some facility for
 * creating base transports to read/write from.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServerTransport {
 public:
  virtual ~TServerTransport() {}

  virtual bool listen() = 0;
  virtual TTransport* accept() = 0;
  virtual void close() = 0;

 protected:
  TServerTransport() {}
};

#endif
