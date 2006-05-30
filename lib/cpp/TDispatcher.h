#ifndef T_DISPATCHER_H
#define T_DISPATCHER_H

#include <string>

/**
 * A dispatcher is a generic object that accepts an input buffer and returns
 * a buffer. It can be used in a variety of ways, i.e. as a client that
 * sends data over the network and returns a response, or as a server that
 * reads an input and returns an output.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TDispatcher {
 public:
  virtual ~TDispatcher() {};
  virtual std::string dispatch(const std::string& s) = 0;
 protected:
  TDispatcher() {}
};

#endif
