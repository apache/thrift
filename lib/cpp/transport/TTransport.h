#ifndef T_TRANSPORT_H
#define T_TRANSPORT_H

#include <string>

/**
 * Generic interface for a method of transporting data.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TTransport {
 public:
  virtual ~TTransport() {};

  virtual bool open() = 0;
  virtual void close() = 0;

  virtual int  read (std::string& s, uint32_t size) = 0;
  virtual void write(const std::string& s) = 0;

 protected:
  TTransport() {};
};

#endif
