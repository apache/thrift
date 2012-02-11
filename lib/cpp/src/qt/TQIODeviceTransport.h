#ifndef _THRIFT_ASYNC_TQIODEVICE_TRANSPORT_H_
#define _THRIFT_ASYNC_TQIODEVICE_TRANSPORT_H_ 1

#include <boost/shared_ptr.hpp>

#include <transport/TVirtualTransport.h>

class QIODevice;

namespace apache { namespace thrift { namespace transport {

/**
 *  Transport that operates on a QIODevice (socket, file, etc).
 */
class TQIODeviceTransport : public apache::thrift::transport::TVirtualTransport<TQIODeviceTransport> {
 public:
  explicit TQIODeviceTransport(boost::shared_ptr<QIODevice> dev);
  virtual ~TQIODeviceTransport();

  void open();
  bool isOpen();
  bool peek();
  void close();

  uint32_t readAll(uint8_t *buf, uint32_t len);
  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);
  uint32_t write_partial(const uint8_t* buf, uint32_t len);

  void flush();

  uint8_t* borrow(uint8_t* buf, uint32_t* len);
  void consume(uint32_t len);

 private:
   TQIODeviceTransport(const TQIODeviceTransport&);
   TQIODeviceTransport& operator=(const TQIODeviceTransport&);
   
   boost::shared_ptr<QIODevice> dev_;
};
}}} // apache::thrift::transport

#endif // #ifndef _THRIFT_ASYNC_TQIODEVICE_TRANSPORT_H_

