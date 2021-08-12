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

#ifndef _THRIFT_ASYNC_TQIODEVICE_TRANSPORT_H_
#define _THRIFT_ASYNC_TQIODEVICE_TRANSPORT_H_ 1

#include <memory>

#include <thrift/transport/TVirtualTransport.h>

class QIODevice;

namespace apache {
namespace thrift {
namespace transport {

/**
 *  Transport that operates on a QIODevice (socket, file, etc).
 */
class TQIODeviceTransport
    : public apache::thrift::transport::TVirtualTransport<TQIODeviceTransport> {
public:
  explicit TQIODeviceTransport(std::shared_ptr<QIODevice> dev);
  ~TQIODeviceTransport() override;

  void open() override;
  bool isOpen() const override;
  bool peek() override;
  void close() override;

  uint32_t readAll(uint8_t* buf, uint32_t len);
  uint32_t read(uint8_t* buf, uint32_t len);

  void write(const uint8_t* buf, uint32_t len);
  uint32_t write_partial(const uint8_t* buf, uint32_t len);

  void flush() override;

  uint8_t* borrow(uint8_t* buf, uint32_t* len);
  void consume(uint32_t len);

private:
  TQIODeviceTransport(const TQIODeviceTransport&);
  TQIODeviceTransport& operator=(const TQIODeviceTransport&);

  std::shared_ptr<QIODevice> dev_;
};
}
}
} // apache::thrift::transport

#endif // #ifndef _THRIFT_ASYNC_TQIODEVICE_TRANSPORT_H_
