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

#ifndef _THRIFT_TRANSPORT_TWEBSOCKETSERVER_H_
#define _THRIFT_TRANSPORT_TWEBSOCKETSERVER_H_ 1

#include <cstdlib>
#include <iostream>
#include <sstream>

#include <openssl/sha.h>

#include <thrift/config.h>
#include <thrift/protocol/TProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/THttpServer.h>
#if defined(_MSC_VER) || defined(__MINGW32__)
#include <Shlwapi.h>
#define THRIFT_strncasecmp(str1, str2, len) _strnicmp(str1, str2, len)
#define THRIFT_strcasestr(haystack, needle) StrStrIA(haystack, needle)
#else
#define THRIFT_strncasecmp(str1, str2, len) strncasecmp(str1, str2, len)
#define THRIFT_strcasestr(haystack, needle) strcasestr(haystack, needle)
#endif
#if defined(__CYGWIN__)
#include <alloca.h>
#endif

using std::string;

namespace apache {
namespace thrift {
namespace transport {

std::string base64Encode(unsigned char* data, int length);

template <bool binary>
class TWebSocketServer : public THttpServer {
public:
  TWebSocketServer(std::shared_ptr<TTransport> transport, std::shared_ptr<TConfiguration> config = nullptr)
    : THttpServer(transport, config) {
      resetHandshake();
  }

  ~TWebSocketServer() override = default;

  uint32_t readAll_virt(uint8_t* buf, uint32_t len) override {
    // If we do not have a good handshake, the client will attempt one.
    if (!handshakeComplete()) {
      resetHandshake();
      THttpServer::read(buf, len);
      // If we did not get everything we expected, the handshake failed
      // and we need to send a 400 response back.
      if (!handshakeComplete()) {
        sendBadRequest();
        return 0;
      }
      // Otherwise, send back the 101 response.
      THttpServer::flush();
    }

    uint32_t want = len;
    auto have = readBuffer_.available_read();

    // If we have some data in the buffer, copy it out and return it.
    // We have to return it without attempting to read more, since we aren't
    // guaranteed that the underlying transport actually has more data, so
    // attempting to read from it could block.
    if (have > 0 && have >= want) {
      return readBuffer_.read(buf, want);
    }

    // Read another frame.
    if (!readFrame()) {
      // EOF.  No frame available.
      return 0;
    }

    // Hand over whatever we have.
    uint32_t give = (std::min)(want, readBuffer_.available_read());
    return readBuffer_.read(buf, give);
  }

  void flush() override {
    resetConsumedMessageSize();
    writeFrameHeader();
    uint8_t* buffer;
    uint32_t length;
    writeBuffer_.getBuffer(&buffer, &length);
    transport_->write(buffer, length);
    transport_->flush();
    writeBuffer_.resetBuffer();
  }

protected:
  std::string getHeader(uint32_t len) override {
    THRIFT_UNUSED_VARIABLE(len);
    std::ostringstream h;
    h << "HTTP/1.1 101 Switching Protocols" << CRLF << "Server: Thrift/" << PACKAGE_VERSION << CRLF
      << "Upgrade: websocket" << CRLF << "Connection: Upgrade" << CRLF
      << "Sec-WebSocket-Accept: " << acceptKey_ << CRLF << CRLF;
    return h.str();
  }

  void parseHeader(char* header) override {
    char* colon = strchr(header, ':');
    if (colon == nullptr) {
      return;
    }
    size_t sz = colon - header;
    char* value = colon + 1;

    if (THRIFT_strncasecmp(header, "Upgrade", sz) == 0) {
      if (THRIFT_strcasestr(value, "websocket") != nullptr) {
        upgrade_ = true;
      }
    } else if (THRIFT_strncasecmp(header, "Connection", sz) == 0) {
      if (THRIFT_strcasestr(value, "Upgrade") != nullptr) {
        connection_ = true;
      }
    } else if (THRIFT_strncasecmp(header, "Sec-WebSocket-Key", sz) == 0) {
      std::string toHash = value + 1;
      toHash += "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
      unsigned char hash[20];
      SHA1((const unsigned char*)toHash.c_str(), toHash.length(), hash);
      acceptKey_ = base64Encode(hash, 20);
      secWebSocketKey_ = true;
    } else if (THRIFT_strncasecmp(header, "Sec-WebSocket-Version", sz) == 0) {
      if (THRIFT_strcasestr(value, "13") != nullptr) {
        secWebSocketVersion_ = true;
      }
    }
  }

  bool parseStatusLine(char* status) override {
    char* method = status;

    char* path = strchr(method, ' ');
    if (path == nullptr) {
      throw TTransportException(string("Bad Status: ") + status);
    }

    *path = '\0';
    while (*(++path) == ' ') {
    };

    char* http = strchr(path, ' ');
    if (http == nullptr) {
      throw TTransportException(string("Bad Status: ") + status);
    }
    *http = '\0';

    if (strcmp(method, "GET") == 0) {
      // GET method ok, looking for content.
      return true;
    }
    throw TTransportException(string("Bad Status (unsupported method): ") + status);
  }

private:
  enum class CloseCode : uint16_t {
    NormalClosure = 1000,
    GoingAway = 1001,
    ProtocolError = 1002,
    UnsupportedDataType = 1003,
    NoStatusCode = 1005,
    AbnormalClosure = 1006,
    InvalidData = 1007,
    PolicyViolation = 1008,
    MessageTooBig = 1009,
    ExtensionExpected = 1010,
    UnexpectedError = 1011,
    NotSecure = 1015
  };

  enum class Opcode : uint8_t {
    Continuation = 0x0,
    Text = 0x1,
    Binary = 0x2,
    Close = 0x8,
    Ping = 0x9,
    Pong = 0xA
  };

  void failConnection(CloseCode reason) {
    writeFrameHeader(Opcode::Close);
    auto buffer = htons(static_cast<uint16_t>(reason));
    transport_->write(reinterpret_cast<const uint8_t*>(&buffer), 2);
    transport_->flush();
    transport_->close();
  }

  bool handshakeComplete() {
    return upgrade_ && connection_ && secWebSocketKey_ && secWebSocketVersion_;
  }

  void pong() {
    writeFrameHeader(Opcode::Pong);
    uint8_t* buffer;
    uint32_t size;
    readBuffer_.getBuffer(&buffer, &size);
    transport_->write(buffer, size);
    transport_->flush();
  }

  bool readFrame() {
    uint8_t headerBuffer[8];

    auto read = transport_->read(headerBuffer, 2);
    if (read < 2) {
      return false;
    }
    // Since Thrift has its own message end marker and we read frame by frame,
    // it doesn't really matter if the frame is marked as FIN.
    // Capture it only for debugging only.
    auto fin = (headerBuffer[0] & 0x80) != 0;
    THRIFT_UNUSED_VARIABLE(fin);

    // RSV1, RSV2, RSV3
    if ((headerBuffer[0] & 0x70) != 0) {
      failConnection(CloseCode::ProtocolError);
      throw TTransportException(TTransportException::CORRUPTED_DATA,
                                "Reserved bits must be zeroes");
    }

    auto opcode = (Opcode)(headerBuffer[0] & 0x0F);

    // Mask
    if ((headerBuffer[1] & 0x80) == 0) {
      failConnection(CloseCode::ProtocolError);
      throw TTransportException(TTransportException::CORRUPTED_DATA,
                                "Messages from the client must be masked");
    }

    // Read the length
    uint64_t payloadLength = headerBuffer[1] & 0x7F;
    if (payloadLength == 126) {
      read = transport_->read(headerBuffer, 2);
      if (read < 2) {
        return false;
      }
      payloadLength = ntohs(*reinterpret_cast<uint16_t*>(headerBuffer));
    } else if (payloadLength == 127) {
      read = transport_->read(headerBuffer, 8);
      if (read < 8) {
        return false;
      }
      payloadLength = THRIFT_ntohll(*reinterpret_cast<uint64_t*>(headerBuffer));
      if ((payloadLength & 0x8000000000000000) != 0) {
        failConnection(CloseCode::ProtocolError);
        throw TTransportException(
            TTransportException::CORRUPTED_DATA,
            "The most significant bit of the payload length must be zero");
      }
    }

    // size_t is smaller than a ulong on a 32-bit system
    if (payloadLength > UINT32_MAX) {
      failConnection(CloseCode::MessageTooBig);
      return false;
    }

    auto length = static_cast<uint32_t>(payloadLength);

    if (length > 0) {
      // Read the masking key
      read = transport_->read(headerBuffer, 4);
      if (read < 4) {
        return false;
      }

      readBuffer_.resetBuffer(length);
      uint8_t* buffer = readBuffer_.getWritePtr(length);
      read = transport_->read(buffer, length);
      readBuffer_.wroteBytes(read);
      if (read < length) {
        return false;
      }
      
      // Unmask the data
      for (size_t i = 0; i < length; i++) {
        buffer[i] ^= headerBuffer[i % 4];
      }

      T_DEBUG("FIN=%d, Opcode=%X, length=%d, payload=%s", fin, opcode, length,
              binary ? readBuffer_.toHexString() : cast(string) readBuffer_);
    }

    switch (opcode) {
    case Opcode::Close:
      if (length >= 2) {
        uint8_t buffer[2];
        readBuffer_.read(buffer, 2);
        CloseCode closeCode = static_cast<CloseCode>(ntohs(*reinterpret_cast<uint16_t*>(buffer)));
        THRIFT_UNUSED_VARIABLE(closeCode);
        string closeReason = readBuffer_.readAsString(length - 2);
        T_DEBUG("Connection closed: %d %s", closeCode, closeReason);
      }
      transport_->close();
      return false;
    case Opcode::Ping:
      pong();
      return readFrame();
    default:
      return true;
    }
  }

  void resetHandshake() {
    connection_ = false;
    secWebSocketKey_ = false;
    secWebSocketVersion_ = false;
    upgrade_ = false;
  }

  void sendBadRequest() {
    std::ostringstream h;
    h << "HTTP/1.1 400 Bad Request" << CRLF << "Server: Thrift/" << PACKAGE_VERSION << CRLF << CRLF;
    std::string header = h.str();
    transport_->write(reinterpret_cast<const uint8_t*>(header.data()), static_cast<uint32_t>(header.length()));
    transport_->flush();
    transport_->close();
  }

  void writeFrameHeader(Opcode opcode = Opcode::Continuation) {
    uint32_t headerSize = 1;
    uint32_t length = writeBuffer_.available_read();
    if (length < 126) {
      ++headerSize;
    } else if (length < 65536) {
      headerSize += 3;
    } else {
      headerSize += 9;
    }
    // The server does not mask the response

    uint8_t* header = static_cast<uint8_t*>(alloca(headerSize));
    if (opcode == Opcode::Continuation) {
      opcode = binary ? Opcode::Binary : Opcode::Text;
    }
    header[0] = static_cast<uint8_t>(opcode) | 0x80;
    if (length < 126) {
      header[1] = static_cast<uint8_t>(length);
    } else if (length < 65536) {
      header[1] = 126;
      *reinterpret_cast<uint16_t*>(header + 2) = htons(length);
    } else {
      header[1] = 127;
      *reinterpret_cast<uint64_t*>(header + 2) = THRIFT_htonll(length);
    }

    transport_->write(header, headerSize);
  }

  // Add constant here to avoid a linker error on Windows
  constexpr static const char* CRLF = "\r\n";
  std::string acceptKey_;
  bool connection_;
  bool secWebSocketKey_;
  bool secWebSocketVersion_;
  bool upgrade_;
};

/**
 * Wraps a transport into binary WebSocket protocol
 */
class TBinaryWebSocketServerTransportFactory : public TTransportFactory {
public:
  TBinaryWebSocketServerTransportFactory() = default;

  ~TBinaryWebSocketServerTransportFactory() override = default;

  /**
   * Wraps the transport into a buffered one.
   */
  std::shared_ptr<TTransport> getTransport(std::shared_ptr<TTransport> trans) override {
    return std::shared_ptr<TTransport>(new TWebSocketServer<true>(trans));
  }
};

/**
 * Wraps a transport into text WebSocket protocol
 */
class TTextWebSocketServerTransportFactory : public TTransportFactory {
public:
  TTextWebSocketServerTransportFactory() = default;

  ~TTextWebSocketServerTransportFactory() override = default;

  /**
   * Wraps the transport into a buffered one.
   */
  std::shared_ptr<TTransport> getTransport(std::shared_ptr<TTransport> trans) override {
    return std::shared_ptr<TTransport>(new TWebSocketServer<false>(trans));
  }
};
} // namespace transport
} // namespace thrift
} // namespace apache
#endif
