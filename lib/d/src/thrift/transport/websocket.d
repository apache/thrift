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
module thrift.transport.websocket;

import std.algorithm;
import std.algorithm.searching;
import std.base64;
import std.bitmanip;
import std.conv;
import std.digest.sha;
import std.stdio;
import std.string;
import std.uni;
import thrift.base : VERSION;
import thrift.transport.base;
import thrift.transport.http;

/**
 * WebSocket server transport.
 */
final class TServerWebSocketTransport(bool binary) : THttpTransport {
  /**
   * Constructs a new instance.
   *
   * Param:
   *   transport = The underlying transport used for the actual I/O.
   */
  this(TTransport transport) {
    super(transport);
    transport_ = transport;
  }

  override size_t read(ubyte[] buf) {
    // If we do not have a good handshake, the client will attempt one.
    if (!handshakeComplete) {
      resetHandshake();
      super.read(buf);
      // If we did not get everything we expected, the handshake failed
      // and we need to send a 400 response back.
      if (!handshakeComplete) {
        sendBadRequest();
        return 0;
      }
      // Otherwise, send back the 101 response.
      super.flush();
    }

    // If the buffer is empty, read a new frame off the wire.
    if (readBuffer_.empty) {
      if (!readFrame()) {
        return 0;
      }
    }

    auto size = min(readBuffer_.length, buf.length);
    buf[0..size] = readBuffer_[0..size];
    readBuffer_ = readBuffer_[size..$];
    return size;
  }

  override void write(in ubyte[] buf) {
    writeBuffer_ ~= buf;
  }

  override void flush() {
    if (writeBuffer_.empty) {
      return;
    }

    // Properly reset the write buffer even some of the protocol operations go
    // wrong.
    scope (exit) {
      writeBuffer_.length = 0;
      writeBuffer_.assumeSafeAppend();
    }

    writeFrameHeader();
    transport_.write(writeBuffer_);
    transport_.flush();
  }

protected:
  override string getHeader(size_t dataLength) {
    return "HTTP/1.1 101 Switching Protocols\r\n" ~
      "Server: Thrift/" ~ VERSION ~ "\r\n" ~
      "Upgrade: websocket\r\n" ~
      "Connection: Upgrade\r\n" ~
      "Sec-WebSocket-Accept: " ~ acceptKey_ ~ "\r\n" ~
      "\r\n";
  }

  override void parseHeader(const(ubyte)[] header) {
    auto split = findSplit(header, [':']);
    if (split[1].empty) {
      // No colon found.
      return;
    }

    static bool compToLower(ubyte a, ubyte b) {
      return toLower(a) == toLower(b);
    }

    if (startsWith!compToLower(split[0], cast(ubyte[])"upgrade")) {
      auto upgrade = stripLeft(cast(const(char)[])split[2]);
      upgrade_ = sicmp(upgrade, "websocket") == 0;
    } else if (startsWith!compToLower(split[0], cast(ubyte[])"connection")) {
      auto connection = stripLeft(cast(const(char)[])split[2]);
      connection_ = canFind(connection.toLower, "upgrade");
    } else if (startsWith!compToLower(split[0], cast(ubyte[])"sec-websocket-key")) {
      auto secWebSocketKey = stripLeft(cast(const(char)[])split[2]);
      auto hash = sha1Of(secWebSocketKey ~ WEBSOCKET_GUID);
      acceptKey_ = Base64.encode(hash);
      secWebSocketKey_ = true;
    } else if (startsWith!compToLower(split[0], cast(ubyte[])"sec-websocket-version")) {
      auto secWebSocketVersion = stripLeft(cast(const(char)[])split[2]);
      secWebSocketVersion_ = sicmp(secWebSocketVersion, "13") == 0;
    }
  }

  override bool parseStatusLine(const(ubyte)[] status) {
    // Method SP Request-URI SP HTTP-Version CRLF.
    auto split = findSplit(status, [' ']);
    if (split[1].empty) {
      throw new TTransportException("Bad status: " ~ to!string(status),
        TTransportException.Type.CORRUPTED_DATA);
    }

    auto uriVersion = split[2][countUntil!"a != b"(split[2], ' ') .. $];
    if (!canFind(uriVersion, ' ')) {
      throw new TTransportException("Bad status: " ~ to!string(status),
        TTransportException.Type.CORRUPTED_DATA);
    }

    if (split[0] == "GET") {
      // GET method ok, looking for content.
      return true;
    }

    throw new TTransportException("Bad status (unsupported method): " ~
      to!string(status), TTransportException.Type.CORRUPTED_DATA);
  }

private:
  @property bool handshakeComplete() { 
    return upgrade_ && connection_ && secWebSocketKey_ && secWebSocketVersion_;
  }

  void failConnection(CloseCode reason) {
    writeFrameHeader(Opcode.Close);
    transport_.write(nativeToBigEndian!ushort(reason));
    transport_.flush();
    transport_.close();
  }

  void pong() {
    writeFrameHeader(Opcode.Pong);
    transport_.write(readBuffer_);
    transport_.flush();
  }

  bool readFrame() {
    ubyte[8] headerBuffer;

    auto read = transport_.read(headerBuffer[0..2]);
    if (read < 2) {
      return false;
    }
    // Since Thrift has its own message end marker and we read frame by frame,
    // it doesn't really matter if the frame is marked as FIN.
    // Capture it only for debugging only.
    debug auto fin = (headerBuffer[0] & 0x80) != 0;

    // RSV1, RSV2, RSV3
    if ((headerBuffer[0] & 0x70) != 0) {
      failConnection(CloseCode.ProtocolError);
      throw new TTransportException("Reserved bits must be zeroes", TTransportException.Type.CORRUPTED_DATA);
    }

    Opcode opcode;
    try {
      opcode = to!Opcode(headerBuffer[0] & 0x0F);
    } catch (ConvException) {
      failConnection(CloseCode.ProtocolError);
      throw new TTransportException("Unknown opcode", TTransportException.Type.CORRUPTED_DATA);
    }

    // Mask
    if ((headerBuffer[1] & 0x80) == 0) {
      failConnection(CloseCode.ProtocolError);
      throw new TTransportException("Messages from the client must be masked", TTransportException.Type.CORRUPTED_DATA);
    }

    // Read the length
    ulong payloadLength = headerBuffer[1] & 0x7F;
    if (payloadLength == 126) {
      read = transport_.read(headerBuffer[0..2]);
      if (read < 2) {
        return false;
      }
      payloadLength = bigEndianToNative!ushort(headerBuffer[0..2]);
    } else if (payloadLength == 127) {
      read = transport_.read(headerBuffer);
      if (read < headerBuffer.length) {
        return false;
      }
      payloadLength = bigEndianToNative!ulong(headerBuffer);
      if ((payloadLength & 0x8000000000000000) != 0) {
        failConnection(CloseCode.ProtocolError);
        throw new TTransportException("The most significant bit of the payload length must be zero", 
          TTransportException.Type.CORRUPTED_DATA);
      }
    }

    // size_t is smaller than a ulong on a 32-bit system
    static if (size_t.max < ulong.max) {
      if(payloadLength > size_t.max) {
        failConnection(CloseCode.MessageTooBig);
        return false;
      }
    }

    auto length = cast(size_t)payloadLength;

    if (length > 0) {
      // Read the masking key
      read = transport_.read(headerBuffer[0..4]);
      if (read < 4) {
        return false;
      }

      readBuffer_ = new ubyte[](length);
      read = transport_.read(readBuffer_);
      if (read < length) {
        return false;
      }

      // Unmask the data
      for (size_t i = 0; i < length; i++) {
        readBuffer_[i] ^= headerBuffer[i % 4];
      }

      debug writef("FIN=%d, Opcode=%X, length=%d, payload=%s\n",
          fin,
          opcode,
          length,
          binary ? readBuffer_.toHexString() : cast(string)readBuffer_);
    }

    switch (opcode) {
      case Opcode.Close:
        debug {
          if (length >= 2) {
            CloseCode closeCode;
            try {
              closeCode = to!CloseCode(bigEndianToNative!ushort(readBuffer_[0..2]));
            } catch (ConvException) {
              closeCode = CloseCode.NoStatusCode;
            }

            string closeReason;
            if (length == 2) {
              closeReason = to!string(cast(CloseCode)closeCode);
            } else {
              closeReason = cast(string)readBuffer_[2..$];
            }

            writef("Connection closed: %d %s\n", closeCode, closeReason);
          }
        }
        transport_.close();
        return false;
      case Opcode.Ping:
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
    auto header = "HTTP/1.1 400 Bad Request\r\n" ~
      "Server: Thrift/" ~ VERSION ~ "\r\n" ~
      "\r\n";
    transport_.write(cast(const(ubyte[]))header);
    transport_.flush();
    transport_.close();
  }

  void writeFrameHeader(Opcode opcode = Opcode.Continuation) {
    size_t headerSize = 1;
    if (writeBuffer_.length < 126) {
      ++headerSize;
    } else if (writeBuffer_.length < 65536) {
      headerSize += 3;
    } else {
      headerSize += 9;
    }
    // The server does not mask the response

    ubyte[] header = new ubyte[headerSize];
    if (opcode == Opcode.Continuation) {
      header[0] = binary ? Opcode.Binary : Opcode.Text;
    }
    else {
      header[0] = opcode;
    }
    header[0] |= 0x80;
    if (writeBuffer_.length < 126) {
      header[1] = cast(ubyte)writeBuffer_.length;
    } else if (writeBuffer_.length < 65536) {
      header[1] = 126;
      header[2..4] = nativeToBigEndian(cast(ushort)writeBuffer_.length);
    } else {
      header[1] = 127;
      header[2..10] = nativeToBigEndian(cast(ulong)writeBuffer_.length);
    }

    transport_.write(header);
  }

  enum WEBSOCKET_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

  TTransport transport_;

  string acceptKey_;
  bool connection_;
  bool secWebSocketKey_;
  bool secWebSocketVersion_;
  bool upgrade_;
  ubyte[] readBuffer_;
  ubyte[] writeBuffer_;
}

class TServerWebSocketTransportFactory(bool binary) : TTransportFactory {
  override TTransport getTransport(TTransport trans) {
    return new TServerWebSocketTransport!binary(trans);
  }
}

alias TServerBinaryWebSocketTransportFactory = TServerWebSocketTransportFactory!true;
alias TServerTextWebSocketTransportFactory = TServerWebSocketTransportFactory!false;

private enum CloseCode : ushort {
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
}

private enum Opcode : ubyte {
  Continuation = 0x0,
  Text = 0x1,
  Binary = 0x2,
  Close = 0x8,
  Ping = 0x9,
  Pong = 0xA
}
