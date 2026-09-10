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
   * The default value for maxPayloadLength, matching the frame size limit
   * used consistently across the Thrift libraries.
   */
  enum DEFAULT_MAX_PAYLOAD_LENGTH = 16384000;

  /**
   * The largest payload accepted in a single WebSocket frame, in bytes.
   *
   * The payload length is read off the frame header and sizes the read
   * buffer, so it decides how much memory a peer can ask for before it has
   * sent anything.
   */
  size_t maxPayloadLength = DEFAULT_MAX_PAYLOAD_LENGTH;

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

    writeFrameHeader(Opcode.Continuation, writeBuffer_.length);
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

    // A header name is the whole token before the colon (RFC 9110 5.1), so it
    // is compared in full: startsWith() let every name that merely began with
    // one of these count as that name, and all four of them together are the
    // handshake.
    if (equal!compToLower(split[0], cast(ubyte[])"upgrade")) {
      auto upgrade = stripLeft(cast(const(char)[])split[2]);
      upgrade_ = sicmp(upgrade, "websocket") == 0;
    } else if (equal!compToLower(split[0], cast(ubyte[])"connection")) {
      auto connection = stripLeft(cast(const(char)[])split[2]);
      connection_ = canFind(connection.toLower, "upgrade");
    } else if (equal!compToLower(split[0], cast(ubyte[])"sec-websocket-key")) {
      auto secWebSocketKey = stripLeft(cast(const(char)[])split[2]);
      auto hash = sha1Of(secWebSocketKey ~ WEBSOCKET_GUID);
      acceptKey_ = Base64.encode(hash);
      secWebSocketKey_ = true;
    } else if (equal!compToLower(split[0], cast(ubyte[])"sec-websocket-version")) {
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
    writeFrameHeader(Opcode.Close, ushort.sizeof);
    transport_.write(nativeToBigEndian!ushort(reason));
    transport_.flush();
    transport_.close();
  }

  void pong() {
    // RFC 6455 5.5.3: the Pong carries the Ping's payload, which is what the
    // read buffer holds at this point, and the header has to say so -- a peer
    // told the frame is empty reads the payload as the next frame header.
    writeFrameHeader(Opcode.Pong, readBuffer_.length);
    transport_.write(readBuffer_);
    transport_.flush();
  }

  bool readFrame() {
    // Loop rather than recurse: a Ping carries nothing for the caller, so the
    // reader has to go on to the next frame to satisfy it, and a peer may send
    // as many Pings as it likes. Answering each by calling readFrame() again
    // would take a stack frame per Ping.
    while (true) {
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

      // The length below sizes the read buffer before a single payload byte has
      // arrived, so decide on it first.
      if (payloadLength > maxPayloadLength) {
        failConnection(CloseCode.MessageTooBig);
        return false;
      }

      auto length = cast(size_t)payloadLength;

      // The masking key is part of the header of every frame whose MASK bit is
      // set, whatever the payload length, and a client frame without MASK was
      // refused above. Read only for a frame that carries a payload, it would be
      // left in the stream and the next header parsed out of it.
      read = transport_.read(headerBuffer[0..4]);
      if (read < 4) {
        return false;
      }

      if (length > 0) {
        readBuffer_ = new ubyte[](length);
        // Wait for the whole payload. A single read returns whatever one recv()
        // produced, so a payload the network splits across segments would be
        // taken for the end of the stream. Waiting is only safe because the
        // length has been held to maxPayloadLength above.
        try {
          transport_.readAll(readBuffer_);
        } catch (TTransportException e) {
          if (e.type != TTransportException.Type.END_OF_FILE) {
            throw e;
          }
          // The peer went away part-way through the frame, which is what the
          // caller has always been told about a payload that does not turn up.
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
      } else {
        // Nothing for the caller, and nothing for pong() to echo back either.
        readBuffer_ = null;
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
          continue;
        default:
          return true;
      }
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

  void writeFrameHeader(Opcode opcode, size_t length) {
    size_t headerSize = 1;
    if (length < 126) {
      ++headerSize;
    } else if (length < 65536) {
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
    if (length < 126) {
      header[1] = cast(ubyte)length;
    } else if (length < 65536) {
      header[1] = 126;
      header[2..4] = nativeToBigEndian(cast(ushort)length);
    } else {
      header[1] = 127;
      header[2..10] = nativeToBigEndian(cast(ulong)length);
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

unittest {
  import core.memory : GC;
  import thrift.transport.memory;

  // A frame that declares a 2 GiB payload and carries none of it: 14 bytes on
  // the wire. The declared length must not size the read buffer.
  auto frame = cast(ubyte[])[
    0x82, 0xff,              // FIN, binary, masked, 64-bit length follows
    0x00, 0x00, 0x00, 0x00,  // high half of the length
    0x80, 0x00, 0x00, 0x00,  // low half: 2 GiB
    0xde, 0xad, 0xbe, 0xef   // masking key
  ];

  auto ws = new TServerWebSocketTransport!true(new TMemoryBuffer(frame));

  auto before = GC.stats().allocatedInCurrentThread;
  assert(!ws.readFrame(), "a frame promising more than it carries was accepted");
  auto allocated = GC.stats().allocatedInCurrentThread - before;
  assert(allocated < 1024 * 1024,
    "reading a 14 byte frame allocated " ~ to!string(allocated) ~ " bytes");
}

version (unittest) {
  /**
   * A transport whose read and write sides are separate, so that the response
   * the handshake writes is not handed straight back to it as a frame.
   */
  private final class TPipeTransport : TBaseTransport {
    this(string incoming) { in_ = cast(ubyte[])incoming.dup; }

    override bool isOpen() @property { return true; }
    override bool peek() { return pos_ < in_.length; }
    override void open() {}
    override void close() {}

    override size_t read(ubyte[] buf) {
      if (pos_ >= in_.length) return 0;
      auto n = min(buf.length, in_.length - pos_);
      buf[0 .. n] = in_[pos_ .. pos_ + n];
      pos_ += n;
      return n;
    }

    override void write(in ubyte[] buf) { out_ ~= buf; }
    override void flush() {}

    /// Everything the transport has written back.
    string written() const { return cast(string)out_.idup; }

  private:
    ubyte[] in_;
    ubyte[] out_;
    size_t pos_;
  }
}

unittest {
  // All four of these names together are the handshake, so a name that merely
  // begins with one of them must not count as it.
  static bool accepted(string headers) {
    auto peer = new TPipeTransport("GET /ws HTTP/1.1\r\n" ~ headers ~ "\r\n");
    auto ws = new TServerWebSocketTransport!true(peer);
    ubyte[64] buf;
    // A completed handshake goes straight on to read a frame, and there is
    // none here; what the handshake decided is in the response line.
    try { ws.read(buf); } catch (TTransportException) {}
    return canFind(peer.written(), "HTTP/1.1 101");
  }

  enum key = "dGhlIHNhbXBsZSBub25jZQ==";

  assert(accepted("Upgrade: websocket\r\nConnection: Upgrade\r\n" ~
    "Sec-WebSocket-Key: " ~ key ~ "\r\nSec-WebSocket-Version: 13\r\n"));

  assert(!accepted("Upgrade-Foo: websocket\r\nConnection-Foo: Upgrade\r\n" ~
    "Sec-WebSocket-Key-Foo: " ~ key ~ "\r\nSec-WebSocket-Version-Foo: 13\r\n"));

  assert(!accepted("Upgrade-Foo: websocket\r\nConnection: Upgrade\r\n" ~
    "Sec-WebSocket-Key: " ~ key ~ "\r\nSec-WebSocket-Version: 13\r\n"));

  assert(!accepted("Upgrade: websocket\r\nConnection: Upgrade\r\n" ~
    "Sec-WebSocket-KeyX: " ~ key ~ "\r\nSec-WebSocket-Version: 13\r\n"));

  assert(!accepted("Upgrade: websocket\r\nConnection: Upgrade\r\n" ~
    "Sec-WebSocket-Key: " ~ key ~ "\r\nSec-WebSocket-Versions: 13\r\n"));

  // A name that is not a prefix of one of them was refused before and still is.
  assert(!accepted("Upgra: websocket\r\nConn: Upgrade\r\n" ~
    "Sec-WebSocket: " ~ key ~ "\r\nSec-WebSocket-Ver: 13\r\n"));
}

version (unittest) {
  /**
   * A transport that serves a scripted byte stream at most `chunk` bytes per
   * read -- the way a socket returns whatever one recv() produced -- records
   * what is written back, and notes how far down the stack it was read from.
   */
  private final class TScriptedTransport : TBaseTransport {
    this(const(ubyte)[] incoming, size_t chunk = size_t.max) {
      in_ = incoming.dup;
      chunk_ = chunk;
    }

    override bool isOpen() @property { return true; }
    override bool peek() { return pos_ < in_.length; }
    override void open() {}
    override void close() {}

    override size_t read(ubyte[] buf) {
      ubyte marker;
      auto here = cast(size_t)&marker;
      if (lowest == 0 || here < lowest) lowest = here;

      if (pos_ >= in_.length) return 0;
      auto n = min(buf.length, in_.length - pos_, chunk_);
      buf[0 .. n] = in_[pos_ .. pos_ + n];
      pos_ += n;
      return n;
    }

    override void write(in ubyte[] buf) { written ~= buf; }
    override void flush() {}

    /// Everything written back, and the lowest stack address read() ran at.
    ubyte[] written;
    size_t lowest;

  private:
    ubyte[] in_;
    size_t pos_;
    size_t chunk_;
  }

  /// A client frame: FIN set and masked with a fixed key, as a client's must be.
  private ubyte[] clientFrame(Opcode opcode, const(ubyte)[] payload) {
    immutable ubyte[4] key = [0x37, 0xfa, 0x21, 0x3d];
    ubyte[] frame = [cast(ubyte)(0x80 | opcode)];
    if (payload.length < 126) {
      frame ~= cast(ubyte)(0x80 | payload.length);
    } else if (payload.length < 65536) {
      frame ~= cast(ubyte)(0x80 | 126);
      auto length = nativeToBigEndian(cast(ushort)payload.length);
      frame ~= length[];
    } else {
      frame ~= cast(ubyte)(0x80 | 127);
      auto length = nativeToBigEndian(cast(ulong)payload.length);
      frame ~= length[];
    }
    frame ~= key[];
    foreach (i, b; payload) {
      frame ~= cast(ubyte)(b ^ key[i % 4]);
    }
    return frame;
  }
}

unittest {
  // A payload the network delivers in pieces is still read whole (THRIFT-6178).
  auto payload = new ubyte[](4000);
  foreach (i, ref b; payload) b = cast(ubyte)(i * 31);

  auto ws = new TServerWebSocketTransport!true(
    new TScriptedTransport(clientFrame(Opcode.Binary, payload), 1000));
  assert(ws.readFrame(), "a payload that arrived in pieces was taken for the end of the stream");
  assert(ws.readBuffer_ == payload);

  // A payload that really does stop short is still the end of the stream.
  auto cut = clientFrame(Opcode.Binary, payload)[0 .. 8 + 500];
  ws = new TServerWebSocketTransport!true(new TScriptedTransport(cut, 1000));
  assert(!ws.readFrame(), "a payload cut short was handed over");
}

unittest {
  // Pings are answered from a constant stack depth (THRIFT-6179). A one-byte
  // Ping is seven bytes on the wire, and answering each by calling readFrame()
  // again cost a stack frame per Ping.
  ubyte[] stream;
  foreach (i; 0 .. 2000) {
    stream ~= clientFrame(Opcode.Ping, [cast(ubyte)0x42]);
  }
  stream ~= clientFrame(Opcode.Binary, cast(const(ubyte)[])"data");

  auto peer = new TScriptedTransport(stream);
  auto ws = new TServerWebSocketTransport!true(peer);
  ubyte top;
  assert(ws.readFrame());
  assert(ws.readBuffer_ == cast(const(ubyte)[])"data");
  auto depth = cast(size_t)&top - peer.lowest;
  assert(depth < 64 * 1024, "2000 Pings took the reader " ~ to!string(depth) ~ " bytes down the stack");
}

unittest {
  // The masking key of a masked frame with no payload is part of its header
  // (THRIFT-6180). Left unread, the next header was parsed out of it.
  auto peer = new TScriptedTransport(
    clientFrame(Opcode.Ping, []) ~ clientFrame(Opcode.Binary, cast(const(ubyte)[])"hello"));
  auto ws = new TServerWebSocketTransport!true(peer);
  bool read;
  try {
    read = ws.readFrame();
  } catch (TTransportException e) {
    assert(false, "the frame after an empty Ping was misread: " ~ e.msg);
  }
  assert(read);
  assert(ws.readBuffer_ == cast(const(ubyte)[])"hello");
  assert(peer.written == cast(const(ubyte)[])[0x8A, 0x00], "the Pong for an empty Ping was not empty");
}

unittest {
  // A Pong for an empty Ping echoes nothing, even right after a Ping whose
  // payload is still in the read buffer (THRIFT-6180).
  auto peer = new TScriptedTransport(clientFrame(Opcode.Ping, cast(const(ubyte)[])"PING") ~
    clientFrame(Opcode.Ping, []) ~ clientFrame(Opcode.Binary, cast(const(ubyte)[])"d"));
  auto ws = new TServerWebSocketTransport!true(peer);
  bool read;
  try {
    read = ws.readFrame();
  } catch (TTransportException e) {
    assert(false, "the frame after an empty Ping was misread: " ~ e.msg);
  }
  assert(read);
  assert(ws.readBuffer_ == cast(const(ubyte)[])"d");
  assert(peer.written == cast(const(ubyte)[])[0x8A, 0x04] ~ cast(const(ubyte)[])"PING" ~
    cast(const(ubyte)[])[0x8A, 0x00], "the Pongs were written as " ~ to!string(peer.written));
}

unittest {
  // A Pong carries the Ping's payload, and its header says how long that is (THRIFT-6180).
  auto ping = cast(const(ubyte)[])"PINGDATA";
  auto peer = new TScriptedTransport(clientFrame(Opcode.Ping, ping) ~ clientFrame(Opcode.Binary, [cast(ubyte)1]));
  auto ws = new TServerWebSocketTransport!true(peer);
  assert(ws.readFrame());
  assert(peer.written == cast(const(ubyte)[])[0x8A, 0x08] ~ ping,
    "the Pong was written as " ~ to!string(peer.written));
}

unittest {
  // A Close frame's header says it carries the two-byte status code (THRIFT-6180).
  auto peer = new TScriptedTransport(clientFrame(Opcode.Binary, cast(const(ubyte)[])"12345"));
  auto ws = new TServerWebSocketTransport!true(peer);
  ws.maxPayloadLength = 4;
  assert(!ws.readFrame());
  assert(peer.written == cast(const(ubyte)[])[0x88, 0x02, 0x03, 0xF1],  // 1009, message too big
    "the Close frame was written as " ~ to!string(peer.written));
}

unittest {
  // flush() is the one caller whose body really is the write buffer; its
  // header described that correctly before, and still does.
  auto peer = new TScriptedTransport([]);
  auto ws = new TServerWebSocketTransport!true(peer);
  ws.write(cast(const(ubyte)[])"hello");
  ws.flush();
  assert(peer.written == cast(const(ubyte)[])[0x82, 0x05] ~ cast(const(ubyte)[])"hello");
}
