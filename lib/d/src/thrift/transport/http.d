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

/**
 * HTTP tranpsort implementation, modelled after the C++ one.
 *
 * Unfortunately, libcurl is quite heavyweight and supports only client-side
 * applications. This is an implementation of the basic HTTP/1.1 parts
 * supporting HTTP 100 Continue, chunked transfer encoding, keepalive, etc.
 */
module thrift.transport.http;

import core.stdc.string : memmove;
import std.algorithm : canFind, countUntil, endsWith, equal, findSplit, min;
import std.ascii : toLower;
import std.array : empty;
import std.conv : to;
import std.datetime : Clock, UTC;
import std.string : strip;
import thrift.base : VERSION;
import thrift.transport.base;
import thrift.transport.memory;
import thrift.transport.socket;

/**
 * Base class for both client- and server-side HTTP transports.
 */
abstract class THttpTransport : TBaseTransport {
  this(TTransport transport) {
    transport_ = transport;
    readHeaders_ = true;
    httpBuf_ = new ubyte[HTTP_BUFFER_SIZE];
    httpBufRemaining_ = httpBuf_[0 .. 0];
    readBuffer_ = new TMemoryBuffer;
    writeBuffer_ = new TMemoryBuffer;
  }

  override bool isOpen() {
    return transport_.isOpen();
  }

  override bool peek() {
    return transport_.peek();
  }

  override void open() {
    transport_.open();
  }

  override void close() {
    transport_.close();
  }

  override size_t read(ubyte[] buf) {
    if (!readBuffer_.peek()) {
      readBuffer_.reset();

      if (!refill()) return 0;

      if (readHeaders_) {
        readHeaders();
      }

      size_t got;
      if (chunked_) {
        got = readChunked();
      } else {
        got = readContent(contentLength_);
      }
      readHeaders_ = true;

      if (got == 0) return 0;
    }
    return readBuffer_.read(buf);
  }

  override size_t readEnd() {
    // Read any pending chunked data (footers etc.)
    if (chunked_) {
      while (!chunkedDone_) {
        readChunked();
      }
    }
    return 0;
  }

  override void write(in ubyte[] buf) {
    writeBuffer_.write(buf);
  }

  override void flush() {
    auto data = writeBuffer_.getContents();
    string header = getHeader(data.length);

    transport_.write(cast(const(ubyte)[]) header);
    transport_.write(data);
    transport_.flush();

    // Reset the buffer and header variables.
    writeBuffer_.reset();
    readHeaders_ = true;
  }

  /**
   * The size of the buffer to read HTTP requests into, in bytes. Will expand
   * as required, up to maxHttpBufferSize.
   */
  enum HTTP_BUFFER_SIZE = 1024;

  /**
   * The default value for maxHttpBufferSize, matching the frame size limit
   * used consistently across the Thrift libraries.
   */
  enum DEFAULT_MAX_HTTP_BUFFER_SIZE = 16384000;

  /**
   * The largest the read buffer will grow to, in bytes.
   *
   * The buffer only has to hold one line at a time; it grows because the peer
   * has not sent a CRLF yet, so how far it grows is the peer's choice unless
   * something bounds it.
   */
  size_t maxHttpBufferSize = DEFAULT_MAX_HTTP_BUFFER_SIZE;

  /**
   * The default value for maxBodySize, matching the frame size limit used
   * consistently across the Thrift libraries.
   */
  enum DEFAULT_MAX_BODY_SIZE = 16384000;

  /**
   * The largest message body that will be read, in bytes.
   *
   * The declared content length, and the number of chunks in a chunked body,
   * are numbers the peer chooses. The body does not pass through the line
   * buffer, so maxHttpBufferSize does not bound it.
   */
  size_t maxBodySize = DEFAULT_MAX_BODY_SIZE;

protected:
  abstract string getHeader(size_t dataLength);
  abstract bool parseStatusLine(const(ubyte)[] status);

  void parseHeader(const(ubyte)[] header) {
    auto split = findSplit(header, [':']);
    if (split[1].empty) {
      // No colon found.
      return;
    }

    static bool compToLower(ubyte a, ubyte b) {
      return toLower(cast(char)a) == toLower(cast(char)b);
    }

    // A header name is the whole token before the colon (RFC 9110 5.1), so it
    // is compared in full: startsWith() accepted every name that merely began
    // with one of these, and a transport that reads "Content-Length-Foo" as
    // "Content-Length" disagrees with every other party on the connection
    // about where the message ends.
    if (equal!compToLower(split[0], cast(ubyte[])"transfer-encoding")) {
      if (endsWith!compToLower(split[2], cast(ubyte[])"chunked")) {
        chunked_ = true;
      }
    } else if (equal!compToLower(split[0], cast(ubyte[])"content-length")) {
      chunked_ = false;
      contentLength_ = parseContentLength(split[2]);
    }
  }

  /**
   * Reads a Content-Length header value.
   *
   * Content-Length is 1*DIGIT (RFC 9110 8.6), which leaves no room for a sign,
   * for trailing text, or for a value size_t cannot hold. parse!size_t stops
   * at the first character it cannot use without saying that anything was left
   * over, and what it throws otherwise is a std.conv exception rather than a
   * transport one.
   *
   * Throws: TTransportException if the value is not such a number.
   */
  static size_t parseContentLength(const(ubyte)[] value) {
    // Optional whitespace surrounds a field value without being part of it
    // (RFC 9110 5.5).
    auto digits = strip(cast(const(char)[])value);

    if (digits.empty) {
      throw new TTransportException("Bad Content-Length: " ~ to!string(value),
        TTransportException.Type.CORRUPTED_DATA);
    }

    size_t result;
    foreach (c; digits) {
      if (c < '0' || c > '9') {
        throw new TTransportException("Bad Content-Length: " ~ to!string(digits),
          TTransportException.Type.CORRUPTED_DATA);
      }
      immutable digit = cast(size_t)(c - '0');
      if (result > (size_t.max - digit) / 10) {
        throw new TTransportException("Bad Content-Length: " ~ to!string(digits),
          TTransportException.Type.CORRUPTED_DATA);
      }
      result = result * 10 + digit;
    }
    return result;
  }

  /**
   * Reads a chunk size.
   *
   * chunk-size is 1*HEXDIG, optionally followed by chunk extensions
   * introduced with a semicolon (RFC 9112 7.1). parse!size_t with a radix
   * returns zero for a line that holds no hexadecimal digit at all instead of
   * throwing, so the handler that was meant to reject a bad chunk size never
   * saw the commonest malformed one, and "zz" was read as the chunk that ends
   * the body.
   *
   * Throws: TTransportException if the line is not such a number.
   */
  static size_t parseChunkSize(const(ubyte)[] line) {
    // Nothing in the grammar allows whitespace here, but a peer that pads the
    // line was understood before and still is.
    auto digits = strip(cast(const(char)[])line);

    size_t result;
    size_t count;
    foreach (c; digits) {
      // The extensions are not interpreted, but they do end the number.
      if (c == ';') break;

      size_t value;
      if (c >= '0' && c <= '9') {
        value = c - '0';
      } else if (c >= 'a' && c <= 'f') {
        value = c - 'a' + 10;
      } else if (c >= 'A' && c <= 'F') {
        value = c - 'A' + 10;
      } else {
        throw new TTransportException("Invalid chunk size: " ~ to!string(digits),
          TTransportException.Type.CORRUPTED_DATA);
      }

      if (result > (size_t.max - value) / 16) {
        throw new TTransportException("Invalid chunk size: " ~ to!string(digits),
          TTransportException.Type.CORRUPTED_DATA);
      }
      result = result * 16 + value;
      ++count;
    }

    if (count == 0) {
      throw new TTransportException("Invalid chunk size: " ~ to!string(digits),
        TTransportException.Type.CORRUPTED_DATA);
    }
    return result;
  }

private:
  ubyte[] readLine() {
    while (true) {
      auto split = findSplit(httpBufRemaining_, cast(ubyte[])"\r\n");

      if (split[1].empty) {
        // No CRLF yet, move whatever we have now to front and refill.
        if (httpBufRemaining_.empty) {
          httpBufRemaining_ = httpBuf_[0 .. 0];
        } else {
          // Source and destination overlap whenever anything is left over,
          // which a slice assignment does not allow.
          auto length = httpBufRemaining_.length;
          memmove(httpBuf_.ptr, httpBufRemaining_.ptr, length);
          httpBufRemaining_ = httpBuf_[0 .. length];
        }

        if (!refill()) {
          auto buf = httpBufRemaining_;
          httpBufRemaining_ = httpBufRemaining_[$ - 1 .. $ - 1];
          return buf;
        }
      } else {
        // Set the remaining buffer to the part after \r\n and return the part
        // (line) before it.
        httpBufRemaining_ = split[2];
        return split[0];
      }
    }
  }

  void readHeaders() {
    // Initialize headers state variables
    bodyBytesRead_ = 0;
    contentLength_ = 0;
    chunked_ = false;
    chunkedDone_ = false;
    chunkSize_ = 0;

    // Control state flow
    bool statusLine = true;
    bool finished;

    // Loop until headers are finished
    while (true) {
      auto line = readLine();

      if (line.length == 0) {
        if (finished) {
          readHeaders_ = false;
          return;
        } else {
          // Must have been an HTTP 100, keep going for another status line
          statusLine = true;
        }
      } else {
        if (statusLine) {
          statusLine = false;
          finished = parseStatusLine(line);
        } else {
          parseHeader(line);
        }
      }
    }
  }

  size_t readChunked() {
    size_t length;

    auto chunkSize = parseChunkSize(readLine());

    if (chunkSize == 0) {
      readChunkedFooters();
    } else {
      // Read data content
      length += readContent(chunkSize);
      // Read trailing CRLF after content
      readLine();
    }
    return length;
  }

  void readChunkedFooters() {
    while (true) {
      auto line = readLine();
      if (line.length == 0) {
        chunkedDone_ = true;
        break;
      }
    }
  }

  size_t readContent(size_t size) {
    chargeBodyBytes(size);

    auto need = size;
    while (need > 0) {
      if (httpBufRemaining_.length == 0) {
        // We have given all the data, reset position to head of the buffer.
        httpBufRemaining_ = httpBuf_[0 .. 0];
        if (!refill()) return size - need;
      }

      auto give = min(httpBufRemaining_.length, need);
      readBuffer_.write(cast(ubyte[])httpBufRemaining_[0 .. give]);
      httpBufRemaining_ = httpBufRemaining_[give .. $];
      need -= give;
    }
    return size;
  }

  /**
   * Charges size bytes of body against maxBodySize, refusing the message once
   * it would exceed it.
   *
   * Throws: TTransportException if the message is over its allowance.
   */
  void chargeBodyBytes(size_t size) {
    // bodyBytesRead_ never passes maxBodySize, so the difference cannot wrap.
    if (size > maxBodySize - bodyBytesRead_) {
      throw new TTransportException("HTTP body exceeds the maximum body size",
        TTransportException.Type.CORRUPTED_DATA);
    }
    bodyBytesRead_ += size;
  }

  bool refill() {
    // Is there a nicer way to do this?
    auto indexBegin = httpBufRemaining_.ptr - httpBuf_.ptr;
    auto indexEnd = indexBegin + httpBufRemaining_.length;

    if (httpBuf_.length - indexEnd <= (httpBuf_.length / 4)) {
      if (httpBuf_.length >= maxHttpBufferSize) {
        throw new TTransportException("HTTP line does not fit in the maximum buffer size",
          TTransportException.Type.CORRUPTED_DATA);
      }
      httpBuf_.length = (httpBuf_.length > maxHttpBufferSize / 2) ?
        maxHttpBufferSize : httpBuf_.length * 2;
    }

    // Read more data.
    auto got = transport_.read(cast(ubyte[])httpBuf_[indexEnd .. $]);
    if (got == 0) return false;
    httpBufRemaining_ = httpBuf_[indexBegin .. indexEnd + got];
    return true;
  }

  TTransport transport_;

  TMemoryBuffer writeBuffer_;
  TMemoryBuffer readBuffer_;

  bool readHeaders_;
  bool chunked_;
  size_t bodyBytesRead_;
  bool chunkedDone_;
  size_t chunkSize_;
  size_t contentLength_;

  ubyte[] httpBuf_;
  ubyte[] httpBufRemaining_;
}

/**
 * HTTP client transport.
 */
final class TClientHttpTransport : THttpTransport {
  /**
   * Constructs a client http transport operating on the passed underlying
   * transport.
   *
   * Params:
   *   transport = The underlying transport used for the actual I/O.
   *   host = The HTTP host string.
   *   path = The HTTP path string.
   */
  this(TTransport transport, string host, string path) {
    super(transport);
    host_ = host;
    path_ = path;
  }

  /**
   * Convenience overload for constructing a client HTTP transport using a
   * TSocket connecting to the specified host and port.
   *
   * Params:
   *   host = The server to connect to, also used as HTTP host string.
   *   port = The port to connect to.
   *   path = The HTTP path string.
   */
  this(string host, ushort port, string path) {
    this(new TSocket(host, port), host, path);
  }

protected:
  override string getHeader(size_t dataLength) {
    return "POST " ~ path_ ~ " HTTP/1.1\r\n" ~
      "Host: " ~ host_ ~ "\r\n" ~
      "Content-Type: application/x-thrift\r\n" ~
      "Content-Length: " ~ to!string(dataLength) ~ "\r\n" ~
      "Accept: application/x-thrift\r\n" ~
      "User-Agent: Thrift/" ~ VERSION ~ " (D/TClientHttpTransport)\r\n" ~
      "\r\n";
  }

  override bool parseStatusLine(const(ubyte)[] status) {
    // HTTP-Version SP Status-Code SP Reason-Phrase CRLF
    auto firstSplit = findSplit(status, [' ']);
    if (firstSplit[1].empty) {
      throw new TTransportException("Bad status: " ~ to!string(status),
        TTransportException.Type.CORRUPTED_DATA);
    }

    auto codeReason = firstSplit[2][countUntil!"a != b"(firstSplit[2], ' ') .. $];
    auto secondSplit = findSplit(codeReason, [' ']);
    if (secondSplit[1].empty) {
      throw new TTransportException("Bad status: " ~ to!string(status),
        TTransportException.Type.CORRUPTED_DATA);
    }

    if (secondSplit[0] == "200") {
      // HTTP 200 = OK, we got the response
      return true;
    } else if (secondSplit[0] == "100") {
      // HTTP 100 = continue, just keep reading
      return false;
    }

    throw new TTransportException("Bad status (unhandled status code): " ~
      to!string(cast(const(char[]))status), TTransportException.Type.CORRUPTED_DATA);
  }

private:
  string host_;
  string path_;
}

/**
 * HTTP server transport.
 */
final class TServerHttpTransport : THttpTransport {
  /**
   * Constructs a new instance.
   *
   * Param:
   *   transport = The underlying transport used for the actual I/O.
   */
  this(TTransport transport) {
    super(transport);
  }

protected:
  override string getHeader(size_t dataLength) {
    return "HTTP/1.1 200 OK\r\n" ~
      "Date: " ~ getRFC1123Time() ~ "\r\n" ~
      "Server: Thrift/" ~ VERSION ~ "\r\n" ~
      "Content-Type: application/x-thrift\r\n" ~
      "Content-Length: " ~ to!string(dataLength) ~ "\r\n" ~
      "Connection: Keep-Alive\r\n" ~
      "\r\n";
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

    if (split[0] == "POST") {
      // POST method ok, looking for content.
      return true;
    }

    throw new TTransportException("Bad status (unsupported method): " ~
      to!string(status), TTransportException.Type.CORRUPTED_DATA);
  }
}

/**
 * Wraps a transport into a HTTP server protocol.
 */
alias TWrapperTransportFactory!TServerHttpTransport TServerHttpTransportFactory;

private {
  import std.string : format;
  string getRFC1123Time() {
    auto sysTime = Clock.currTime(UTC());

    auto dayName = capMemberName(sysTime.dayOfWeek);
    auto monthName = capMemberName(sysTime.month);

    return format("%s, %s %s %s %s:%s:%s GMT", dayName, sysTime.day,
      monthName, sysTime.year, sysTime.hour, sysTime.minute, sysTime.second);
  }

  import std.ascii : toUpper;
  import std.traits : EnumMembers;
  string capMemberName(T)(T val) if (is(T == enum)) {
    foreach (i, e; EnumMembers!T) {
      enum name = __traits(derivedMembers, T)[i];
      enum capName = cast(char) toUpper(name[0]) ~ name [1 .. $];
      if (val == e) {
        return capName;
      }
    }
    throw new Exception("Not a member of " ~ T.stringof ~ ": " ~ to!string(val));
  }

  unittest {
    enum Foo {
      bar,
      bAZ
    }

    import std.exception;
    enforce(capMemberName(Foo.bar) == "Bar");
    enforce(capMemberName(Foo.bAZ) == "BAZ");
  }
}

unittest {
  import std.array : replicate;
  import thrift.transport.memory;

  // A header line longer than the buffer the transport starts with, so that
  // readLine() has to shift what it has and grow.
  {
    auto wire = cast(ubyte[])("POST / HTTP/1.1\r\nX-Filler: " ~ replicate("A", 20_000)
      ~ "\r\nContent-Length: 5\r\n\r\nhello");
    auto http = new TServerHttpTransport(new TMemoryBuffer(wire));
    ubyte[5] buf;
    assert(http.read(buf) == 5);
    assert(cast(string)buf[] == "hello");
  }

  // A header line that never ends. The buffer used to double for as long as
  // the peer kept sending.
  {
    auto wire = cast(ubyte[])("POST / HTTP/1.1\r\nX-Filler: " ~ replicate("A", 200_000));
    auto http = new TServerHttpTransport(new TMemoryBuffer(wire));
    http.maxHttpBufferSize = 16 * 1024;

    ubyte[16] buf;
    bool threw;
    try {
      http.read(buf);
    } catch (TTransportException) {
      threw = true;
    }
    assert(threw, "a header line without a CRLF was read without limit");
    assert(http.httpBuf_.length <= http.maxHttpBufferSize,
      "the read buffer grew past the maximum");
  }
}

unittest {
  import std.exception : assertThrown;
  import thrift.transport.memory;

  // Reads one message off a server transport fed the given wire bytes, and
  // returns what the transport handed out as the body.
  static string readBody(string wire) {
    auto http = new TServerHttpTransport(new TMemoryBuffer(cast(ubyte[])wire.dup));
    ubyte[64] buf;
    return cast(string)buf[0 .. http.read(buf)].idup;
  }

  enum request = "POST / HTTP/1.1\r\n";

  // A header name is the whole token before the colon (RFC 9110 5.1). The
  // names below all begin with one the transport knows and are not it, so the
  // body they frame is the peer's choice and not the one any other party on
  // the connection sees.
  assert(readBody(request ~ "Content-Length: 5\r\n\r\nhello") == "hello");
  assert(readBody(request ~ "content-length: 5\r\n\r\nhello") == "hello");
  assert(readBody(request ~ "Content-Length-Foo: 5\r\n\r\nhello") == "");
  assert(readBody(request ~ "Content-LengthX: 5\r\n\r\nhello") == "");
  assert(readBody(request ~ "Content-Lengths: 5\r\n\r\nhello") == "");

  // The same name arriving twice, the second one abbreviated: a parser that
  // follows the grammar frames five bytes here, and so must this one.
  assert(readBody(request ~ "Content-Length: 5\r\nContent-Length-Foo: 2\r\n\r\nhello")
    == "hello");

  // Transfer-Encoding, both directions.
  assert(readBody(request ~ "Transfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")
    == "hello");
  assert(readBody(request ~ "Transfer-Encoding-Foo: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n")
    == "");

  // Content-Length is 1*DIGIT (RFC 9110 8.6): no sign, no trailing text, and
  // nothing that does not fit. parse!size_t stops at the first character it
  // cannot use and says nothing about the rest, and what it throws on a sign
  // or an overflow is a std.conv exception rather than a transport one.
  assertThrown!TTransportException(readBody(request ~ "Content-Length: -1\r\n\r\nhello"));
  assertThrown!TTransportException(readBody(request ~ "Content-Length: +5\r\n\r\nhello"));
  assertThrown!TTransportException(readBody(request ~ "Content-Length: 5abc\r\n\r\nhello"));
  assertThrown!TTransportException(readBody(request ~ "Content-Length: 0x10\r\n\r\nhello"));
  assertThrown!TTransportException(readBody(request ~ "Content-Length: \r\n\r\nhello"));
  assertThrown!TTransportException(
    readBody(request ~ "Content-Length: 99999999999999999999999999\r\n\r\nhello"));

  // Optional whitespace around the value is not part of it (RFC 9110 5.5)...
  assert(readBody(request ~ "Content-Length:5\r\n\r\nhello") == "hello");
  assert(readBody(request ~ "Content-Length:   5   \r\n\r\nhello") == "hello");

  // ... but none is allowed between the name and the colon, so the name here
  // is "Content-Length " and not one the transport knows.
  assert(readBody(request ~ "Content-Length : 5\r\n\r\nhello") == "");

  // A chunk size is 1*HEXDIG with optional extensions after a semicolon
  // (RFC 9112 7.1). parse!size_t given a radix returns zero for a line with no
  // hexadecimal digit in it rather than throwing, so a line the grammar does
  // not allow was read as the chunk that ends the body and the message quietly
  // came out empty.
  static string readChunkedBody(string chunks) {
    return readBody(request ~ "Transfer-Encoding: chunked\r\n\r\n" ~ chunks);
  }

  assert(readChunkedBody("5\r\nhello\r\n0\r\n\r\n") == "hello");
  assert(readChunkedBody("5;ext=1\r\nhello\r\n0\r\n\r\n") == "hello");
  assert(readChunkedBody("5 \r\nhello\r\n0\r\n\r\n") == "hello");
  assert(readChunkedBody("A\r\nhelloworld\r\n0\r\n\r\n") == "helloworld");
  assert(readChunkedBody("a\r\nhelloworld\r\n0\r\n\r\n") == "helloworld");

  assertThrown!TTransportException(readChunkedBody("zz\r\nhello\r\n0\r\n\r\n"));
  assertThrown!TTransportException(readChunkedBody("hello\r\nhello\r\n0\r\n\r\n"));
  assertThrown!TTransportException(readChunkedBody("-5\r\nhello\r\n0\r\n\r\n"));
  assertThrown!TTransportException(readChunkedBody("5xyz\r\nhello\r\n0\r\n\r\n"));
  assertThrown!TTransportException(readChunkedBody(";ext=1\r\nhello\r\n0\r\n\r\n"));
  assertThrown!TTransportException(
    readChunkedBody("1ffffffffffffffff\r\nhello\r\n0\r\n\r\n"));
}

version (unittest) {
  /**
   * Serves a fixed header block and then a body, counting the body bytes it
   * was asked for. Asking whether a read threw is not enough to tell a bounded
   * transport from an unbounded one -- an over-declared body runs the peer out
   * and throws either way -- so the tests below count what the peer was asked
   * to send.
   */
  private final class TCountingTransport : TBaseTransport {
    this(string headers, string content) {
      headers_ = cast(ubyte[])headers.dup;
      body_ = cast(ubyte[])content.dup;
    }

    override bool isOpen() @property { return true; }
    override bool peek() { return true; }
    override void open() {}
    override void close() {}

    override size_t read(ubyte[] buf) {
      import std.algorithm : min;
      if (headerPos_ < headers_.length) {
        auto n = min(buf.length, headers_.length - headerPos_);
        buf[0 .. n] = headers_[headerPos_ .. headerPos_ + n];
        headerPos_ += n;
        return n;
      }
      if (bodyPos_ >= body_.length) return 0;
      auto n = min(buf.length, body_.length - bodyPos_);
      buf[0 .. n] = body_[bodyPos_ .. bodyPos_ + n];
      bodyPos_ += n;
      return n;
    }

    /// How many body bytes the transport asked this peer for.
    size_t bodyServed() const { return bodyPos_; }

  private:
    ubyte[] headers_;
    ubyte[] body_;
    size_t headerPos_;
    size_t bodyPos_;
  }
}

unittest {
  import std.array : replicate;
  import thrift.transport.memory;

  enum maxBody = 64 * 1024;
  enum overLong = 4 * maxBody;

  // A declared length larger than the maximum is refused before anything is
  // read on account of it. The declared number itself costs nothing -- nothing
  // is allocated up front -- so what it buys is the licence to keep reading,
  // and that is what has to be refused.
  {
    auto peer = new TCountingTransport(
      "POST / HTTP/1.1\r\nContent-Length: " ~ to!string(overLong) ~ "\r\n\r\n",
      replicate("x", overLong));
    auto http = new TServerHttpTransport(peer);
    http.maxBodySize = maxBody;

    ubyte[64] buf;
    bool threw;
    try { http.read(buf); } catch (TTransportException) { threw = true; }
    assert(threw, "an over-long declared body was read");
    assert(peer.bodyServed() == 0,
      "bytes were read on account of a length that was already too large");
  }

  // The largest size_t there is, which is what a peer sends when it wants the
  // reading to simply not stop.
  {
    auto peer = new TCountingTransport(
      "POST / HTTP/1.1\r\nContent-Length: 18446744073709551615\r\n\r\n",
      replicate("x", overLong));
    auto http = new TServerHttpTransport(peer);
    http.maxBodySize = maxBody;

    ubyte[64] buf;
    bool threw;
    try { http.read(buf); } catch (TTransportException) { threw = true; }
    assert(threw, "a body declared as size_t.max was read");
    assert(peer.bodyServed() == 0, "bytes were read on account of size_t.max");
  }

  // A chunked body declares no total at all: it runs until the peer stops.
  {
    enum chunkSize = 4096;
    string chunks;
    foreach (i; 0 .. overLong / chunkSize) {
      chunks ~= "1000\r\n" ~ replicate("y", chunkSize) ~ "\r\n";
    }
    chunks ~= "0\r\n\r\n";

    auto peer = new TCountingTransport(
      "POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n", chunks);
    auto http = new TServerHttpTransport(peer);
    http.maxBodySize = maxBody;

    ubyte[64] buf;
    bool threw;
    try {
      http.read(buf);
      http.readEnd();
    } catch (TTransportException) { threw = true; }
    assert(threw, "a chunked body was read without limit");
    // Every chunk is charged, so the peer runs out of allowance a little past
    // the maximum -- the chunk framing is served too -- and nowhere near the
    // four times as much it was holding.
    assert(peer.bodyServed() <= maxBody + 1024,
      "more than the maximum was read from a chunked body");
  }

  // A body inside the maximum still arrives, ...
  {
    auto http = new TServerHttpTransport(new TMemoryBuffer(
      cast(ubyte[])("POST / HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello")));
    http.maxBodySize = maxBody;
    ubyte[16] buf;
    assert(http.read(buf) == 5);
    assert(cast(string)buf[0 .. 5] == "hello");
  }

  // ... and so does the next one on the same connection: the allowance is per
  // message, not per connection.
  {
    enum big = maxBody - 1024;
    auto one = "POST / HTTP/1.1\r\nContent-Length: " ~ to!string(big) ~ "\r\n\r\n"
      ~ replicate("z", big);
    auto http = new TServerHttpTransport(new TMemoryBuffer(cast(ubyte[])(one ~ one)));
    http.maxBodySize = maxBody;

    auto buf = new ubyte[big];
    foreach (message; 0 .. 2) {
      size_t have;
      while (have < big) {
        auto got = http.read(buf[have .. $]);
        assert(got > 0, "a second message on the connection was refused");
        have += got;
      }
      assert(have == big);
    }
  }
}
