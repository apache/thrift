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

// The WebSocket server reassembles a message that arrives as many fragments and
// a frame that spans many reads. The work of reassembling has to stay
// proportional to the message size, not to the square of the number of pieces,
// and the total held to the transport's maximum is unchanged.

const test = require("tape");
const net = require("net");
const crypto = require("crypto");
const thrift = require("thrift/lib/nodejs/lib/thrift");
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");

// TBufferedTransport with a small maximum, so a test can reach the limit
// without moving a hundred megabytes to do it.
function smallTransport(maxLength) {
  function Small(buffer, callback) {
    return new TBufferedTransport(buffer, callback);
  }
  Small.receiver = TBufferedTransport.receiver;
  Small.DEFAULT_MAX_LENGTH = maxLength;
  return Small;
}

function handshakeRequest() {
  return (
    "GET / HTTP/1.1\r\n" +
    "Host: 127.0.0.1\r\n" +
    "Upgrade: websocket\r\n" +
    "Connection: Upgrade\r\n" +
    "Sec-WebSocket-Key: " +
    crypto.randomBytes(16).toString("base64") +
    "\r\n" +
    "Sec-WebSocket-Version: 13\r\n\r\n"
  );
}

function upgrade(port, callback) {
  const socket = net.connect(port, "127.0.0.1", function () {
    socket.setNoDelay(true);
    socket.write(handshakeRequest());
  });
  socket.once("data", function (data) {
    if (data.toString().indexOf("101 Switching Protocols") < 0) {
      throw new Error("WebSocket handshake failed");
    }
    callback(socket);
  });
  return socket;
}

// A well-formed masked frame carrying payload; fin === false marks it as a
// fragment of a larger message.
function maskedFrame(payload, fin) {
  const mask = Buffer.from([0x01, 0x02, 0x03, 0x04]);
  const masked = Buffer.from(payload);
  for (let i = 0; i < masked.length; i++) {
    masked[i] ^= mask[i % 4];
  }
  let header;
  if (masked.length < 0x7e) {
    header = Buffer.alloc(2 + 4);
    header[1] = 0x80 | masked.length;
    mask.copy(header, 2);
  } else if (masked.length <= 0xffff) {
    header = Buffer.alloc(4 + 4);
    header[1] = 0x80 | 0x7e;
    header.writeUInt16BE(masked.length, 2);
    mask.copy(header, 4);
  } else {
    header = Buffer.alloc(10 + 4);
    header[1] = 0x80 | 0x7f;
    header.writeUInt32BE(0, 2);
    header.writeUInt32BE(masked.length, 6);
    mask.copy(header, 10);
  }
  header[0] = (fin === false ? 0x00 : 0x80) | 0x02;
  return Buffer.concat([header, masked]);
}

function startServer(onProcess, transport, callback) {
  const server = thrift.createWebServer({
    services: {
      "/": {
        processor: {
          Processor: function () {
            this.process = function (input, output) {
              onProcess(input, output);
            };
          },
        },
        handler: {},
        transport: transport,
      },
    },
  });
  server.listen(0, "127.0.0.1", function () {
    callback(server, server.address().port);
  });
}

// Counts the bytes requested from Buffer.alloc and Buffer.concat, and the
// largest single request, until the returned function restores them and reports
// the totals. Used to measure the reassembly work without timing the CPU.
function startAllocCounter() {
  const realAlloc = Buffer.alloc;
  const realConcat = Buffer.concat;
  let total = 0;
  let largest = 0;
  function note(n) {
    total += n;
    if (n > largest) {
      largest = n;
    }
  }
  Buffer.alloc = function (size) {
    if (typeof size === "number") {
      note(size);
    }
    return realAlloc.apply(Buffer, arguments);
  };
  Buffer.concat = function (list, length) {
    if (typeof length === "number") {
      note(length);
    } else if (Array.isArray(list)) {
      let n = 0;
      for (let i = 0; i < list.length; i++) {
        n += list[i].length;
      }
      note(n);
    }
    return realConcat.apply(Buffer, arguments);
  };
  return function () {
    Buffer.alloc = realAlloc;
    Buffer.concat = realConcat;
    return { total: total, largest: largest };
  };
}

test("reassembling a fragmented message does not grow with the fragment count", function (assert) {
  const fragSize = 1024;
  const fragments = 256;
  const messageSize = fragSize * fragments; // 256 KiB
  // Build the frames up front, so their construction is not counted.
  const frames = [];
  for (let i = 0; i < fragments; i++) {
    frames.push(maskedFrame(Buffer.alloc(fragSize, 0x61), i === fragments - 1));
  }

  let stop = null;
  let finished = false;
  let server = null;
  let socket = null;

  startServer(
    function () {
      if (finished) {
        return;
      }
      finished = true;
      const seen = stop();
      assert.comment(
        "allocated " +
          seen.total +
          " bytes to reassemble a " +
          messageSize +
          "-byte message in " +
          fragments +
          " fragments",
      );
      // Re-joining the whole buffer on each of N fragments would allocate about
      // N*N/2 * fragSize ~ 33 MiB here (~130x the message). Linear reassembly
      // stays a small multiple of the message size.
      assert.ok(
        seen.total < 16 * messageSize,
        "reassembly allocated " +
          seen.total +
          " bytes, a small multiple of the " +
          messageSize +
          "-byte message",
      );
      server.close();
      socket.destroy();
      assert.end();
    },
    smallTransport(messageSize * 4),
    function (srv, port) {
      server = srv;
      socket = upgrade(port, function (sock) {
        stop = startAllocCounter();
        for (let i = 0; i < frames.length; i++) {
          sock.write(frames[i]);
        }
      });
      socket.on("error", function () {});
      setTimeout(function () {
        if (finished) {
          return;
        }
        finished = true;
        if (stop) {
          stop();
        }
        assert.fail("the message never reached the processor");
        server.close();
        socket.destroy();
        assert.end();
      }, 10000).unref();
    },
  );
});

test("reassembling one frame split across many reads stays linear", function (assert) {
  const payloadSize = 128 * 1024; // 128 KiB in one frame
  const frame = maskedFrame(Buffer.alloc(payloadSize, 0x62), true);
  const writeSize = 1024;

  let stop = null;
  let dataEvents = 0;
  let finished = false;
  let server = null;
  let socket = null;

  startServer(
    function () {
      if (finished) {
        return;
      }
      finished = true;
      const seen = stop();
      assert.comment(
        "reassembled a " +
          payloadSize +
          "-byte frame from " +
          dataEvents +
          " reads using " +
          seen.total +
          " bytes",
      );
      assert.ok(dataEvents > 1, "the frame really was split across reads");
      // Re-joining the carried-over bytes on each read would be quadratic in the
      // number of reads; appending in place keeps it a small multiple of the
      // frame size.
      assert.ok(
        seen.total < 16 * payloadSize,
        "reassembly allocated " +
          seen.total +
          " bytes, a small multiple of the " +
          payloadSize +
          "-byte frame",
      );
      server.close();
      socket.destroy();
      assert.end();
    },
    smallTransport(payloadSize * 8),
    function (srv, port) {
      server = srv;
      srv.on("upgrade", function (request, serverSide) {
        serverSide.on("data", function () {
          dataEvents += 1;
        });
      });
      socket = upgrade(port, function (sock) {
        stop = startAllocCounter();
        let off = 0;
        (function pump() {
          if (off >= frame.length) {
            return;
          }
          const end = Math.min(off + writeSize, frame.length);
          sock.write(frame.subarray(off, end));
          off = end;
          setImmediate(pump);
        })();
      });
      socket.on("error", function () {});
      setTimeout(function () {
        if (finished) {
          return;
        }
        finished = true;
        if (stop) {
          stop();
        }
        assert.fail("the frame never reached the processor whole");
        server.close();
        socket.destroy();
        assert.end();
      }, 10000).unref();
    },
  );
});

test("a fragmented message is still held to the transport maximum", function (assert) {
  // A regression guard, true on the fixed and the unfixed tree alike: the server
  // closes the connection at the limit and never asks for a single buffer larger
  // than it.
  const maxLength = 64 * 1024;
  let stop = null;
  let finished = false;
  let closed = false;
  let seen = null;
  let server = null;
  let socket = null;

  function conclude(assertFn) {
    if (finished) {
      return;
    }
    finished = true;
    if (stop) {
      seen = stop();
    }
    assertFn();
    server.close();
    socket.destroy();
    assert.end();
  }

  startServer(
    function () {
      conclude(function () {
        assert.fail("an over-limit message reached the processor");
      });
    },
    smallTransport(maxLength),
    function (srv, port) {
      server = srv;
      // 20 x 4 KiB = 80 KiB of fragments, none final: past the 64 KiB maximum.
      const fragment = maskedFrame(Buffer.alloc(4096, 0x61), false);
      socket = upgrade(port, function (sock) {
        stop = startAllocCounter();
        for (let i = 0; i < 20; i++) {
          sock.write(fragment);
        }
      });
      socket.on("error", function () {});
      socket.on("close", function () {
        closed = true;
        conclude(function () {
          assert.pass("the connection was closed at the limit");
          assert.ok(
            seen && seen.largest <= maxLength,
            "no single allocation exceeded the " +
              maxLength +
              "-byte maximum (largest was " +
              (seen && seen.largest) +
              ")",
          );
        });
      });
      setTimeout(function () {
        conclude(function () {
          assert.fail(
            "the connection stayed open past the limit (closed=" + closed + ")",
          );
        });
      }, 5000).unref();
    },
  );
});
