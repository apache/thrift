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

// The WebSocket frame decoder in web_server.js sizes its payload buffer from
// the length the frame declares. These tests cover what happens when that
// length does not describe the bytes that actually arrived.

const test = require("tape");
const net = require("net");
const crypto = require("crypto");
const thrift = require("thrift/lib/nodejs/lib/thrift");
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");

// TBufferedTransport with a small maximum, so that a test can reach the limit
// without moving a hundred megabytes to do it.
function smallTransport(maxLength) {
  function Small(buffer, callback) {
    return new TBufferedTransport(buffer, callback);
  }
  Small.receiver = TBufferedTransport.receiver;
  Small.DEFAULT_MAX_LENGTH = maxLength;
  return Small;
}

function startServer(callback, transport) {
  const server = thrift.createWebServer({
    services: {
      "/": {
        processor: {
          Processor: function () {
            this.process = function () {};
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

// Opens a connection and completes the WebSocket handshake, then hands the
// socket to the caller.
function upgrade(port, callback) {
  const socket = net.connect(port, "127.0.0.1", function () {
    socket.write(
      "GET / HTTP/1.1\r\n" +
        "Host: 127.0.0.1\r\n" +
        "Upgrade: websocket\r\n" +
        "Connection: Upgrade\r\n" +
        "Sec-WebSocket-Key: " +
        crypto.randomBytes(16).toString("base64") +
        "\r\n" +
        "Sec-WebSocket-Version: 13\r\n\r\n",
    );
  });
  socket.once("data", function (data) {
    if (data.toString().indexOf("101 Switching Protocols") < 0) {
      throw new Error("WebSocket handshake failed");
    }
    callback(socket);
  });
  return socket;
}

// FIN|BIN, masked, 127-length marker, then a 64-bit length and a 4-byte mask.
function longFormHeader(high32, low32) {
  const header = Buffer.alloc(14);
  header[0] = 0x82;
  header[1] = 0xff;
  header.writeUInt32BE(high32, 2);
  header.writeUInt32BE(low32, 6);
  header.writeUInt32BE(0xdeadbeef, 10);
  return header;
}

// Runs the tail of a test exactly once, whichever of the socket closing or
// the timeout gets there first.
function once(server, socket, assert) {
  let done = false;
  return function (ok, message) {
    if (done) {
      return;
    }
    done = true;
    if (ok) {
      assert.pass(message);
    } else {
      assert.fail(message);
    }
    socket.destroy();
    server.close();
    assert.end();
  };
}

// A well-formed masked frame carrying payload.
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

test("a frame declaring more payload than it carries is rejected", function (assert) {
  startServer(function (server, port) {
    const before = process.memoryUsage().external;
    const socket = upgrade(port, function (sock) {
      // 512 MiB declared, no payload at all.
      sock.write(longFormHeader(0, 0x20000000));
    });
    const finish = once(server, socket, assert);

    socket.on("close", function () {
      const grew = process.memoryUsage().external - before;
      finish(
        grew < 64 * 1024 * 1024,
        "the declared length did not size an allocation (external memory grew by " +
          Math.round(grew / (1024 * 1024)) +
          " MiB)",
      );
    });
    socket.on("error", function () {});
    setTimeout(function () {
      finish(false, "the connection was left open");
    }, 5000).unref();
  });
});

test("a payload length that does not fit in 32 bits is rejected", function (assert) {
  startServer(function (server, port) {
    const socket = upgrade(port, function (sock) {
      // The high half of the 64-bit length is where the size really is; the
      // decoder read the low half only, and saw a four-byte frame.
      sock.write(longFormHeader(0x00000001, 0x00000004));
      sock.write(Buffer.from([0x00, 0x00, 0x00, 0x00]));
    });
    const finish = once(server, socket, assert);

    socket.on("close", function () {
      finish(true, "the connection was closed");
    });
    socket.on("error", function () {});
    setTimeout(function () {
      finish(false, "the connection was left open");
    }, 5000).unref();
  });
});

test("continuation fragments are not accumulated without limit", function (assert) {
  const maxLength = 64 * 1024;
  startServer(function (server, port) {
    const fragment = maskedFrame(Buffer.alloc(1000, 0x61), false);
    let pump = null;

    const socket = upgrade(port, function (sock) {
      // Never send the final fragment: without a cap this grows for as long
      // as the peer keeps sending.
      pump = setInterval(function () {
        if (sock.destroyed || !sock.writable) {
          clearInterval(pump);
          return;
        }
        for (let i = 0; i < 20; i++) {
          sock.write(fragment);
        }
      }, 1);
      pump.unref();
    });
    const done = once(server, socket, assert);
    const finish = function (ok, message) {
      if (pump) {
        clearInterval(pump);
      }
      done(ok, message);
    };

    socket.on("close", function () {
      finish(true, "the connection was closed");
    });
    socket.on("error", function () {});
    setTimeout(function () {
      finish(false, "fragments were accumulated without limit");
    }, 10000).unref();
  }, smallTransport(maxLength));
});

test("a frame split across reads is reassembled", function (assert) {
  let socket = null;
  let finish = null;

  // A payload with a recognisable shape, so that the test can tell a
  // reassembled message from a truncated one padded with zeroes.
  const payload = Buffer.alloc(20000);
  for (let i = 0; i < payload.length; i++) {
    payload[i] = (i % 251) + 1;
  }

  const server = thrift.createWebServer({
    services: {
      "/": {
        processor: {
          Processor: function () {
            this.process = function (input) {
              let got = null;
              try {
                got = input.trans.read(payload.length);
              } catch (e) {
                finish(false, "the processor could not read the message: " + e);
                return;
              }
              finish(
                Buffer.compare(got, payload) === 0,
                "the message reached the processor whole",
              );
            };
          },
        },
        handler: {},
      },
    },
  });

  server.listen(0, "127.0.0.1", function () {
    socket = upgrade(server.address().port, function (sock) {
      const frame = maskedFrame(payload);
      // Hand the frame over in pieces, the way TCP would.
      let offset = 0;
      const send = function () {
        if (offset >= frame.length) {
          return;
        }
        const end = Math.min(offset + 1400, frame.length);
        sock.write(frame.subarray(offset, end));
        offset = end;
        setTimeout(send, 1).unref();
      };
      send();
    });
    socket.on("error", function () {});
    finish = once(server, socket, assert);
    setTimeout(function () {
      finish(false, "the message never reached the processor whole");
    }, 10000).unref();
  });
});

test("an ordinary frame is still delivered", function (assert) {
  let socket = null;
  let finish = null;

  const server = thrift.createWebServer({
    services: {
      "/": {
        processor: {
          Processor: function () {
            this.process = function () {
              finish(true, "the processor saw the message");
            };
          },
        },
        handler: {},
      },
    },
  });

  server.listen(0, "127.0.0.1", function () {
    socket = upgrade(server.address().port, function (sock) {
      // A short binary message, well inside every limit.
      sock.write(
        maskedFrame(
          Buffer.from([
            0x80, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04, 0x70, 0x69, 0x6e,
            0x67, 0x00,
          ]),
        ),
      );
    });
    socket.on("error", function () {});
    finish = once(server, socket, assert);
    setTimeout(function () {
      finish(false, "the message never reached the processor");
    }, 5000).unref();
  });
});
