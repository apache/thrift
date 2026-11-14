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

// A POST body whose message fails to decode gets one 500 response, and the
// server answers the requests that follow.

const test = require("tape");
const net = require("net");
const childProcess = require("child_process");
const thrift = require("thrift");
const ThriftTest = require("./gen-nodejs/ThriftTest");

// Four bytes that start a binary message with an unknown version.
const BAD = Buffer.from([0xff, 0x00, 0x00, 0x00]);

// The server runs in a child process of its own.
function serve() {
  const server = thrift.createWebServer({
    services: {
      "/svc": {
        processor: ThriftTest,
        handler: {},
        protocol: thrift.TBinaryProtocol,
        transport: thrift.TBufferedTransport,
      },
    },
  });
  server.listen(0, "127.0.0.1", function () {
    process.send({ port: server.address().port });
  });
}

function chunk(buf) {
  return Buffer.concat([
    Buffer.from(buf.length.toString(16) + "\r\n"),
    buf,
    Buffer.from("\r\n"),
  ]);
}

// Sends a chunked POST whose body chunks are written apart, so the server
// sees one "data" event per chunk, and calls back with the status line.
function post(port, chunks, done) {
  let reply = "";
  const socket = net.connect(port, "127.0.0.1", function () {
    socket.write(
      "POST /svc HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n",
    );
    let i = 0;
    (function next() {
      if (socket.destroyed) {
        return;
      }
      if (i < chunks.length) {
        socket.write(chunk(chunks[i++]));
        setTimeout(next, 50);
      } else {
        socket.write("0\r\n\r\n");
      }
    })();
  });
  socket.on("data", function (data) {
    reply += data;
  });
  socket.on("error", function () {});
  socket.on("close", function () {
    done(reply.split("\r\n")[0]);
  });
  setTimeout(function () {
    socket.destroy();
  }, 1000);
}

function postTest() {
  test("a POST whose body keeps failing to decode gets one 500 and the server keeps running", function (assert) {
    assert.timeoutAfter(10000);
    const child = childProcess.fork(__filename, ["serve"], {
      stdio: ["ignore", "ignore", "pipe", "ipc"],
    });
    // the child holds a listening server: stop it however the test ends
    assert.on("end", function () {
      child.kill();
    });
    process.on("exit", function () {
      child.kill();
    });
    let stderr = "";
    child.stderr.on("data", function (data) {
      stderr += data;
    });
    child.on("message", function (msg) {
      post(msg.port, [BAD, BAD], function (status) {
        assert.equal(
          status,
          "HTTP/1.1 500 Internal Server Error",
          "the request gets a 500",
        );
        setTimeout(function () {
          assert.equal(child.exitCode, null, "the server keeps serving");
          if (child.exitCode !== null) {
            assert.comment("server process stderr:\n" + stderr);
            assert.end();
            return;
          }
          post(msg.port, [BAD], function (status) {
            assert.equal(
              status,
              "HTTP/1.1 500 Internal Server Error",
              "a later request is answered",
            );
            assert.end();
          });
        }, 200);
      });
    });
  });
}

if (process.argv[2] === "serve") {
  serve();
} else {
  postTest();
}
