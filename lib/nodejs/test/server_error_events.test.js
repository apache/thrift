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

// An error on one connection is reported through the server's "error" event
// when the application listens for it, and through the library log when it
// does not. Either way that connection is closed and the server goes on
// serving the others.

const test = require("tape");
const net = require("net");
const childProcess = require("child_process");
const thrift = require("thrift");
const ThriftTest = require("./gen-nodejs/ThriftTest");

// The first four bytes of a binary message, carrying an unknown version.
const BAD_VERSION = Buffer.from([0x80, 0x00, 0x00, 0x00]);

const handler = {
  testString: function (thing, result) {
    result(null, thing);
  },
};

// Runs in a child process: serves ThriftTest without an "error" listener and
// hands the port and every line the library logs to the parent.
function serve() {
  thrift.setLogFunc(function (msg) {
    process.send({ log: msg });
  });
  const server = thrift.createServer(ThriftTest, handler);
  server.listen(0, "127.0.0.1", function () {
    process.send({ port: server.address().port });
  });
}

function startServerProcess(onListening) {
  const child = childProcess.fork(__filename, ["serve"], {
    stdio: ["ignore", "ignore", "pipe", "ipc"],
  });
  const state = { child: child, logs: [], stderr: "" };
  child.stderr.on("data", function (chunk) {
    state.stderr += chunk;
  });
  child.on("message", function (msg) {
    if (msg.port) {
      onListening(msg.port);
    } else if (msg.log) {
      state.logs.push(msg.log);
    }
  });
  return state;
}

function callTestString(port, value, done) {
  const connection = thrift.createConnection("127.0.0.1", port, {});
  const client = thrift.createClient(ThriftTest, connection);
  let finished = false;
  function finish(err, response) {
    if (!finished) {
      finished = true;
      connection.end();
      done(err, response);
    }
  }
  connection.on("error", finish);
  client.testString(value, finish);
}

// Opens a raw connection, lets "act" misbehave on it, and calls back once the
// connection is closed and the server has had a moment to react.
function misbehave(port, act, done) {
  const socket = net.connect(port, "127.0.0.1", function () {
    act(socket);
  });
  socket.on("error", function () {});
  socket.on("close", function () {
    setTimeout(done, 200);
  });
}

// Checks that the server process is still running, that it still answers a
// regular call, and that the connection's error went to the library log.
function assertStillServing(assert, server, port, logged) {
  const child = server.child;
  assert.equal(child.exitCode, null, "the server process is still running");
  if (child.exitCode !== null) {
    assert.comment("server process stderr:\n" + server.stderr);
    assert.end();
    return;
  }
  callTestString(port, "still serving", function (err, response) {
    assert.error(err, "a later call succeeds");
    assert.equal(response, "still serving", "and gets its answer");
    assert.ok(
      server.logs.some(function (line) {
        return logged.test(line);
      }),
      "the connection's error was logged",
    );
    child.kill();
    assert.end();
  });
}

const cases = {
  "createServer without an error listener logs a bad message and keeps serving":
    function (assert) {
      const server = startServerProcess(function (port) {
        misbehave(
          port,
          function (socket) {
            socket.write(BAD_VERSION);
          },
          function () {
            assertStillServing(assert, server, port, /Bad version/);
          },
        );
      });
    },

  "createServer without an error listener keeps serving after a connection reset":
    function (assert) {
      if (typeof net.Socket.prototype.resetAndDestroy !== "function") {
        assert.comment("skipped: this Node.js cannot reset a connection");
        assert.end();
        return;
      }
      const server = startServerProcess(function (port) {
        misbehave(
          port,
          function (socket) {
            socket.resetAndDestroy();
          },
          function () {
            assertStillServing(assert, server, port, /ECONNRESET/);
          },
        );
      });
    },

  "createServer with an error listener receives the error and does not log it":
    function (assert) {
      const logged = [];
      thrift.setLogFunc(function (msg) {
        logged.push(msg);
      });
      const server = thrift.createServer(ThriftTest, handler);
      const errors = [];
      server.on("error", function (err) {
        errors.push(err);
      });
      server.listen(0, "127.0.0.1", function () {
        misbehave(
          server.address().port,
          function (socket) {
            socket.write(BAD_VERSION);
          },
          function () {
            thrift.setLogFunc(console.log);
            assert.equal(errors.length, 1, "one error event");
            assert.ok(
              errors[0] instanceof thrift.Thrift.TProtocolException,
              "carrying the TProtocolException",
            );
            assert.equal(
              errors[0] && errors[0].type,
              thrift.Thrift.TProtocolExceptionType.BAD_VERSION,
              "of type BAD_VERSION",
            );
            assert.deepEqual(logged, [], "nothing was logged");
            server.close(function () {
              assert.end();
            });
          },
        );
      });
    },
};

if (process.argv[2] === "serve") {
  serve();
} else {
  Object.keys(cases).forEach(function (caseName) {
    test(caseName, cases[caseName]);
  });
}
