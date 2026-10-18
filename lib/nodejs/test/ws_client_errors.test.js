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

// A response the client cannot route -- a sequence id that is not a safe
// integer (JSON does not constrain the type, so it can be a string such as
// "__proto__"), a wrong protocol version, or input that is not JSON -- is
// reported through the connection's "error" event, like any other connection
// error, instead of being thrown out of the socket callback where it would end
// the client. The WS cases run a client in a child process, because on the
// unfixed tree the throw ends that process.

const test = require("tape");
const thrift = require("thrift");
const childProcess = require("child_process");
const { WebSocketServer } = require("ws");
const XHRConnection =
  require("thrift/lib/nodejs/lib/thrift/xhr_connection").XHRConnection;

// Each response is a well-formed TJSONProtocol message the client cannot map
// back to a request it made.
const RESPONSES = {
  "a sequence id that is not a safe integer": '[1,"x",2,"__proto__",{}]',
  "a wrong protocol version": '[2,"x",2,1,{}]',
  "input that is not JSON": "this is not a thrift message",
};

// The child: a WS client that registers the documented "error" listener and
// sends one request. It prints GOT_ERROR and exits 0 when the error is reported
// (so the process is still running), NO_ERROR / exit 2 if nothing arrives, and
// a non-zero exit from an uncaught error if it escaped instead of being reported.
function runChildClient(port) {
  function Client(output, Protocol) {
    this.output = output;
    this.Protocol = Protocol;
    this._reqs = {};
  }
  Client.prototype.hello = function () {
    this._reqs[1] = function () {};
    const p = new this.Protocol(this.output);
    p.writeMessageBegin("hello", thrift.Thrift.MessageType.CALL, 1);
    p.writeStructBegin("args");
    p.writeFieldStop();
    p.writeStructEnd();
    p.writeMessageEnd();
    this.output.flush();
  };
  Client.prototype.recv_hello = function () {};

  const connection = thrift.createWSConnection("127.0.0.1", port, {
    transport: thrift.TBufferedTransport,
    protocol: thrift.TJSONProtocol,
    path: "/",
  });
  thrift.createWSClient({ Client }, connection);
  connection.on("error", function (e) {
    console.log("GOT_ERROR " + (e && e.name));
    connection.close();
    process.exit(0);
  });
  connection.on("open", function () {
    connection.client.hello();
  });
  connection.open();
  setTimeout(function () {
    console.log("NO_ERROR");
    process.exit(2);
  }, 1500);
}

// The parent: a one-shot WS server that replies with "response" on the first
// message, a forked child client, and its exit code and output.
function runCase(response, done) {
  const server = new WebSocketServer({ host: "127.0.0.1", port: 0 });
  server.on("listening", function () {
    server.once("connection", function (sock) {
      sock.once("message", function () {
        sock.send(response);
      });
    });
    const child = childProcess.fork(
      __filename,
      ["wsclient", String(server.address().port)],
      { stdio: ["ignore", "pipe", "pipe", "ipc"] },
    );
    let out = "";
    child.stdout.on("data", function (d) {
      out += d;
    });
    child.stderr.on("data", function (d) {
      out += d;
    });
    const kill = setTimeout(function () {
      child.kill("SIGKILL");
    }, 4000);
    child.on("close", function (code) {
      clearTimeout(kill);
      server.close(function () {
        done(code, out.trim());
      });
    });
  });
}

if (process.argv[2] === "wsclient") {
  runChildClient(Number(process.argv[3]));
} else {
  Object.keys(RESPONSES).forEach(function (what) {
    test(
      "the WS client reports and keeps running on " + what,
      function (assert) {
        runCase(RESPONSES[what], function (code, out) {
          assert.notEqual(
            code,
            1,
            "the client process did not end with an uncaught error (exit " +
              code +
              ", output: " +
              JSON.stringify(out) +
              ")",
          );
          assert.ok(
            /GOT_ERROR/.test(out),
            "the connection reported the error to its listener",
          );
          assert.equal(code, 0, "the client exited cleanly after reporting it");
          assert.end();
        });
      },
    );
  });

  // The XHR connection has the same decode loop; drive it in-process through
  // setRecvBuffer. A wrong-version reply throws inside readMessageBegin before
  // any client lookup, so no client object is needed.
  test("the XHR client reports a decode error instead of throwing it", function (assert) {
    const connection = new XHRConnection("127.0.0.1", 9099, {});
    let emitted = null;
    connection.on("error", function (e) {
      emitted = e;
    });
    let threw = null;
    try {
      connection.setRecvBuffer(Buffer.from('[2,"x",2,1,{}]', "utf8"));
    } catch (e) {
      threw = e;
    }
    assert.error(threw, "setRecvBuffer did not throw the decode error out");
    assert.ok(emitted, "the error was emitted on the connection");
    assert.end();
  });
}
