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

// A response a StdIO client cannot decode -- a wrong protocol version, or a
// sequence id that is not a safe integer (JSON does not constrain the type, so
// it can be a string such as "constructor") -- is reported through the
// connection's "error" event, like any other connection error, instead of
// being thrown out of the stdout data callback where it would end the client
// process. The subprocess case runs a client in a child process, because on
// the unfixed tree the throw ends that process.

if (process.argv[2] === "stdiopeer") {
  // A minimal StdIO peer: write one response the client cannot decode -- a
  // message header whose first four bytes are a protocol version the client
  // does not speak -- then stay attached until the client closes our stdin.
  process.stdout.write(Buffer.from([0x80, 0x02, 0x00, 0x01]));
  process.stdin.on("end", function () {
    process.exit(0);
  });
  process.stdin.on("close", function () {
    process.exit(0);
  });
  process.stdin.resume();
  // Upper bound so a run never leaves this process behind.
  setTimeout(function () {
    process.exit(0);
  }, 10000);
} else if (process.argv[2] === "stdioclient") {
  // The client: a StdIO connection to the peer above, with the documented
  // "error" listener registered. It prints GOT_ERROR and exits 0 when the
  // error is reported (so the process is still running), NO_ERROR / exit 2 if
  // nothing arrives, and a non-zero exit if the error escaped instead.
  const thrift = require("thrift");
  const connection = thrift.createStdIOConnection(
    "node " + __filename + " stdiopeer",
    {},
  );
  connection.on("error", function (e) {
    console.log("GOT_ERROR " + (e && e.name));
    connection.end();
    process.exit(0);
  });
  setTimeout(function () {
    console.log("NO_ERROR");
    process.exit(2);
  }, 1500);
} else {
  const test = require("tape");
  const thrift = require("thrift");
  const childProcess = require("child_process");

  // The subprocess case: fork the client above, let its peer send a response
  // it cannot decode, and check the client's exit code and output. On the
  // fixed tree it reports the error and exits 0; on the unfixed tree the throw
  // ends it with exit 1.
  test("the StdIO client reports a decode error and keeps running", function (assert) {
    const child = childProcess.fork(__filename, ["stdioclient"], {
      stdio: ["ignore", "pipe", "pipe", "ipc"],
    });
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
      assert.notEqual(
        code,
        1,
        "the client process did not end with an uncaught error (exit " +
          code +
          ", output: " +
          JSON.stringify(out.trim()) +
          ")",
      );
      assert.ok(
        /GOT_ERROR/.test(out),
        "the connection reported the error to its listener",
      );
      assert.equal(code, 0, "the client exited cleanly after reporting it");
      assert.end();
    });
  });

  // The same decode loop driven in-process: feed a wrong-version response to
  // the connection's stdout stream and confirm it is emitted, not thrown out
  // of the data callback. The peer is a long-lived process so the pipes exist.
  test("a StdIO decode error is emitted, not thrown out of the data callback", function (assert) {
    const connection = thrift.createStdIOConnection("cat", {});
    let emitted = null;
    connection.on("error", function (e) {
      emitted = e;
    });
    let threw = null;
    try {
      connection.child.stdout.emit(
        "data",
        Buffer.from([0x80, 0x02, 0x00, 0x01]),
      );
    } catch (e) {
      threw = e;
    }
    assert.error(
      threw,
      "the decode error was not thrown out of the stdout data callback",
    );
    assert.ok(emitted, "the error was reported on the connection");
    connection.child.kill("SIGKILL");
    assert.end();
  });

  // A sequence id that is not a safe integer (here the string "constructor",
  // which a plain object resolves to an inherited function) must not be used
  // to look a request up: the response is reported through "error" rather than
  // routed to an inherited member and silently dropped.
  test("a StdIO response with a non-integer sequence id is reported, not silently dropped", function (assert) {
    const connection = thrift.createStdIOConnection("cat", {
      protocol: thrift.TJSONProtocol,
    });
    connection.client = {
      _reqs: {},
      recv_hello: function (message, mtype, dummy_seqid) {
        this._reqs[dummy_seqid](null, "result");
      },
    };
    let emitted = null;
    connection.on("error", function (e) {
      emitted = e;
    });
    let threw = null;
    try {
      connection.child.stdout.emit(
        "data",
        Buffer.from('[1,"hello",2,"constructor",{}]', "utf8"),
      );
    } catch (e) {
      threw = e;
    }
    assert.error(
      threw,
      "the response was not thrown out of the stdout data callback",
    );
    assert.ok(
      emitted,
      "the mis-sequenced response was reported on the connection",
    );
    connection.child.kill("SIGKILL");
    assert.end();
  });

  // The safe-integer guard now sits in front of every StdIO response, so a
  // valid one must still reach its callback unchanged.
  test("a valid StdIO response still reaches its callback", function (assert) {
    const connection = thrift.createStdIOConnection("cat", {
      protocol: thrift.TJSONProtocol,
    });
    let received = null;
    connection.client = {
      _reqs: {
        1: function (err, success) {
          received = { err: err, success: success };
        },
      },
      recv_hello: function (message, mtype, dummy_seqid) {
        this._reqs[dummy_seqid](null, "result");
      },
    };
    let emitted = null;
    connection.on("error", function (e) {
      emitted = e;
    });
    connection.child.stdout.emit(
      "data",
      Buffer.from('[1,"hello",2,1,{}]', "utf8"),
    );
    assert.notOk(emitted, "no error was reported for a valid response");
    assert.ok(
      received && received.success === "result",
      "the request callback received the response",
    );
    connection.child.kill("SIGKILL");
    assert.end();
  });
}
