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

// A handler that fails asynchronously ends its own call only: a oneway handler
// that rejects, a promise-style handler that rejects with something other than
// an Error, and a callback-style handler that returns a rejected promise. The
// server process goes on serving, for the ES5 and the ES6 generated code.

const test = require("tape");
const childProcess = require("child_process");
const thrift = require("thrift");

const GENERATED = {
  es5: "./gen-nodejs/ThriftTest",
  es6: "./gen-nodejs-es6/ThriftTest",
};

// Runs in a child process: serves ThriftTest from the given generated code with
// handlers that fail, and hands the port to the parent.
function serve(variant) {
  const ThriftTest = require(GENERATED[variant]);
  const handler = {
    // oneway, asynchronous: there is no reply to report the failure in
    testOneway: async function (secondsToSleep) {
      throw new Error("the oneway handler failed after " + secondsToSleep);
    },
    // promise style, rejecting with null; the parameter makes it promise style
    // eslint-disable-next-line no-unused-vars
    testString: function (thing) {
      return Promise.reject(null);
    },
    // promise style, rejecting with an object whose message is no string
    testI32: function (thing) {
      return Promise.reject({ message: { reason: thing } });
    },
    // callback style by its length, but it returns a rejected promise and never calls back
    testByte: function () {
      return Promise.reject(new Error("rejected instead of calling back"));
    },
    testVoid: function (result) {
      result(null);
    },
  };
  const server = thrift.createServer(ThriftTest, handler);
  server.listen(0, "127.0.0.1", function () {
    process.send({ port: server.address().port });
  });
}

function startServerProcess(variant, onListening) {
  const child = childProcess.fork(__filename, ["serve", variant], {
    stdio: ["ignore", "ignore", "pipe", "ipc"],
  });
  const state = { child: child, stderr: "" };
  child.stderr.on("data", function (chunk) {
    state.stderr += chunk;
  });
  child.on("message", function (msg) {
    if (msg.port) {
      onListening(msg.port);
    }
  });
  return state;
}

// Hands the outcome of a client call to a callback; both generated clients
// return a promise when they get no callback.
function settle(promise, finish) {
  promise.then(function (response) {
    finish(null, response);
  }, finish);
}

// Makes one call on a connection of its own, and calls back with its outcome
// within five seconds.
function call(variant, port, invoke, done) {
  const ThriftTest = require(GENERATED[variant]);
  const connection = thrift.createConnection("127.0.0.1", port, {});
  const client = thrift.createClient(ThriftTest, connection);
  let finished = false;
  const timer = setTimeout(function () {
    finish(new Error("no answer within five seconds"));
  }, 5000);
  function finish(err, response) {
    if (!finished) {
      finished = true;
      clearTimeout(timer);
      connection.end();
      done(err, response);
    }
  }
  connection.on("error", finish);
  invoke(client, finish);
}

// Makes the failing call, then checks that the server process still runs and
// still answers a regular call.
function checkCall(variant, invoke, check) {
  return function (assert) {
    const server = startServerProcess(variant, function (port) {
      call(variant, port, invoke, function (err, response) {
        check(assert, err, response);
        // give the server a moment before looking
        setTimeout(function () {
          assert.equal(
            server.child.exitCode,
            null,
            "the server process is still running",
          );
          if (server.child.exitCode !== null) {
            assert.comment("server process stderr:\n" + server.stderr);
            assert.end();
            return;
          }
          call(
            variant,
            port,
            function (client, finish) {
              settle(client.testVoid(), finish);
            },
            function (err) {
              assert.error(err, "a later call succeeds");
              server.child.kill();
              assert.end();
            },
          );
        }, 300);
      });
    });
  };
}

function expectApplicationException(message) {
  return function (assert, err) {
    assert.ok(
      err instanceof thrift.Thrift.TApplicationException,
      "the call ends with a TApplicationException",
    );
    assert.equal(err && err.message, message, "whose message is " + message);
  };
}

const cases = {
  "a oneway handler that rejects": {
    invoke: function (client, finish) {
      // a oneway call gets no answer; give the server time to run the handler
      const sent = client.testOneway(1);
      if (sent && typeof sent.then === "function") {
        sent.then(null, function () {});
      }
      setTimeout(finish, 300);
    },
    check: function () {},
  },
  "a promise-style handler that rejects with null": {
    invoke: function (client, finish) {
      settle(client.testString("x"), finish);
    },
    check: expectApplicationException("Unknown error"),
  },
  "a promise-style handler that rejects with an odd message": {
    invoke: function (client, finish) {
      settle(client.testI32(7), finish);
    },
    check: expectApplicationException("[object Object]"),
  },
  "a callback-style handler that returns a rejected promise": {
    invoke: function (client, finish) {
      settle(client.testByte(1), finish);
    },
    check: expectApplicationException("rejected instead of calling back"),
  },
};

if (process.argv[2] === "serve") {
  serve(process.argv[3]);
} else {
  Object.keys(GENERATED).forEach(function (variant) {
    Object.keys(cases).forEach(function (caseName) {
      const c = cases[caseName];
      test(variant + ": " + caseName, checkCall(variant, c.invoke, c.check));
    });
  });
}
