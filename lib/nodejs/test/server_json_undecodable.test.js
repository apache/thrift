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

// The server closes a connection whose JSON message cannot be decoded, and
// keeps serving other connections.

const test = require("tape");
const net = require("net");
const thrift = require("thrift");
const ThriftTest = require("./gen-nodejs/ThriftTest");

// A testString call whose argument field carries a type key TJSONProtocol
// does not know.
const UNDECODABLE = Buffer.from('[1,"testString",1,1,{"1":{"zzz":1}}]');

const options = {
  protocol: thrift.TJSONProtocol,
  transport: thrift.TBufferedTransport,
};

const handler = {
  testString: function (thing, result) {
    result(null, thing);
  },
};

function startServer(callback) {
  const server = thrift.createServer(ThriftTest, handler, options);
  server.on("error", function () {});
  server.listen(0, "127.0.0.1", function () {
    callback(server, server.address().port);
  });
}

test("the server closes a connection that sent an undecodable JSON message", function (assert) {
  assert.timeoutAfter(10000);
  startServer(function (server, port) {
    let closed = false;
    const socket = net.connect(port, "127.0.0.1", function () {
      socket.write(UNDECODABLE);
      // one more byte on the same connection
      setTimeout(function () {
        if (!socket.destroyed) {
          socket.write("x");
        }
      }, 100);
    });
    socket.on("error", function () {});
    socket.on("close", function () {
      closed = true;
    });
    setTimeout(function () {
      assert.ok(closed, "the server closed the connection");
      socket.destroy();

      const connection = thrift.createConnection("127.0.0.1", port, options);
      const client = thrift.createClient(ThriftTest, connection);
      connection.on("error", function (err) {
        assert.fail(err);
      });
      client.testString("still serving", function (err, response) {
        assert.error(err, "a later call succeeds");
        assert.equal(response, "still serving", "and gets its answer");
        connection.end();
        server.close();
        assert.end();
      });
    }, 1000);
  });
});
