#!/usr/bin/env node

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

const assert = require("assert");
const test = require("tape");
const thrift = require("thrift");
const program = require("commander");

program
  .option("--host <host>", "Set the thrift server host to connect", "localhost")
  .option("--port <port>", "Set the thrift server port number to connect", 9090)
  .parse(process.argv);

const Service = require("./gen-2/second-episode/gen-nodejs/Service");
const Types = require("types-package/first-episode/Types_types");

const host = program.host;
const port = program.port;

const options = {
  transport: thrift.TBufferedTransport,
  protocol: thrift.TJSONProtocol
};

const connection = thrift.createConnection(host, port, options);
const testDriver = function(client, callback) {
  test("NodeJS episodic compilation client-server test", function(assert) {
    const type1Object = new Types.Type1();
    type1Object.number = 42;
    type1Object.message = "The answer";
    client.testEpisode(type1Object, function(err, response) {
      assert.error(err, "no callback error");
      assert.equal(response.number, type1Object.number + 1);
      assert.equal(
        response.message,
        type1Object.message + " [Hello from the server]"
      );
      assert.end();
      callback("Server successfully tested");
    });
  });
};

connection.on("error", function(err) {
  assert(false, err);
});

const client = thrift.createClient(Service, connection);

runTests();

function runTests() {
  testDriver(client, function(status) {
    console.log(status);
    connection.destroy();
  });
}

exports.expressoTest = function() {};
