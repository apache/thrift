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

"use strict";

const test = require("tape");
const thrift = require("thrift");
const ThriftTest = require("./gen-nodejs/ThriftTest");

test("generated clients reset the transport after serialization errors", function t(assert) {
  const flushed = [];
  const transport = new thrift.TBufferedTransport(null, function (message) {
    flushed.push(message);
  });
  const client = new ThriftTest.Client(transport, thrift.TBinaryProtocol);

  assert.throws(
    function () {
      client.testString(42, function () {});
    },
    /writeString called without a string\/Buffer argument/,
    "the serialization error is rethrown",
  );
  assert.equal(transport.outCount, 0, "the partial message is discarded");
  assert.equal(
    transport._seqid,
    null,
    "the partial message sequence ID is discarded",
  );

  client.testVoid(function () {});

  assert.equal(flushed.length, 1, "the next request is flushed by itself");
  let input;
  thrift.TBufferedTransport.receiver(function (transport) {
    input = transport;
  })(flushed[0]);
  const protocol = new thrift.TBinaryProtocol(input);
  assert.equal(
    protocol.readMessageBegin().fname,
    "testVoid",
    "the next request is not prefixed by the partial message",
  );
  assert.end();
});

test("generated clients reset framed transports after serialization errors", function t(assert) {
  const flushed = [];
  const transport = new thrift.TFramedTransport(null, function (message) {
    flushed.push(message);
  });
  const client = new ThriftTest.Client(transport, thrift.TBinaryProtocol);

  assert.throws(
    function () {
      client.testString(42, function () {});
    },
    /writeString called without a string\/Buffer argument/,
    "the serialization error is rethrown",
  );
  assert.equal(transport.outCount, 0, "the partial message is discarded");
  assert.equal(
    transport._seqid,
    null,
    "the partial message sequence ID is discarded",
  );

  client.testVoid(function () {});

  assert.equal(flushed.length, 1, "the next request is flushed by itself");
  let input;
  thrift.TFramedTransport.receiver(function (transport) {
    input = transport;
  })(flushed[0]);
  const protocol = new thrift.TBinaryProtocol(input);
  assert.equal(
    protocol.readMessageBegin().fname,
    "testVoid",
    "the next request is not prefixed by the partial message",
  );
  assert.end();
});
