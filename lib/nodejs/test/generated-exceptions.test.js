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

// ES5 generated types (pre-ES6 path)
const ttypesEs5 = require("./gen-nodejs/ThriftTest_types");
// ES6 generated types
const ttypesEs6 = require("./gen-nodejs-es6/ThriftTest_types");

function serializeBinary(data) {
  let buff;
  const transport = new thrift.TBufferedTransport(null, function (msg) {
    buff = msg;
  });
  const prot = new thrift.TBinaryProtocol(transport);
  data[Symbol.for("write")](prot);
  prot.flush();
  return buff;
}

// Test that ES6 generated exception constructor passes the exception name
// (not the args object) to super(), matching the ES5 behavior.
// Regression test for: https://github.com/apache/thrift/pull/3372

test("ES6 generated exception - constructor sets name and message correctly", function t(assert) {
  const e = new ttypesEs6.Xception({ errorCode: 1001, message: "test error" });
  assert.ok(e instanceof thrift.Thrift.TException, "is instanceof TException");
  assert.ok(e instanceof Error, "is instanceof Error");
  assert.equal(e.name, "Xception", "name is set to exception class name");
  assert.equal(e.errorCode, 1001, "custom field errorCode is set");
  assert.equal(typeof e.stack, "string", "has stack trace");
  assert.end();
});

test("ES6 generated exception - super() receives string, not args object", function t(assert) {
  const e = new ttypesEs6.Xception({ errorCode: 1001, message: "test error" });
  // The bug was that super(args) passed the args object to TException,
  // which would cause message to be "[object Object]"
  assert.notEqual(e.message, "[object Object]",
    "message is not '[object Object]' (would indicate args object was passed to super)");
  assert.end();
});

test("ES6 generated exception - serialization does not throw", function t(assert) {
  const e = new ttypesEs6.Xception({ errorCode: 1001, message: "test error" });
  assert.doesNotThrow(function () {
    serializeBinary(e);
  }, "serializing an ES6 exception should not throw");
  assert.end();
});

test("ES5 generated exception - constructor sets name and message correctly", function t(assert) {
  const e = new ttypesEs5.Xception({ errorCode: 1001, message: "test error" });
  assert.ok(e instanceof thrift.Thrift.TException, "is instanceof TException");
  assert.ok(e instanceof Error, "is instanceof Error");
  assert.equal(e.name, "Xception", "name is set to exception class name");
  assert.equal(e.errorCode, 1001, "custom field errorCode is set");
  assert.end();
});

test("ES5 and ES6 generated exceptions have consistent behavior", function t(assert) {
  const es5 = new ttypesEs5.Xception({ errorCode: 1001, message: "test error" });
  const es6 = new ttypesEs6.Xception({ errorCode: 1001, message: "test error" });
  assert.equal(es5.name, es6.name, "name matches between ES5 and ES6");
  assert.equal(es5.errorCode, es6.errorCode, "errorCode matches between ES5 and ES6");
  assert.end();
});
