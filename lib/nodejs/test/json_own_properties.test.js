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

// A member name is data, not a lever on the object it is stored in, and a
// field the parser produced has to be one the reader can consume and then get
// rid of. These cover both halves for TJSONProtocol.

const test = require("tape");
const json_parse = require("thrift/lib/nodejs/lib/thrift/json_parse");
const TJSONProtocol = require("thrift/lib/nodejs/lib/thrift/json_protocol");
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");
const Thrift = require("thrift/lib/nodejs/lib/thrift/thrift");

// -- json_parse: every member is an own property, as with JSON.parse ---------

test("json_parse keeps __proto__ as an own member", function (assert) {
  const parsed = json_parse('{"__proto__": {"i32": 1}}');

  assert.deepEqual(
    Object.keys(parsed),
    ["__proto__"],
    "__proto__ is an own key",
  );
  assert.equal(
    Object.getPrototypeOf(parsed),
    Object.prototype,
    "the object keeps its prototype",
  );
  const descriptor = Object.getOwnPropertyDescriptor(parsed, "__proto__");
  assert.deepEqual(
    descriptor && descriptor.value,
    { i32: 1 },
    "the member holds the parsed value",
  );
  assert.end();
});

test("json_parse agrees with JSON.parse on member names", function (assert) {
  const texts = [
    '{"a": 1, "b": 2}',
    '{"__proto__": {"x": 1}}',
    '{"constructor": {"x": 1}}',
    '{"prototype": 1, "__proto__": 2}',
    '{"a": {"__proto__": {"nested": true}}}',
  ];
  texts.forEach(function (text) {
    const ours = json_parse(text);
    const native = JSON.parse(text);
    assert.deepEqual(
      Object.keys(ours),
      Object.keys(native),
      "same own keys for " + text,
    );
    assert.equal(
      Object.getPrototypeOf(ours),
      Object.getPrototypeOf(native),
      "same prototype for " + text,
    );
  });
  assert.end();
});

test("json_parse still rejects a duplicated __proto__", function (assert) {
  assert.throws(
    function () {
      json_parse('{"__proto__": 1, "__proto__": 2}');
    },
    /Duplicate key/,
    "the duplicate-key guard covers __proto__ too",
  );
  assert.end();
});

// -- readFieldBegin: a field it reports must be one it can consume -----------

function protocolFor(json) {
  let prot = null;
  const receiver = TBufferedTransport.receiver(function (transport) {
    prot = new TJSONProtocol(transport);
  });
  receiver(Buffer.from(json, "utf8"));
  return prot;
}

// Runs the loop generated read() uses: it leaves only on STOP.
function readAllFields(prot, budget) {
  const fields = [];
  for (let i = 0; i < budget; i++) {
    const r = prot.readFieldBegin();
    if (r.ftype === Thrift.Type.STOP) {
      return fields;
    }
    fields.push(r.fid);
    prot.readFieldEnd();
  }
  return null; // never reached STOP
}

test("readFieldBegin reaches STOP on an ordinary struct", function (assert) {
  const prot = protocolFor('[1,"m",1,0,{"1":{"i32":5},"2":{"str":"x"}}]');
  prot.readMessageBegin();
  prot.readStructBegin();
  assert.deepEqual(readAllFields(prot, 100), [1, 2], "both fields, then STOP");
  assert.end();
});

test("readFieldBegin reaches STOP with a __proto__ member present", function (assert) {
  const prot = protocolFor('[1,"m",1,0,{"__proto__":{"1":{"i32":1}}}]');
  prot.readMessageBegin();
  prot.readStructBegin();
  assert.notEqual(
    readAllFields(prot, 1000),
    null,
    "the read loop terminates rather than repeating a field it cannot delete",
  );
  assert.end();
});

test("readFieldBegin ignores inherited members", function (assert) {
  const prot = protocolFor('[1,"m",1,0,{}]');
  prot.readMessageBegin();
  prot.readStructBegin();

  // What a replaced prototype leaves behind, built directly so that this holds
  // even if something upstream starts producing such an object again: the key
  // is enumerable through for...in and delete cannot remove it.
  prot.rstack = [Object.create({ 1: { i32: 1 } })];
  prot.rpos = [];

  assert.deepEqual(
    readAllFields(prot, 1000),
    [],
    "an inherited field is not reported, and the loop ends",
  );
  assert.end();
});
