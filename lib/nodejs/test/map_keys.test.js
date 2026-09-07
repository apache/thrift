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

// A map key arrives from the peer, and a generated map is a plain object. The
// one name "__proto__" is not an ordinary member name on such an object: a
// plain assignment under it runs the setter inherited from Object.prototype,
// which either swaps the object's prototype or, for a value the setter will
// not take, discards the entry. Neither is storing a pair. Exercised through
// the generated read and write code rather than the protocol methods, since it
// is the generated code that does the storing.

const test = require("tape");
const thrift = require("thrift");
const ttypes = require("./gen-nodejs/MapKeyTest_types");

const protocols = {
  binary: thrift.TBinaryProtocol,
  compact: thrift.TCompactProtocol,
  json: thrift.TJSONProtocol,
};

// Building the payload needs defineProperty: writing { __proto__: v } in a
// source literal hits the very setter under test, so the entry would never
// exist to be serialized.
function withKey(key, value) {
  const holder = {};
  Object.defineProperty(holder, key, {
    value: value,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  return holder;
}

function roundTrip(Protocol, holder) {
  const chunks = [];
  const writeTransport = new thrift.TBufferedTransport(undefined, function (
    buf,
  ) {
    chunks.push(Buffer.from(buf));
  });
  // The message envelope is what sets up TJSONProtocol's write stack, so it is
  // not optional here even though the struct is all the test cares about.
  const writeProtocol = new Protocol(writeTransport);
  writeProtocol.writeMessageBegin("m", thrift.Thrift.MessageType.CALL, 0);
  holder[Symbol.for("write")](writeProtocol);
  writeProtocol.writeMessageEnd();
  writeTransport.flush();

  let read = null;
  thrift.TBufferedTransport.receiver(function (transport) {
    const readProtocol = new Protocol(transport);
    readProtocol.readMessageBegin();
    read = new ttypes.MapKeyHolder();
    read[Symbol.for("read")](readProtocol);
    readProtocol.readMessageEnd();
  })(Buffer.concat(chunks));
  return read;
}

function ownKeys(map) {
  return Object.keys(map);
}

Object.keys(protocols).forEach(function (name) {
  const Protocol = protocols[name];

  test("a __proto__ map key stays a member (" + name + ")", function (assert) {
    const sent = new ttypes.MapKeyHolder();
    sent.structValues = withKey("__proto__", new ttypes.MapValue({ s: "v" }));
    sent.stringValues = withKey("__proto__", "plain");
    sent.listValues = withKey("__proto__", ["a"]);
    sent.mapValues = withKey("__proto__", { inner: "x" });

    const got = roundTrip(Protocol, sent);

    ["structValues", "stringValues", "listValues", "mapValues"].forEach(
      function (field) {
        const map = got[field];
        assert.deepEqual(
          ownKeys(map),
          ["__proto__"],
          field + ": the pair is an own member",
        );
        assert.equal(
          Object.getPrototypeOf(map),
          Object.prototype,
          field + ": the map keeps its prototype",
        );
      },
    );

    const structEntry = Object.getOwnPropertyDescriptor(
      got.structValues,
      "__proto__",
    );
    assert.equal(
      structEntry && structEntry.value && structEntry.value.s,
      "v",
      "the struct value survives the round trip",
    );
    const stringEntry = Object.getOwnPropertyDescriptor(
      got.stringValues,
      "__proto__",
    );
    assert.equal(
      stringEntry && stringEntry.value,
      "plain",
      "the string value survives the round trip",
    );
    assert.end();
  });

  test("ordinary map keys are unchanged (" + name + ")", function (assert) {
    const sent = new ttypes.MapKeyHolder();
    sent.structValues = { a: new ttypes.MapValue({ s: "v" }) };
    sent.stringValues = { b: "plain", constructor: "also fine" };
    sent.listValues = { c: ["a", "b"] };
    sent.mapValues = { d: { inner: "x" } };

    const got = roundTrip(Protocol, sent);

    assert.equal(got.structValues.a.s, "v", "struct value");
    assert.equal(got.stringValues.b, "plain", "string value");
    assert.equal(
      got.stringValues.constructor,
      "also fine",
      "a name that only looks special",
    );
    assert.deepEqual(got.listValues.c, ["a", "b"], "list value");
    assert.deepEqual(got.mapValues.d, { inner: "x" }, "map value");
    assert.equal(
      Object.getPrototypeOf(got.stringValues),
      Object.prototype,
      "prototype intact",
    );
    assert.end();
  });

  test("a decoded map can be sent on again (" + name + ")", function (assert) {
    const sent = new ttypes.MapKeyHolder();
    sent.structValues = withKey("__proto__", new ttypes.MapValue({ s: "v" }));
    sent.stringValues = { b: "plain" };
    sent.listValues = {};
    sent.mapValues = {};

    const once = roundTrip(Protocol, sent);
    const twice = roundTrip(Protocol, once);

    assert.deepEqual(
      ownKeys(twice.structValues),
      ["__proto__"],
      "the member survives a second round trip",
    );
    assert.equal(twice.stringValues.b, "plain", "and so does an ordinary one");
    assert.end();
  });
});
