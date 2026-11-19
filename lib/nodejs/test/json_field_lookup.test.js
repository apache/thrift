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

// TJSONProtocol.readFieldBegin takes the next field of the struct on top of the
// read stack. It used to rebuild the struct's key list with Object.keys on
// every call and use only the first entry, so reading a struct of n fields
// enumerated n + (n-1) + ... + 1 keys -- quadratic in the field count the peer
// chose, on the server read path. The key list is now taken once per struct and
// walked with a cursor, so the work is linear. The two shapes are told apart by
// the number of keys Object.keys enumerates over the field loop, which is
// deterministic.

const test = require("tape");
const thrift = require("thrift");

const TJSONProtocol = thrift.TJSONProtocol;
const Type = thrift.Thrift.Type;

// Counts the keys Object.keys enumerates while reading a struct of N fields,
// each value {"i32":1} so the field loop does not stop early.
function enumeratedKeys(N) {
  const parts = [];
  for (let i = 1; i <= N; i++) {
    parts.push('"' + i + '":{"i32":1}');
  }
  const struct = JSON.parse("{" + parts.join(",") + "}");

  const p = new TJSONProtocol({});
  p.rstack = [struct];
  p.rpos = [];

  const orig = Object.keys;
  let enumerated = 0;
  Object.keys = function (o) {
    const r = orig(o);
    enumerated += r.length;
    return r;
  };
  let count = 0;
  try {
    while (true) {
      const f = p.readFieldBegin();
      if (f.ftype === Type.STOP) {
        break;
      }
      p.readFieldEnd();
      count++;
    }
  } finally {
    Object.keys = orig;
  }
  return { fields: count, enumerated: enumerated };
}

test("readFieldBegin reads a struct's fields in linear time", function (assert) {
  const r1000 = enumeratedKeys(1000);
  const r2000 = enumeratedKeys(2000);
  assert.equal(r1000.fields, 1000, "all 1000 fields are read");
  assert.equal(r2000.fields, 2000, "all 2000 fields are read");
  // The cursor enumerates about 2n keys (the struct once, plus one type key per
  // field); the old per-call rescan enumerated n(n+1)/2, i.e. 501500 for n=1000.
  assert.ok(
    r1000.enumerated <= 5 * 1000,
    "1000 fields enumerate O(n) keys (got " + r1000.enumerated + ")",
  );
  assert.ok(
    r2000.enumerated < 3 * r1000.enumerated,
    "enumeration scales linearly (" +
      r1000.enumerated +
      " -> " +
      r2000.enumerated +
      ")",
  );
  assert.end();
});

test("readFieldBegin yields every field once, in order, then STOP", function (assert) {
  const struct = JSON.parse('{"1":{"i32":7},"5":{"str":"hi"},"9":{"tf":1}}');
  const p = new TJSONProtocol({});
  p.rstack = [struct];
  p.rpos = [];
  const seq = [];
  while (true) {
    const f = p.readFieldBegin();
    if (f.ftype === Type.STOP) {
      seq.push("STOP");
      break;
    }
    seq.push(f.fid + ":" + f.ftype);
    p.readFieldEnd();
  }
  assert.deepEqual(
    seq,
    ["1:" + Type.I32, "5:" + Type.STRING, "9:" + Type.BOOL, "STOP"],
    "every field is handed back once, in order, then STOP",
  );
  assert.end();
});

// A map's entries arrive as one object, and readValue hands back its keys one
// at a time. The map's own keys are taken once per map and walked with a
// cursor, so the work is linear in the map's size. The entries object is
// wrapped in a Proxy whose ownKeys trap counts the keys each enumeration
// returns, which is deterministic.
function mapRead(N, keyOf) {
  const entries = {};
  for (let i = 0; i < N; i++) {
    entries[keyOf(i)] = i;
  }
  let enumerated = 0;
  const counted = new Proxy(entries, {
    ownKeys: function (target) {
      const keys = Reflect.ownKeys(target);
      enumerated += keys.length;
      return keys;
    },
  });
  const p = new TJSONProtocol({});
  p.rstack = [["str", "i32", N, counted]];
  p.rpos = [];
  const m = p.readMapBegin();
  const keys = [];
  const values = [];
  for (let i = 0; i < m.size; i++) {
    keys.push(p.readString());
    values.push(p.readI32());
  }
  p.readMapEnd();
  return { keys: keys, values: values, enumerated: enumerated };
}

test("readValue reads a map's keys in linear time", function (assert) {
  const keyOf = function (i) {
    return "k" + i;
  };
  const r1000 = mapRead(1000, keyOf);
  const r2000 = mapRead(2000, keyOf);
  assert.equal(r1000.keys.length, 1000, "all 1000 entries are read");
  assert.equal(r2000.keys.length, 2000, "all 2000 entries are read");
  // readMapBegin counts the entries once and the cursor takes the keys once:
  // about 2n keys in all.
  assert.ok(
    r1000.enumerated <= 3 * 1000,
    "1000 entries enumerate O(n) keys (got " + r1000.enumerated + ")",
  );
  assert.ok(
    r2000.enumerated < 3 * r1000.enumerated,
    "enumeration scales linearly (" +
      r1000.enumerated +
      " -> " +
      r2000.enumerated +
      ")",
  );
  assert.end();
});

test("readValue yields every map entry once, in order, with its value", function (assert) {
  const r = mapRead(5, function (i) {
    return "key" + i;
  });
  assert.deepEqual(
    r.keys,
    ["key0", "key1", "key2", "key3", "key4"],
    "every key once, in order",
  );
  assert.deepEqual(r.values, [0, 1, 2, 3, 4], "each key's value");

  // i64 keys arrive as decimal strings and read back as numbers
  const p = new TJSONProtocol({});
  p.rstack = [["i64", "str", 2, { 123456789012: "a", 2: "b" }]];
  p.rpos = [];
  const m = p.readMapBegin();
  const seen = [];
  for (let i = 0; i < m.size; i++) {
    seen.push(p.readI64().toString() + "=" + p.readString());
  }
  p.readMapEnd();
  assert.deepEqual(
    seen.sort(),
    ["123456789012=a", "2=b"],
    "i64 keys and their values",
  );
  assert.end();
});
