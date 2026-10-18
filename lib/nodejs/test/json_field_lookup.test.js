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
