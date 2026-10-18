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

/* jshint node: true, esversion: 8 */
"use strict";

// TJSONProtocol.readFieldBegin takes the next field of the struct on top of the
// read stack. It used to rebuild the struct's key list with Object.keys on
// every call and use only the first entry, so reading a struct of n fields
// enumerated n + (n-1) + ... + 1 keys -- quadratic in the field count the peer
// chose. The key list is now taken once per struct and walked with a cursor, so
// the work is linear. This test counts the keys Object.keys enumerates over the
// field loop: that number is deterministic and tells the two shapes apart (a
// wall-clock figure is in the dossier only).
//
// The browser library has no Node test runner of its own (its suite is QUnit
// driven through a headless browser), so this loads the real src/thrift.js into
// a vm context -- Object.keys below is the context's own -- and drives the
// reader's begin/end pair, exactly as the generated struct reader does.

const vm = require("vm");
const fs = require("fs");
const path = require("path");
const assert = require("assert");

const thriftPath = path.join(__dirname, "..", "src", "thrift.js");

function freshContext() {
  const ctx = vm.createContext({ console });
  vm.runInContext(fs.readFileSync(thriftPath, "utf8"), ctx, {
    filename: "thrift.js",
  });
  return ctx;
}

// Counts the keys Object.keys enumerates while reading a struct of N fields,
// each value {"i32":1} so the field loop does not stop early.
function enumeratedKeys(ctx, N) {
  const runner = vm.runInContext(
    `
    (function (N) {
      var parts = [];
      for (var i = 1; i <= N; i++) { parts.push('"' + i + '":{"i32":1}'); }
      var struct = JSON.parse('{' + parts.join(',') + '}');

      var orig = Object.keys;
      var enumerated = 0;
      Object.keys = function (o) { var r = orig(o); enumerated += r.length; return r; };

      var p = new Thrift.Protocol({});
      p.rstack = [struct];
      p.rpos = [];
      var count = 0;
      try {
        while (true) {
          var f = p.readFieldBegin();
          if (f.ftype === Thrift.Type.STOP) { break; }
          p.readFieldEnd();
          count++;
        }
      } finally {
        Object.keys = orig;
      }
      return { fields: count, enumerated: enumerated };
    })
    `,
    ctx,
  );
  return runner(N);
}

// Reads a struct of mixed field types and returns the fid/ftype pairs the reader
// yields, in order, to confirm the cursor still hands back every field once.
function readSequence(ctx) {
  return vm.runInContext(
    `
    (function () {
      var struct = JSON.parse('{"1":{"i32":7},"5":{"str":"hi"},"9":{"tf":1}}');
      var p = new Thrift.Protocol({});
      p.rstack = [struct];
      p.rpos = [];
      var seq = [];
      while (true) {
        var f = p.readFieldBegin();
        if (f.ftype === Thrift.Type.STOP) { seq.push('STOP'); break; }
        seq.push(f.fid + ':' + f.ftype);
        p.readFieldEnd();
      }
      return { seq: seq, I32: Thrift.Type.I32, STRING: Thrift.Type.STRING, BOOL: Thrift.Type.BOOL };
    })()
    `,
    ctx,
  );
}

let failed = 0;
function check(name, fn) {
  try {
    fn();
    console.log("ok   - " + name);
  } catch (e) {
    failed += 1;
    console.log("FAIL - " + name + ": " + e.message);
  }
}

const r1000 = enumeratedKeys(freshContext(), 1000);
const r2000 = enumeratedKeys(freshContext(), 2000);
console.log(
  "# N=1000 fields=" +
    r1000.fields +
    " keysEnumerated=" +
    r1000.enumerated +
    "  | N=2000 fields=" +
    r2000.fields +
    " keysEnumerated=" +
    r2000.enumerated +
    "  (quadratic would be 501500 / 2003000)",
);

// Linear: reading n fields enumerates O(n) keys, not O(n^2). The cursor shape
// enumerates about 2n (the struct once, plus one type key per field); the old
// rescan enumerated n(n+1)/2. 5*n leaves generous room for the linear shape and
// is far below the quadratic count.
check("reading 1000 fields enumerates O(n) keys", () =>
  assert.ok(
    r1000.enumerated <= 5 * 1000,
    "enumerated " + r1000.enumerated + " keys for 1000 fields",
  ),
);
check("key enumeration scales linearly, not quadratically", () =>
  assert.ok(
    r2000.enumerated < 3 * r1000.enumerated,
    "2000-field count " +
      r2000.enumerated +
      " vs 1000-field count " +
      r1000.enumerated,
  ),
);
check("all declared fields are read", () => {
  assert.strictEqual(r1000.fields, 1000);
  assert.strictEqual(r2000.fields, 2000);
});

const seqResult = readSequence(freshContext());
check("the cursor yields every field once, in order, then STOP", () => {
  // Join to a primitive string: the array comes from the vm realm, so a
  // deep-equal against a host array would fail on prototype identity alone.
  const expected = [
    "1:" + seqResult.I32,
    "5:" + seqResult.STRING,
    "9:" + seqResult.BOOL,
    "STOP",
  ].join("|");
  assert.strictEqual(seqResult.seq.join("|"), expected);
});

if (failed > 0) {
  console.log("\n" + failed + " assertion(s) failed");
  process.exit(1);
}
console.log("\nall readFieldBegin field-lookup assertions passed");
