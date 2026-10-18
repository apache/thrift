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

// TJSONProtocol parses the whole message before any of it is read, so the size
// a list, set or map declares can be checked against the elements the message
// holds for it before the first element is read. Without that check, skip() and
// the generated container readers loop over the declared size with no upper
// bound, so a tiny message with a huge declared size runs the full count on the
// single JavaScript thread (the generated list reader also builds that many
// array slots). A container with no size at all still reads as empty.
//
// The browser library has no Node test runner of its own (its suite is QUnit
// driven through a headless browser), so this loads the real src/thrift.js into
// a vm context and drives the reader with declared sizes much larger than the
// elements present. A read budget counts scalar reads and aborts a runaway loop,
// so the two shapes are told apart without running billions of iterations.

const vm = require("vm");
const fs = require("fs");
const path = require("path");
const assert = require("assert");

const thriftPath = path.join(__dirname, "..", "src", "thrift.js");
const ctx = vm.createContext({ console });
vm.runInContext(fs.readFileSync(thriftPath, "utf8"), ctx, {
  filename: "thrift.js",
});

const BUDGET = 2000000;
const DECLARED = 2000000;

const results = vm.runInContext(
  `
(function (budget, declared) {
  var reads = 0;
  var proto = Thrift.Protocol.prototype;
  ['readI32','readI64','readString','readDouble','readBool','readByte','readI16','readBinary']
    .forEach(function (m) {
      var orig = proto[m];
      proto[m] = function () { if (++reads > budget) { throw new Error('OVER_BUDGET'); } return orig.apply(this, arguments); };
    });

  function proto1(wire) { var p = new Thrift.Protocol({}); p.rstack = [wire]; p.rpos = []; return p; }

  // Runs fn and classifies the outcome without leaking vm objects to the host.
  // A TProtocolException is identified by its numeric "type" (its "name" is the
  // base Error name here); "size" captures a value fn returns, for accept cases.
  function run(fn) {
    reads = 0;
    var out = { reads: 0, outcome: '', errType: null, size: null };
    try {
      var v = fn();
      out.outcome = 'completed';
      if (v !== undefined) { out.size = v; }
    } catch (e) {
      out.outcome = (e && e.message === 'OVER_BUDGET') ? 'over-budget' : 'threw';
      if (e && typeof e.type === 'number') { out.errType = e.type; }
      else if (e) { out.errType = (e.name || '') + ':' + (e.message || ''); }
    }
    out.reads = reads;
    return out;
  }

  var T = Thrift.Type;
  return {
    INVALID_DATA: Thrift.TProtocolExceptionType.INVALID_DATA,
    NEGATIVE_SIZE: Thrift.TProtocolExceptionType.NEGATIVE_SIZE,

    // --- reject: a declared size with no elements present, through skip() ---
    // skip() calls readListBegin/readMapBegin/readSetBegin, which now throw
    // before the skip loop can run and before any element is read.
    skipList: run(function () { proto1(['i32', declared]).skip(T.LIST, 0); }),
    skipSet:  run(function () { proto1(['i32', declared]).skip(T.SET, 0); }),
    skipMap:  run(function () { proto1(['i32', 'i32', declared, {}]).skip(T.MAP, 0); }),

    // --- reject: the generated list reader's shape (readListBegin + a loop) ---
    genList: run(function () {
      var p = proto1(['i32', declared]);
      var r = p.readListBegin();
      for (var i = 0; i < r.size; i++) { p.readI32(); }
      p.readListEnd();
    }),

    // --- reject: negative and non-integer sizes, through skip() ---
    negative:   run(function () { proto1(['i32', -1]).skip(T.LIST, 0); }),
    fractional: run(function () { proto1(['i32', 1.5, 1, 2]).skip(T.LIST, 0); }),

    // --- accept: valid, empty, and sizeless containers at readListBegin/
    // readMapBegin (the begin reader is where the size is accepted; the skip /
    // readListEnd round trip needs a surrounding struct and is covered by the
    // QUnit suite). Each returns the declared size the reader accepted. ---
    list3:     run(function () { return proto1(['i32', 3, 1, 2, 3]).readListBegin().size; }),
    map2:      run(function () { return proto1(['i32', 'i32', 2, {'1':2,'3':4}]).readMapBegin().size; }),
    emptyList: run(function () { return proto1(['i32', 0]).readListBegin().size; }),
    // No size at all: readListEnd can leave an emptied list behind, and that
    // reads as empty rather than being rejected.
    sizeless:  run(function () { proto1(['i32']).readListBegin(); return 'ok'; })
  };
})
`,
  ctx,
)(BUDGET, DECLARED);

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

function rejectedBefore(r, kind, expectedType) {
  assert.strictEqual(
    r.outcome,
    "threw",
    kind + " outcome: " + r.outcome + " (reads " + r.reads + ")",
  );
  assert.strictEqual(r.errType, expectedType, kind + " error type");
  assert.strictEqual(r.reads, 0, kind + " read no elements");
}

console.log(
  "# over-declared (" +
    DECLARED +
    "): skipList reads=" +
    results.skipList.reads +
    " skipSet reads=" +
    results.skipSet.reads +
    " skipMap reads=" +
    results.skipMap.reads +
    " genList reads=" +
    results.genList.reads,
);

check("skip() rejects an over-declared list before reading", () =>
  rejectedBefore(results.skipList, "list", results.INVALID_DATA),
);
check("skip() rejects an over-declared set before reading", () =>
  rejectedBefore(results.skipSet, "set", results.INVALID_DATA),
);
check("skip() rejects an over-declared map before reading", () =>
  rejectedBefore(results.skipMap, "map", results.INVALID_DATA),
);
check("the generated list reader is stopped before it builds the slots", () =>
  rejectedBefore(results.genList, "generated list", results.INVALID_DATA),
);
check("a negative container size is rejected", () =>
  rejectedBefore(results.negative, "negative", results.NEGATIVE_SIZE),
);
check("a non-integer container size is rejected", () =>
  rejectedBefore(results.fractional, "fractional", results.INVALID_DATA),
);

check("a list whose size matches its elements is accepted", () => {
  assert.strictEqual(
    results.list3.outcome,
    "completed",
    "outcome " + results.list3.outcome,
  );
  assert.strictEqual(results.list3.size, 3, "size 3 accepted");
});
check("a map whose size matches its entries is accepted", () => {
  assert.strictEqual(
    results.map2.outcome,
    "completed",
    "outcome " + results.map2.outcome,
  );
  assert.strictEqual(results.map2.size, 2, "size 2 accepted");
});
check("an empty container is accepted", () => {
  assert.strictEqual(results.emptyList.outcome, "completed");
  assert.strictEqual(results.emptyList.size, 0, "size 0 accepted");
});
check("a container with no declared size is still accepted", () =>
  assert.strictEqual(
    results.sizeless.outcome,
    "completed",
    "outcome " + results.sizeless.outcome,
  ),
);

if (failed > 0) {
  console.log("\n" + failed + " assertion(s) failed");
  process.exit(1);
}
console.log("\nall container-size assertions passed");
