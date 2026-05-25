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

// Exercises `thrift.toBigInt` / `thrift.fromBigInt` — the JS-side helpers
// emitted by `--gen js:node,bigint` generated code to convert between node-int64
// `Int64` and native `bigint`. Verifies they round-trip through the binary
// protocol without any runtime toggle on `TBinaryProtocol`.

const test = require("tape");
const thrift = require("thrift");
const Int64 = require("node-int64");
const bigIntCompat = require("thrift/lib/nodejs/lib/thrift/bigint_compat");

function writeI64(value) {
  let buff;
  const transport = new thrift.TBufferedTransport(null, function (msg) {
    buff = msg;
  });
  const prot = new thrift.TBinaryProtocol(transport);
  prot.writeI64(value);
  prot.flush();
  return buff;
}

function readI64(serialized) {
  const trans = new thrift.TFramedTransport(serialized);
  const prot = new thrift.TBinaryProtocol(trans);
  return prot.readI64();
}

const I64_MAX = (1n << 63n) - 1n;
const I64_MIN = -(1n << 63n);

test("thrift.fromBigInt / thrift.toBigInt round-trip core values", function (assert) {
  for (const v of [0n, 1n, -1n, 42n, -42n, I64_MAX, I64_MIN]) {
    const i64 = thrift.fromBigInt(v);
    assert.ok(i64 instanceof Int64, `fromBigInt(${v}) returns Int64`);
    const back = thrift.toBigInt(i64);
    assert.equal(
      typeof back,
      "bigint",
      `toBigInt(...) returns bigint for ${v}`,
    );
    assert.equal(back, v, `round-trip ${v}`);
  }
  assert.end();
});

test("thrift.toBigInt honors a non-zero Int64 offset", function (assert) {
  // node-int64 supports `new Int64(buffer, offset)`.
  const buf = Buffer.from([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42]);
  const i64 = new Int64(buf, 8);
  assert.equal(thrift.toBigInt(i64), 42n, "offset is honored");
  assert.end();
});

test("thrift.fromBigInt wraps to the 64-bit signed range", function (assert) {
  // Anything outside [-2^63, 2^63 - 1] is wrapped via BigInt.asIntN(64, ...).
  const overflow = 1n << 63n; // 2^63 — wraps to -2^63
  const i64 = thrift.fromBigInt(overflow);
  assert.equal(thrift.toBigInt(i64), I64_MIN, "2^63 wraps to MIN_I64");
  assert.end();
});

test("thrift.fromBigInt coerces decimal strings and numbers", function (assert) {
  // Generated `map<i64, …>` serialization passes a JS object-key string
  // (`for (k in obj)` yields strings) into `fromBigInt`. Decimal strings
  // must round-trip — including values outside Number's safe range.
  assert.equal(thrift.toBigInt(thrift.fromBigInt("0")), 0n, "string 0");
  assert.equal(thrift.toBigInt(thrift.fromBigInt("42")), 42n, "string 42");
  assert.equal(thrift.toBigInt(thrift.fromBigInt("-42")), -42n, "string -42");
  assert.equal(
    thrift.toBigInt(thrift.fromBigInt("9223372036854775807")),
    I64_MAX,
    "string MAX_I64",
  );
  assert.equal(
    thrift.toBigInt(thrift.fromBigInt("-9223372036854775808")),
    I64_MIN,
    "string MIN_I64",
  );
  // Numbers (safe integer range only).
  assert.equal(thrift.toBigInt(thrift.fromBigInt(42)), 42n, "number 42");
  assert.equal(thrift.toBigInt(thrift.fromBigInt(-42)), -42n, "number -42");
  assert.equal(
    thrift.toBigInt(thrift.fromBigInt(Number.MAX_SAFE_INTEGER)),
    BigInt(Number.MAX_SAFE_INTEGER),
    "number MAX_SAFE_INTEGER",
  );
  assert.end();
});

test("thrift.fromBigInt rejects unsafe-integer numbers", function (assert) {
  // 2^53 + 1 cannot be represented exactly as a JS Number, so it has
  // already lost precision by the time it reaches `fromBigInt`. The
  // helper rejects it explicitly to avoid silent data corruption.
  assert.throws(
    () => thrift.fromBigInt(Number.MAX_SAFE_INTEGER + 1),
    /not a safe integer/,
    "Number.MAX_SAFE_INTEGER + 1 throws",
  );
  assert.throws(
    () => thrift.fromBigInt(Number.MIN_SAFE_INTEGER - 1),
    /not a safe integer/,
    "Number.MIN_SAFE_INTEGER - 1 throws",
  );
  assert.throws(
    () => thrift.fromBigInt(1.5),
    /not a safe integer/,
    "non-integer number throws",
  );
  assert.throws(
    () => thrift.fromBigInt(NaN),
    /not a safe integer/,
    "NaN throws",
  );
  // The decimal-string path still works for the same magnitudes.
  assert.equal(
    thrift.toBigInt(thrift.fromBigInt(String(Number.MAX_SAFE_INTEGER + 1))),
    BigInt(Number.MAX_SAFE_INTEGER) + 1n,
    "decimal-string above MAX_SAFE_INTEGER works",
  );
  assert.end();
});

test("bigint values round-trip through TBinaryProtocol via the helpers", function (assert) {
  for (const v of [0n, 1n, -1n, 42n, -42n, I64_MAX, I64_MIN]) {
    const wire = writeI64(thrift.fromBigInt(v));
    const result = thrift.toBigInt(readI64(wire));
    assert.equal(result, v, `protocol round-trip for ${v}`);
  }
  assert.end();
});

test("values beyond Number.MAX_SAFE_INTEGER survive the protocol round-trip", function (assert) {
  const big = (1n << 53n) + 1n;
  const back = thrift.toBigInt(readI64(writeI64(thrift.fromBigInt(big))));
  assert.equal(back, big, "above MAX_SAFE_INTEGER");

  const small = -((1n << 53n) + 1n);
  const backNeg = thrift.toBigInt(readI64(writeI64(thrift.fromBigInt(small))));
  assert.equal(backNeg, small, "below MIN_SAFE_INTEGER");
  assert.end();
});

// --- bigint_compat fallback ---------------------------------------------
//
// `toBigInt` / `fromBigInt` use `Buffer#readBigInt64BE` / `writeBigInt64BE`
// when available and fall back to a `readInt32BE` / `writeUInt32BE` + BigInt
// composition otherwise (needed for Node 10.x and older Buffer polyfills,
// since the native methods landed in Node 12). The fallback path is exposed
// for direct testing so it's exercised even on runtimes that have the
// natives.

test("readBigInt64BE fallback matches native across boundary values", function (assert) {
  const cases = [
    [0n, [0, 0, 0, 0, 0, 0, 0, 0]],
    [1n, [0, 0, 0, 0, 0, 0, 0, 1]],
    [-1n, [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]],
    [42n, [0, 0, 0, 0, 0, 0, 0, 42]],
    [-42n, [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xd6]],
    [I64_MAX, [0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]],
    [I64_MIN, [0x80, 0, 0, 0, 0, 0, 0, 0]],
  ];
  for (const [expected, bytes] of cases) {
    const buf = Buffer.from(bytes);
    assert.equal(
      bigIntCompat._readBigInt64BEFallback(buf, 0),
      expected,
      `read fallback for ${expected}`,
    );
  }
  assert.end();
});

test("writeBigInt64BE fallback matches native across boundary values", function (assert) {
  const cases = [
    [0n, [0, 0, 0, 0, 0, 0, 0, 0]],
    [1n, [0, 0, 0, 0, 0, 0, 0, 1]],
    [-1n, [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]],
    [42n, [0, 0, 0, 0, 0, 0, 0, 42]],
    [-42n, [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xd6]],
    [I64_MAX, [0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]],
    [I64_MIN, [0x80, 0, 0, 0, 0, 0, 0, 0]],
  ];
  for (const [value, expected] of cases) {
    const buf = Buffer.alloc(8);
    bigIntCompat._writeBigInt64BEFallback(buf, value, 0);
    assert.deepEqual(Array.from(buf), expected, `write fallback for ${value}`);
  }
  assert.end();
});

test("fallback round-trips at non-zero offsets", function (assert) {
  const buf = Buffer.alloc(16);
  bigIntCompat._writeBigInt64BEFallback(buf, I64_MAX, 4);
  assert.equal(bigIntCompat._readBigInt64BEFallback(buf, 4), I64_MAX);
  bigIntCompat._writeBigInt64BEFallback(buf, I64_MIN, 8);
  assert.equal(bigIntCompat._readBigInt64BEFallback(buf, 8), I64_MIN);
  assert.end();
});
