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

// Buffer#readBigInt64BE / writeBigInt64BE were added in Node 12.0.0. The
// published package targets `engines: >= 10.18.0`, and browser-side Buffer
// polyfills may also lag, so we feature-detect and fall back to
// readInt32BE / writeUInt32BE plus BigInt arithmetic when the native
// methods are missing.

const HAS_NATIVE =
  typeof Buffer !== "undefined" &&
  typeof Buffer.prototype.readBigInt64BE === "function" &&
  typeof Buffer.prototype.writeBigInt64BE === "function";

function readBigInt64BEFallback(buf, offset) {
  // Signed high 32 bits, unsigned low 32 bits — recombined with a BigInt
  // shift+OR. Negative `hi` produces an arbitrarily-negative BigInt with
  // ones in all bits above 32, which OR-merges correctly with the
  // unsigned low half.
  const hi = BigInt(buf.readInt32BE(offset));
  const lo = BigInt(buf.readUInt32BE(offset + 4));
  return (hi << 32n) | lo;
}

function writeBigInt64BEFallback(buf, value, offset) {
  const v = BigInt.asIntN(64, value);
  const hi = Number(BigInt.asIntN(32, v >> 32n));
  const lo = Number(BigInt.asUintN(32, v));
  buf.writeInt32BE(hi, offset);
  buf.writeUInt32BE(lo, offset + 4);
}

exports.readBigInt64BE = HAS_NATIVE
  ? function (buf, offset) {
      return buf.readBigInt64BE(offset);
    }
  : readBigInt64BEFallback;

exports.writeBigInt64BE = HAS_NATIVE
  ? function (buf, value, offset) {
      // Native `writeBigInt64BE` rejects values outside [-2^63, 2^63 - 1];
      // wrap explicitly so callers (e.g. `fromBigInt`) can hand in any
      // bigint and get two's-complement truncation, matching the fallback.
      buf.writeBigInt64BE(BigInt.asIntN(64, value), offset);
    }
  : writeBigInt64BEFallback;

// Exposed so tests can exercise the fallback path on runtimes that have
// the native methods.
exports._readBigInt64BEFallback = readBigInt64BEFallback;
exports._writeBigInt64BEFallback = writeBigInt64BEFallback;
exports._hasNativeBigInt64 = HAS_NATIVE;
