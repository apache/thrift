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
exports.Thrift = require("./thrift");

var log = require("./log");
exports.setLogFunc = log.setLogFunc;
exports.setLogLevel = log.setLogLevel;
exports.getLogLevel = log.getLogLevel;

var connection = require("./connection");
exports.Connection = connection.Connection;
exports.createClient = connection.createClient;
exports.createConnection = connection.createConnection;
exports.createUDSConnection = connection.createUDSConnection;
exports.createSSLConnection = connection.createSSLConnection;
exports.createStdIOClient = connection.createStdIOClient;
exports.createStdIOConnection = connection.createStdIOConnection;

var httpConnection = require("./http_connection");
exports.HttpConnection = httpConnection.HttpConnection;
exports.createHttpConnection = httpConnection.createHttpConnection;
exports.createHttpUDSConnection = httpConnection.createHttpUDSConnection;
exports.createHttpClient = httpConnection.createHttpClient;

var wsConnection = require("./ws_connection");
exports.WSConnection = wsConnection.WSConnection;
exports.createWSConnection = wsConnection.createWSConnection;
exports.createWSClient = wsConnection.createWSClient;

var xhrConnection = require("./xhr_connection");
exports.XHRConnection = xhrConnection.XHRConnection;
exports.createXHRConnection = xhrConnection.createXHRConnection;
exports.createXHRClient = xhrConnection.createXHRClient;

var server = require("./server");
exports.createServer = server.createServer;
exports.createMultiplexServer = server.createMultiplexServer;

var web_server = require("./web_server");
exports.createWebServer = web_server.createWebServer;

exports.Int64 = require("node-int64");

const bigIntCompat = require("./bigint_compat");

/**
 * Convert a `node-int64` Int64 (as returned by `TBinaryProtocol.readI64`,
 * `TCompactProtocol.readI64`, etc.) to a native `bigint`. Used by code
 * generated with `--gen js:node,bigint` to surface int64 values as BigInt without
 * a protocol-layer toggle.
 *
 * Uses `Buffer#readBigInt64BE` when available (Node >= 12), and a
 * `readInt32BE` / `readUInt32BE` + BigInt fallback otherwise — so the
 * helpers work across the full `engines: >= 10.18.0` support range.
 *
 * @param {Int64} i64
 * @returns {bigint}
 */
exports.toBigInt = function (i64) {
  return bigIntCompat.readBigInt64BE(i64.buffer, i64.offset || 0);
};

/**
 * Convert a native `bigint` to a `node-int64` Int64 suitable for passing to
 * `writeI64`. Values outside the signed 64-bit range are wrapped to fit
 * (`BigInt.asIntN(64, ...)`).
 *
 * Also accepts a decimal-string or `number` for callers that don't hold the
 * value as a `bigint` — notably generated code serializing `map<i64, …>`,
 * where the map key is iterated as the JS object-key string (`for (k in obj)`).
 * Strings are parsed via `BigInt(string)` (decimal); numbers must be in the
 * safe integer range.
 *
 * Uses `Buffer#writeBigInt64BE` when available, with a 32-bit-pair fallback
 * for older runtimes (see `bigint_compat.js`).
 *
 * @param {bigint | string | number} value
 * @returns {Int64}
 */
exports.fromBigInt = function (value) {
  const Int64 = exports.Int64;
  const buf = Buffer.allocUnsafe(8);
  let big;
  if (typeof value === "bigint") {
    big = value;
  } else if (typeof value === "number") {
    // Reject silent rounding: a number outside the safe-integer range
    // has already lost precision before it reaches this function, so
    // converting it would produce a wrong i64 with no error. Callers
    // with > 2^53 values must pass a `bigint` or a decimal string.
    if (!Number.isSafeInteger(value)) {
      throw new RangeError(
        "thrift.fromBigInt: number " +
          value +
          " is not a safe integer — pass a bigint or decimal string for values beyond Number.MAX_SAFE_INTEGER",
      );
    }
    big = BigInt(value);
  } else {
    // String (decimal) or anything else — defer to BigInt(...) for parsing
    // and let it throw on garbage.
    big = BigInt(value);
  }
  bigIntCompat.writeBigInt64BE(buf, big, 0);
  return new Int64(buf);
};

var mpxProcessor = require("./multiplexed_processor");
var mpxProtocol = require("./multiplexed_protocol");
exports.MultiplexedProcessor = mpxProcessor.MultiplexedProcessor;
exports.Multiplexer = mpxProtocol.Multiplexer;

/*
 * Export transport and protocol so they can be used outside of a
 * cassandra/server context
 */
exports.TBufferedTransport = require("./buffered_transport");
exports.TFramedTransport = require("./framed_transport");

exports.TJSONProtocol = require("./json_protocol");
exports.TBinaryProtocol = require("./binary_protocol");
exports.TCompactProtocol = require("./compact_protocol");
exports.THeaderProtocol = require("./header_protocol");

exports.InputBufferUnderrunError = require("./input_buffer_underrun_error");
