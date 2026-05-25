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

var wsConnection = require("./ws_connection");
exports.WSConnection = wsConnection.WSConnection;
exports.createWSConnection = wsConnection.createWSConnection;
exports.createWSClient = wsConnection.createWSClient;

var xhrConnection = require("./xhr_connection");
exports.XHRConnection = xhrConnection.XHRConnection;
exports.createXHRConnection = xhrConnection.createXHRConnection;
exports.createXHRClient = xhrConnection.createXHRClient;

var ohosConnection = require("./ohos_connection");
exports.OhosConnection = ohosConnection.OhosConnection;
exports.createOhosConnection = ohosConnection.createOhosConnection;
exports.createOhosClient = ohosConnection.createOhosClient;

exports.createClient = require("./create_client");

exports.Int64 = require("node-int64");

const bigIntCompat = require("./bigint_compat");

/**
 * Convert a `node-int64` Int64 to a native `bigint`. Used by code generated
 * with `--gen js:node,bigint`. Feature-detects `Buffer#readBigInt64BE` (Node 12+
 * and modern buffer polyfills) and falls back to a `readInt32BE` /
 * `readUInt32BE` + BigInt composition when the native method is absent.
 *
 * @param {Int64} i64
 * @returns {bigint}
 */
exports.toBigInt = function (i64) {
  return bigIntCompat.readBigInt64BE(i64.buffer, i64.offset || 0);
};

/**
 * Convert a native `bigint` to a `node-int64` Int64 for `writeI64`. Values
 * outside the signed 64-bit range are wrapped (`BigInt.asIntN(64, ...)`).
 * Also accepts a decimal-string or `number` so generated `map<i64, …>`
 * serialization works (map keys reach this function as object-key strings).
 * Uses the same feature-detected fallback as `toBigInt`.
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
    if (!Number.isSafeInteger(value)) {
      throw new RangeError(
        "thrift.fromBigInt: number " +
          value +
          " is not a safe integer — pass a bigint or decimal string for values beyond Number.MAX_SAFE_INTEGER",
      );
    }
    big = BigInt(value);
  } else {
    big = BigInt(value);
  }
  bigIntCompat.writeBigInt64BE(buf, big, 0);
  return new Int64(buf);
};

var mpxProtocol = require("./multiplexed_protocol");
exports.Multiplexer = mpxProtocol.Multiplexer;

/*
 * Export transport and protocol so they can be used outside of a
 * cassandra/server context
 */
exports.TBufferedTransport = require("./buffered_transport");
exports.TFramedTransport = require("./framed_transport");
exports.TWebSocketTransport = require("./ws_transport");

exports.Protocol = require("./json_protocol");
exports.TJSONProtocol = require("./json_protocol");
exports.TBinaryProtocol = require("./binary_protocol");
exports.TCompactProtocol = require("./compact_protocol");

exports.InputBufferUnderrunError = require("./input_buffer_underrun_error");
