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

"use strict";

const test = require("tape");
const tls = require("tls");
const { EventEmitter } = require("events");
const connection = require("thrift/lib/nodejs/lib/thrift/connection");
const server = require("thrift/lib/nodejs/lib/thrift/server");

// When the caller sets neither secureProtocol nor secureOptions, the SSL
// client must not pin a protocol of its own: leaving them unset lets Node
// apply its default minimum version rather than re-enabling older protocols.
test("createSSLConnection leaves an unset TLS floor to Node's default", function (assert) {
  const original = tls.connect;
  tls.connect = function () {
    return new EventEmitter();
  };
  try {
    const options = {};
    connection.createSSLConnection("localhost", 9090, options);

    assert.notEqual(
      options.secureProtocol,
      "SSLv23_method",
      "secureProtocol is not pinned to SSLv23_method",
    );
    assert.notOk(
      "secureProtocol" in options,
      "an unset secureProtocol is left unset so Node applies its default minVersion",
    );
    assert.notOk(
      "secureOptions" in options,
      "an unset secureOptions is left unset",
    );
  } finally {
    tls.connect = original;
  }
  assert.end();
});

// A secureProtocol the caller did set must be preserved exactly.
test("createSSLConnection preserves a caller's secureProtocol", function (assert) {
  const original = tls.connect;
  tls.connect = function () {
    return new EventEmitter();
  };
  try {
    const options = { secureProtocol: "TLSv1_2_method" };
    connection.createSSLConnection("localhost", 9090, options);

    assert.equal(
      options.secureProtocol,
      "TLSv1_2_method",
      "a caller-set secureProtocol is left untouched",
    );
  } finally {
    tls.connect = original;
  }
  assert.end();
});

// The server side must not pin an older protocol either.
test("createMultiplexServer leaves an unset TLS floor to Node's default", function (assert) {
  const original = tls.createServer;
  tls.createServer = function () {
    return new EventEmitter();
  };
  try {
    const options = { tls: {} };
    server.createMultiplexServer({ process: function () {} }, options);

    assert.notEqual(
      options.tls.secureProtocol,
      "SSLv23_method",
      "secureProtocol is not pinned to SSLv23_method",
    );
    assert.notOk(
      "secureProtocol" in options.tls,
      "an unset secureProtocol is left unset so Node applies its default minVersion",
    );
  } finally {
    tls.createServer = original;
  }
  assert.end();
});
