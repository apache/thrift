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

const test = require("tape");
const childProcess = require("child_process");
const path = require("path");
const TBinaryProtocol = require("../lib/thrift/binary_protocol");
const TCompactProtocol = require("../lib/thrift/compact_protocol");

const UUID = "00112233-4455-4677-8899-aabbccddeeff";
const UUID_BYTES = [
  0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x46, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc,
  0xdd, 0xee, 0xff,
];

test("CommonJS runtime loads without require(esm)", function (assert) {
  const args = [];
  if (
    process.allowedNodeEnvironmentFlags &&
    process.allowedNodeEnvironmentFlags.has("--no-experimental-require-module")
  ) {
    args.push("--no-experimental-require-module");
  }
  args.push(
    "-e",
    [
      'const thrift = require("./lib/nodejs/lib/thrift");',
      'const uuid = require("uuid");',
      'if (!thrift || typeof uuid.v4 !== "function" || typeof uuid.v4() !== "string") process.exit(1);',
    ].join("\n"),
  );

  const result = childProcess.spawnSync(process.execPath, args, {
    cwd: path.resolve(__dirname, "../../.."),
    encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stderr || result.stdout);
  assert.end();
});

[
  ["binary", TBinaryProtocol],
  ["compact", TCompactProtocol],
].forEach(function ([name, Protocol]) {
  test(name + " protocol round-trips UUIDs", function (assert) {
    let encoded;
    const writer = new Protocol({
      write: function (value) {
        encoded = Buffer.from(value);
      },
    });
    writer.writeUuid(UUID.toUpperCase());

    assert.deepEqual(Array.from(encoded), UUID_BYTES);

    const reader = new Protocol({
      read: function (length) {
        assert.equal(length, 16);
        return encoded;
      },
    });
    assert.equal(reader.readUuid(), UUID);
    assert.end();
  });
});
