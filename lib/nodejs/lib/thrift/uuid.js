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

const UUID_RE =
  /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

exports.parse = function (uuid) {
  if (typeof uuid !== "string") {
    throw new TypeError("Invalid UUID");
  }
  if (!UUID_RE.test(uuid)) {
    throw new TypeError("Invalid UUID");
  }
  const hex = uuid.replace(/-/g, "");
  const bytes = new Uint8Array(16);
  for (let i = 0; i < 16; i++) {
    bytes[i] = parseInt(hex.substr(i * 2, 2), 16);
  }
  return bytes;
};

const HEX = [];
for (let i = 0; i < 256; i++) {
  HEX.push((i + 0x100).toString(16).slice(1));
}

exports.stringify = function (bytes) {
  if (!bytes || bytes.length < 16) {
    throw new TypeError("Invalid UUID byte array");
  }
  return (
    HEX[bytes[0]] +
    HEX[bytes[1]] +
    HEX[bytes[2]] +
    HEX[bytes[3]] +
    "-" +
    HEX[bytes[4]] +
    HEX[bytes[5]] +
    "-" +
    HEX[bytes[6]] +
    HEX[bytes[7]] +
    "-" +
    HEX[bytes[8]] +
    HEX[bytes[9]] +
    "-" +
    HEX[bytes[10]] +
    HEX[bytes[11]] +
    HEX[bytes[12]] +
    HEX[bytes[13]] +
    HEX[bytes[14]] +
    HEX[bytes[15]]
  );
};
