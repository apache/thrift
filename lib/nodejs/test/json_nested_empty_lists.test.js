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
const thrift = require("thrift");

function encodeList(values, depth) {
  const elementType = depth === 1 ? "i32" : "lst";
  const elements = values.map((value) =>
    depth === 1 ? value : encodeList(value, depth - 1),
  );
  return [elementType, values.length, ...elements];
}

function readList(protocol, depth) {
  const { size } = protocol.readListBegin();
  const values = [];
  for (let index = 0; index < size; index += 1) {
    values.push(
      depth === 1 ? protocol.readI32() : readList(protocol, depth - 1),
    );
  }
  protocol.readListEnd();
  return values;
}

function createProtocol(wire) {
  return new thrift.TJSONProtocol(
    new thrift.TFramedTransport(Buffer.from(wire, "utf8")),
  );
}

function readField(wire, depth) {
  const protocol = createProtocol(wire);
  protocol.readMessageBegin();
  protocol.readStructBegin();
  const field = protocol.readFieldBegin();
  if (field.ftype !== thrift.Thrift.Type.LIST) {
    throw new Error("expected a list field");
  }
  const value = readList(protocol, depth);
  protocol.readFieldEnd();
  protocol.readStructEnd();
  return value;
}

function readMapField(wire, depth) {
  const protocol = createProtocol(wire);
  protocol.readMessageBegin();
  protocol.readStructBegin();
  const field = protocol.readFieldBegin();
  if (field.ftype !== thrift.Thrift.Type.MAP) {
    throw new Error("expected a map field");
  }
  const { size } = protocol.readMapBegin();
  const value = {};
  for (let index = 0; index < size; index += 1) {
    value[protocol.readI32()] = readList(protocol, depth);
  }
  protocol.readMapEnd();
  protocol.readStructEnd();
  return value;
}

test("TJSONProtocol keeps values after empty nested lists", function (assert) {
  const cases = [
    { depth: 2, value: [[], [], [1, 2]] },
    { depth: 2, value: [[1], [], [], [2]] },
    { depth: 3, value: [[[1, 2], []], [], [[3]]] },
  ];

  cases.forEach(({ depth, value }) => {
    const wire = JSON.stringify([
      1,
      "m",
      thrift.Thrift.MessageType.CALL,
      0,
      { 1: { lst: encodeList(value, depth) } },
    ]);
    assert.deepEqual(readField(wire, depth), value, JSON.stringify(value));
  });

  const mapWire = JSON.stringify([
    1,
    "m",
    thrift.Thrift.MessageType.CALL,
    0,
    {
      1: {
        map: [
          "i32",
          "lst",
          2,
          { 0: encodeList([[], []], 2), 1: encodeList([[7]], 2) },
        ],
      },
    },
  ]);
  assert.deepEqual(
    readMapField(mapWire, 2),
    { 0: [[], []], 1: [[7]] },
    "map values after empty nested lists",
  );
  assert.end();
});
