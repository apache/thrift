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

// TJSONProtocol parses a whole message before any of it is read, so the size
// a list, set or map declares can be checked against the elements the message
// actually holds for it before the first element is read.

const test = require("tape");
const thrift = require("thrift");
const ttypes = require("./gen-nodejs/ThriftTest_types");

const Thrift = thrift.Thrift;
const TJSONProtocol = thrift.TJSONProtocol;
const INVALID_DATA = Thrift.TProtocolExceptionType.INVALID_DATA;
const NEGATIVE_SIZE = Thrift.TProtocolExceptionType.NEGATIVE_SIZE;

const WRITE = Symbol.for("write");
const READ = Symbol.for("read");

// More values than any message below holds. Reading more than that means the
// reader went on past the elements that are there.
const READ_BUDGET = 1000;

// Reads a struct of "Type" from the JSON message around "structJson".
function readStruct(Type, structJson) {
  return readMessage(Type, '[1,"m",1,0,' + structJson + "]");
}

// Reads the struct of "Type" in a JSON message and counts the values read
// from the message on the way.
function readMessage(Type, message) {
  const prot = new TJSONProtocol(
    new thrift.TFramedTransport(Buffer.from(message, "utf8")),
  );
  const readValue = prot.readValue;
  const result = { valuesRead: 0, err: null, obj: new Type() };
  prot.readValue = function () {
    result.valuesRead += 1;
    if (result.valuesRead > READ_BUDGET) {
      throw new Error("read " + result.valuesRead + " values");
    }
    return readValue.apply(this, arguments);
  };
  try {
    prot.readMessageBegin();
    result.obj[READ](prot);
  } catch (err) {
    result.err = err;
  }
  return result;
}

// Writes "obj" as the struct of a JSON message.
function writeMessage(obj) {
  let written;
  const out = new TJSONProtocol(
    new thrift.TBufferedTransport(undefined, function (buf) {
      written = buf;
    }),
  );
  out.writeMessageBegin("m", Thrift.MessageType.CALL, 1);
  obj[WRITE](out);
  out.writeMessageEnd();
  out.flush();
  return written.toString();
}

function assertRejected(assert, result, type, what) {
  assert.ok(
    result.err instanceof Thrift.TProtocolException,
    what + ": TProtocolException",
  );
  assert.equal(result.err && result.err.type, type, what + ": type " + type);
  assert.equal(result.valuesRead, 0, what + ": no element was read");
}

const cases = {
  "A container declaring more elements than it holds is rejected": function (
    assert,
  ) {
    const V2 = ttypes.VersioningTestV2;
    assertRejected(
      assert,
      readStruct(V2, '{"8":{"lst":["i32",2000000000,1,2,3]}}'),
      INVALID_DATA,
      "list",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"9":{"set":["i32",2000000000,1,2,3]}}'),
      INVALID_DATA,
      "set",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"10":{"map":["i32","i32",2000000000,{"1":2,"3":4}]}}'),
      INVALID_DATA,
      "map",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"10":{"map":["i32","i32",3]}}'),
      INVALID_DATA,
      "map without entries",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"8":{"lst":["i32",4,1,2,3]}}'),
      INVALID_DATA,
      "list one short",
    );
    assert.end();
  },

  "A container field that is skipped is checked the same way": function (
    assert,
  ) {
    // VersioningTestV1 has no field 8, so the reader skips it.
    assertRejected(
      assert,
      readStruct(
        ttypes.VersioningTestV1,
        '{"8":{"lst":["i32",2000000000,1,2,3]}}',
      ),
      INVALID_DATA,
      "skipped list",
    );
    assertRejected(
      assert,
      readStruct(
        ttypes.VersioningTestV1,
        '{"10":{"map":["i32","i32",2000000000,{"1":2}]}}',
      ),
      INVALID_DATA,
      "skipped map",
    );
    assert.end();
  },

  "A container size that is not an integer is rejected": function (assert) {
    const V2 = ttypes.VersioningTestV2;
    // Too large for a double to hold exactly, so the parser keeps the text.
    assertRejected(
      assert,
      readStruct(V2, '{"8":{"lst":["i32",9007199254740993,1]}}'),
      INVALID_DATA,
      "size beyond 2^53",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"8":{"lst":["i32","3",1,2,3]}}'),
      INVALID_DATA,
      "size as a string",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"8":{"lst":["i32",1.5,1,2]}}'),
      INVALID_DATA,
      "fractional size",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"10":{"map":["i32","i32",null,{"1":2}]}}'),
      INVALID_DATA,
      "map size null",
    );
    assert.end();
  },

  "A negative container size is rejected": function (assert) {
    const V2 = ttypes.VersioningTestV2;
    assertRejected(
      assert,
      readStruct(V2, '{"8":{"lst":["i32",-1,1]}}'),
      NEGATIVE_SIZE,
      "list",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"9":{"set":["i32",-1,1]}}'),
      NEGATIVE_SIZE,
      "set",
    );
    assertRejected(
      assert,
      readStruct(V2, '{"10":{"map":["i32","i32",-1,{}]}}'),
      NEGATIVE_SIZE,
      "map",
    );
    assert.end();
  },

  "Containers whose sizes match their elements still read back": function (
    assert,
  ) {
    const V2 = ttypes.VersioningTestV2;
    const result = readStruct(
      V2,
      '{"8":{"lst":["i32",3,1,2,3]},"9":{"set":["i32",2,4,5]},' +
        '"10":{"map":["i32","i32",2,{"6":7,"8":9}]},' +
        '"11":{"str":"after"}}',
    );
    assert.error(result.err, "no error");
    assert.deepEqual(result.obj.newlist, [1, 2, 3], "list");
    assert.deepEqual(result.obj.newset, [4, 5], "set");
    assert.deepEqual(result.obj.newmap, { 6: 7, 8: 9 }, "map");
    assert.equal(result.obj.newstring, "after", "field after the containers");

    const empty = readStruct(
      V2,
      '{"8":{"lst":["i32",0]},"9":{"set":["i32",0]},' +
        '"10":{"map":["i32","i32",0,{}]}}',
    );
    assert.error(empty.err, "no error for empty containers");
    assert.deepEqual(empty.obj.newlist, [], "empty list");
    assert.deepEqual(empty.obj.newset, [], "empty set");
    assert.deepEqual(empty.obj.newmap, {}, "empty map");
    assert.end();
  },

  "Lists of empty lists still read back": function (assert) {
    [
      [ttypes.NestedListsI32x2, [[], [], []]],
      [ttypes.NestedListsI32x2, [[1, 2], [], [], []]],
      [ttypes.NestedListsI32x3, [[], [], []]],
      [ttypes.NestedListsI32x3, [[[1]], [[]], [], []]],
    ].forEach(function ([Type, integerlist]) {
      const original = new Type({ integerlist: integerlist });
      const result = readMessage(Type, writeMessage(original));
      const what = JSON.stringify(integerlist);
      assert.error(result.err, what + ": no error");
      assert.deepEqual(result.obj, original, what + ": read back");
    });
    assert.end();
  },

  "Nested containers round-trip through TJSONProtocol": function (assert) {
    const original = new ttypes.NestedMixedx2({
      int_set_list: [[1, 2], [], [3]],
      map_int_strset: { 1: ["a", "b"], 2: [] },
      map_int_strset_list: [{ 1: ["c"] }, {}, { 2: ["d", "e"], 3: [] }],
    });
    const result = readMessage(ttypes.NestedMixedx2, writeMessage(original));
    assert.error(result.err, "no error");
    assert.deepEqual(result.obj, original, "read back what was written");
    assert.end();
  },
};

Object.keys(cases).forEach(function (caseName) {
  test(caseName, cases[caseName]);
});
