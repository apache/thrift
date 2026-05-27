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

// Round-trip regression test for the struct/union/exception read/write
// recursion-depth limit. Exercises the generated read/write code (not the
// protocol methods in isolation) over a recursive struct (RecTree), union
// (RecUnion) and exception (RecError) across every protocol that carries the
// limit.

const test = require("tape");
const thrift = require("thrift");
const THeaderTransport = require("thrift/lib/nodejs/lib/thrift/header_transport");
const ttypes = require("./gen-nodejs/JsRecursionDepthTest_types");

const RecTree = ttypes.RecTree;
const RecUnion = ttypes.RecUnion;
const RecError = ttypes.RecError;

const WRITE = Symbol.for("write");
const READ = Symbol.for("read");
const LIMIT = thrift.Thrift.DEFAULT_RECURSION_DEPTH; // 64
const DEPTH_LIMIT = thrift.Thrift.TProtocolExceptionType.DEPTH_LIMIT;

// Build a linear chain "depth" levels deep. Reading or writing it drives the
// generated code "depth" recursion levels deep. A union must have exactly one
// field set, so every node but the innermost one sets "children" and the
// innermost one sets its scalar leaf field.
function makeChain(Type, depth) {
  const leaf = new Type();
  leaf[Type === RecTree ? "item" : "leaf"] = 1;
  let node = leaf;
  for (let i = 1; i < depth; i++) {
    const parent = new Type();
    parent.children = [node];
    node = parent;
  }
  return node;
}

function chainDepth(node) {
  let d = 0;
  while (node) {
    d++;
    node = node.children && node.children.length ? node.children[0] : null;
  }
  return d;
}

function isJSON(ProtoCtor) {
  return ProtoCtor === thrift.TJSONProtocol;
}

// Serialize through the generated writer. When "unbounded" is set the depth
// guard is neutralised on this protocol instance only, so we can craft an
// over-limit payload to feed back into a fresh (bounded) reader.
function serialize(ProtoCtor, obj, unbounded) {
  let buff;
  const transport = new thrift.TBufferedTransport(null, function (msg) {
    buff = msg;
  });
  const proto = new ProtoCtor(transport);
  if (unbounded) {
    proto.incrementRecursionDepth = function () {};
    proto.decrementRecursionDepth = function () {};
  }
  if (isJSON(ProtoCtor)) {
    proto.writeMessageBegin("", 0, 0);
  }
  obj[WRITE](proto);
  if (isJSON(ProtoCtor)) {
    proto.writeMessageEnd();
  }
  proto.flush();
  return buff;
}

function deserialize(ProtoCtor, buff, Type) {
  const transport = new thrift.TFramedTransport(buff);
  const proto = new ProtoCtor(transport);
  if (isJSON(ProtoCtor)) {
    proto.readMessageBegin();
  }
  const obj = new Type();
  obj[READ](proto);
  if (isJSON(ProtoCtor)) {
    proto.readMessageEnd();
  }
  return obj;
}

function depthLimitError(fn) {
  try {
    fn();
  } catch (e) {
    return e;
  }
  return null;
}

const PROTOCOLS = {
  TBinaryProtocol: thrift.TBinaryProtocol,
  TCompactProtocol: thrift.TCompactProtocol,
  TJSONProtocol: thrift.TJSONProtocol,
};

const TYPES = { RecTree: RecTree, RecUnion: RecUnion, RecError: RecError };

Object.keys(PROTOCOLS).forEach(function (pname) {
  const ProtoCtor = PROTOCOLS[pname];

  Object.keys(TYPES).forEach(function (tname) {
    const Type = TYPES[tname];

    test(
      pname + "/" + tname + ": round-trips a chain at the depth limit",
      function (assert) {
        const original = makeChain(Type, LIMIT);
        const buff = serialize(ProtoCtor, original);
        const decoded = deserialize(ProtoCtor, buff, Type);
        assert.equal(
          chainDepth(decoded),
          LIMIT,
          "decoded chain has the same depth as the original",
        );
        assert.end();
      },
    );

    test(
      pname + "/" + tname + ": writing past the depth limit throws DEPTH_LIMIT",
      function (assert) {
        const tooDeep = makeChain(Type, LIMIT + 5);
        const err = depthLimitError(function () {
          serialize(ProtoCtor, tooDeep);
        });
        assert.ok(
          err instanceof thrift.Thrift.TProtocolException,
          "write throws TProtocolException",
        );
        assert.equal(err && err.type, DEPTH_LIMIT, "error type is DEPTH_LIMIT");
        assert.end();
      },
    );

    test(
      pname + "/" + tname + ": reading past the depth limit throws DEPTH_LIMIT",
      function (assert) {
        // Craft an over-limit payload with the guard neutralised, then read it
        // back through a normal, bounded reader.
        const tooDeep = makeChain(Type, LIMIT + 5);
        const buff = serialize(ProtoCtor, tooDeep, true);
        const err = depthLimitError(function () {
          deserialize(ProtoCtor, buff, Type);
        });
        assert.ok(
          err instanceof thrift.Thrift.TProtocolException,
          "read throws TProtocolException",
        );
        assert.equal(err && err.type, DEPTH_LIMIT, "error type is DEPTH_LIMIT");
        assert.end();
      },
    );
  });
});

// THeaderProtocol forwards the generated struct calls (including the recursion
// guard) to a wrapped TBinaryProtocol/TCompactProtocol. Without that
// delegation the generated code crashes with "is not a function", so exercise
// the generated writer straight through it.
function newHeaderProtocol() {
  const transport = new thrift.TFramedTransport();
  transport.setProtocolId(THeaderTransport.SubprotocolId.BINARY);
  return new thrift.THeaderProtocol(transport);
}

test("THeaderProtocol/RecTree: generated struct write within the limit works", function (assert) {
  const proto = newHeaderProtocol();
  const tree = makeChain(RecTree, LIMIT);
  assert.doesNotThrow(function () {
    tree[WRITE](proto);
  }, "generated write through THeaderProtocol does not crash");
  assert.end();
});

test("THeaderProtocol/RecTree: generated struct write past the limit throws DEPTH_LIMIT", function (assert) {
  const proto = newHeaderProtocol();
  const tooDeep = makeChain(RecTree, LIMIT + 5);
  const err = depthLimitError(function () {
    tooDeep[WRITE](proto);
  });
  assert.ok(
    err instanceof thrift.Thrift.TProtocolException,
    "write through THeaderProtocol throws TProtocolException",
  );
  assert.equal(err && err.type, DEPTH_LIMIT, "error type is DEPTH_LIMIT");
  assert.end();
});
