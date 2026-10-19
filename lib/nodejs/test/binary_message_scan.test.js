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

// A TBinaryProtocol / TCompactProtocol message has no length prefix, so a
// buffered server learns where it ends only by reading it. server.js reads it
// from the start of the accumulated buffer on every TCP piece and rolls the read
// position back while it is incomplete, which used to re-decode the whole prefix
// on every piece -- work growing with the square of the number of pieces for a
// message of many small elements. readMessageBegin now walks the argument struct
// once, resuming across pieces, and only lets the generated reader decode it once
// it is complete. These cover (1) that the resumed walk agrees with a full decode
// for a wide range of messages fed one byte at a time, (2) that the per-piece work
// is now linear, and (3) that valid multi-piece and pipelined messages still read
// back.

const test = require("tape");
const TBinaryProtocol = require("thrift/lib/nodejs/lib/thrift/binary_protocol");
const TCompactProtocol = require("thrift/lib/nodejs/lib/thrift/compact_protocol");
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");
const InputBufferUnderrunError = require("thrift/lib/nodejs/lib/thrift/input_buffer_underrun_error");
const Thrift = require("thrift/lib/nodejs/lib/thrift/thrift");

const Type = Thrift.Type;
const DEPTH_LIMIT = Thrift.TProtocolExceptionType.DEPTH_LIMIT;

// Build the on-the-wire bytes of one CALL message whose argument struct is
// produced by writeArgs(p).
function buildMessage(Protocol, name, writeArgs) {
  let bytes = null;
  const out = new TBufferedTransport(undefined, function (buf) {
    bytes = buf;
  });
  const p = new Protocol(out);
  p.writeMessageBegin(name, Thrift.MessageType.CALL, 1);
  p.writeStructBegin("args");
  writeArgs(p);
  p.writeFieldStop();
  p.writeStructEnd();
  p.writeMessageEnd();
  p.flush();
  return bytes;
}

// Drive readMessageBegin + a full read of the argument struct exactly as
// server.js drives a buffered connection: read messages until an underrun, then
// roll back and wait for the next piece. Returns every message that completed.
function feed(fullBuf, chunk, Protocol) {
  const results = [];
  let err = null;
  const onData = TBufferedTransport.receiver(function (twd) {
    const p = new Protocol(twd);
    try {
      while (true) {
        const begin = p.readMessageBegin();
        p.skip(Type.STRUCT);
        p.readMessageEnd();
        twd.commitPosition();
        results.push(begin);
      }
    } catch (e) {
      if (e instanceof InputBufferUnderrunError) {
        twd.rollbackPosition();
      } else {
        err = e;
      }
    }
  });
  for (let o = 0; o < fullBuf.length && !err; o += chunk) {
    onData(fullBuf.subarray(o, Math.min(o + chunk, fullBuf.length)));
  }
  return { results: results, err: err };
}

// Like feed, but tallies every inbound byte the walk and the generated reader
// examine, across all the pieces, so the test can see the work without timing
// anything. The walk reads through borrow(); the generated reader reads through
// the transport's read primitives. Both are counted into the same total.
function feedWork(fullBuf, chunk, Protocol) {
  let work = 0;
  const views = new WeakMap();
  function viewFor(buf) {
    let v = views.get(buf);
    if (!v) {
      v = new Proxy(buf, {
        get: function (target, prop) {
          if (typeof prop === "string" && /^[0-9]+$/.test(prop)) {
            work += 1;
            return target[prop];
          }
          const value = target[prop];
          return typeof value === "function" ? value.bind(target) : value;
        },
      });
      views.set(buf, v);
    }
    return v;
  }
  const realBorrow = TBufferedTransport.prototype.borrow;
  function wrap(twd, name, width) {
    const real = TBufferedTransport.prototype[name];
    twd[name] = function (arg) {
      work += width === null ? arg : width;
      return real.call(this, arg);
    };
  }
  let completed = 0;
  const onData = TBufferedTransport.receiver(function (twd) {
    twd.borrow = function () {
      const b = realBorrow.call(twd);
      return {
        buf: viewFor(b.buf),
        readIndex: b.readIndex,
        writeIndex: b.writeIndex,
      };
    };
    wrap(twd, "readByte", 1);
    wrap(twd, "readI16", 2);
    wrap(twd, "readI32", 4);
    wrap(twd, "readDouble", 8);
    wrap(twd, "read", null);
    wrap(twd, "readString", null);
    const p = new Protocol(twd);
    try {
      while (true) {
        p.readMessageBegin();
        p.skip(Type.STRUCT);
        p.readMessageEnd();
        twd.commitPosition();
        completed += 1;
      }
    } catch (e) {
      if (e instanceof InputBufferUnderrunError) {
        twd.rollbackPosition();
      } else {
        throw e;
      }
    }
  });
  for (let o = 0; o < fullBuf.length; o += chunk) {
    onData(fullBuf.subarray(o, Math.min(o + chunk, fullBuf.length)));
  }
  return { work: work, completed: completed };
}

// One CALL message whose single argument is a list<byte> that declares far more
// elements than are sent, so it never completes -- the shape that re-decoded the
// whole prefix on every piece.
function unfinishedListMessage(Protocol, totalBytes) {
  let head = null;
  const out = new TBufferedTransport(undefined, function (buf) {
    head = buf;
  });
  const p = new Protocol(out);
  p.writeMessageBegin("ping", Thrift.MessageType.CALL, 1);
  p.writeStructBegin("args");
  p.writeFieldBegin("l", Type.LIST, 1);
  p.writeListBegin(Type.BYTE, 1 << 30);
  p.flush();
  const b = Buffer.alloc(totalBytes, 0x01);
  head.copy(b, 0, 0, Math.min(head.length, totalBytes));
  return b;
}

// One CALL message whose argument struct is a long run of one-byte fields with no
// field-stop, so it never completes. Unlike the list above it declares no element
// count, so a count-based shortcut would not help it -- only a true structural walk
// keeps it linear.
function unfinishedStructMessage(Protocol, totalBytes) {
  let head = null;
  const out = new TBufferedTransport(undefined, function (buf) {
    head = buf;
  });
  const p = new Protocol(out);
  p.writeMessageBegin("ping", Thrift.MessageType.CALL, 1);
  p.writeStructBegin("args");
  for (let i = 1; i <= totalBytes; i++) {
    p.writeFieldBegin("f", Type.BYTE, i);
    p.writeByte(i & 0xff);
    p.writeFieldEnd();
  }
  p.flush();
  const b = Buffer.alloc(totalBytes);
  head.copy(b, 0, 0, Math.min(head.length, totalBytes));
  return b;
}

// The equivalence corpus: varied argument structs, each exercising a different
// corner of the two wire formats. Each writer takes the protocol to write to.
function corpus() {
  return [
    ["empty args", function () {}],
    [
      "a bool true field",
      function (p) {
        p.writeFieldBegin("b", Type.BOOL, 1);
        p.writeBool(true);
        p.writeFieldEnd();
      },
    ],
    [
      "a bool false field",
      function (p) {
        p.writeFieldBegin("b", Type.BOOL, 1);
        p.writeBool(false);
        p.writeFieldEnd();
      },
    ],
    [
      "scalar fields",
      function (p) {
        p.writeFieldBegin("by", Type.BYTE, 1);
        p.writeByte(-5);
        p.writeFieldEnd();
        p.writeFieldBegin("i16", Type.I16, 2);
        p.writeI16(-12345);
        p.writeFieldEnd();
        p.writeFieldBegin("i32", Type.I32, 3);
        p.writeI32(-1234567);
        p.writeFieldEnd();
        p.writeFieldBegin("i64", Type.I64, 4);
        p.writeI64(9007199254740000);
        p.writeFieldEnd();
        p.writeFieldBegin("d", Type.DOUBLE, 5);
        p.writeDouble(3.14159);
        p.writeFieldEnd();
      },
    ],
    [
      "a string and a binary field",
      function (p) {
        p.writeFieldBegin("s", Type.STRING, 1);
        p.writeString("hello, scan");
        p.writeFieldEnd();
        p.writeFieldBegin("bin", Type.STRING, 2);
        p.writeBinary(Buffer.from([0, 1, 2, 250, 255]));
        p.writeFieldEnd();
      },
    ],
    [
      "an empty string",
      function (p) {
        p.writeFieldBegin("s", Type.STRING, 1);
        p.writeString("");
        p.writeFieldEnd();
      },
    ],
    [
      "a uuid field",
      function (p) {
        p.writeFieldBegin("u", Type.UUID, 1);
        p.writeUuid("00112233-4455-6677-8899-aabbccddeeff");
        p.writeFieldEnd();
      },
    ],
    [
      "a list of bytes",
      function (p) {
        p.writeFieldBegin("l", Type.LIST, 1);
        p.writeListBegin(Type.BYTE, 20);
        for (let i = 0; i < 20; i++) p.writeByte(i);
        p.writeListEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a list of bools (> 14 elements forces the long size form)",
      function (p) {
        p.writeFieldBegin("l", Type.LIST, 1);
        p.writeListBegin(Type.BOOL, 17);
        for (let i = 0; i < 17; i++) p.writeBool(i % 2 === 0);
        p.writeListEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a set of i16",
      function (p) {
        p.writeFieldBegin("s", Type.SET, 1);
        p.writeSetBegin(Type.I16, 3);
        p.writeI16(100);
        p.writeI16(200);
        p.writeI16(300);
        p.writeSetEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a list of strings",
      function (p) {
        p.writeFieldBegin("l", Type.LIST, 1);
        p.writeListBegin(Type.STRING, 3);
        p.writeString("a");
        p.writeString("bb");
        p.writeString("ccc");
        p.writeListEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a nested list of lists",
      function (p) {
        p.writeFieldBegin("l", Type.LIST, 1);
        p.writeListBegin(Type.LIST, 2);
        p.writeListBegin(Type.I32, 2);
        p.writeI32(1);
        p.writeI32(2);
        p.writeListEnd();
        p.writeListBegin(Type.I32, 1);
        p.writeI32(3);
        p.writeListEnd();
        p.writeListEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "an empty map",
      function (p) {
        p.writeFieldBegin("m", Type.MAP, 1);
        p.writeMapBegin(Type.STRING, Type.I32, 0);
        p.writeMapEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a map of string to i32",
      function (p) {
        p.writeFieldBegin("m", Type.MAP, 1);
        p.writeMapBegin(Type.STRING, Type.I32, 2);
        p.writeString("one");
        p.writeI32(1);
        p.writeString("two");
        p.writeI32(2);
        p.writeMapEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a map of i32 to struct",
      function (p) {
        p.writeFieldBegin("m", Type.MAP, 1);
        p.writeMapBegin(Type.I32, Type.STRUCT, 1);
        p.writeI32(7);
        p.writeStructBegin("inner");
        p.writeFieldBegin("x", Type.I32, 1);
        p.writeI32(42);
        p.writeFieldEnd();
        p.writeFieldStop();
        p.writeStructEnd();
        p.writeMapEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a map of bool to bool",
      function (p) {
        p.writeFieldBegin("m", Type.MAP, 1);
        p.writeMapBegin(Type.BOOL, Type.BOOL, 2);
        p.writeBool(true);
        p.writeBool(false);
        p.writeBool(false);
        p.writeBool(true);
        p.writeMapEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a nested struct",
      function (p) {
        p.writeFieldBegin("inner", Type.STRUCT, 1);
        p.writeStructBegin("inner");
        p.writeFieldBegin("y", Type.STRING, 1);
        p.writeString("deep");
        p.writeFieldEnd();
        p.writeFieldStop();
        p.writeStructEnd();
        p.writeFieldEnd();
      },
    ],
    [
      "a field with a large id (delta cannot encode it)",
      function (p) {
        p.writeFieldBegin("x", Type.I32, 20000);
        p.writeI32(123);
        p.writeFieldEnd();
      },
    ],
  ];
}

[
  { name: "binary", Protocol: TBinaryProtocol },
  { name: "compact", Protocol: TCompactProtocol },
].forEach(function (proto) {
  const Protocol = proto.Protocol;

  test(
    proto.name +
      ": every corpus message fed one byte at a time agrees with a single-shot read",
    function (assert) {
      corpus().forEach(function (entry) {
        const label = entry[0];
        const bytes = buildMessage(Protocol, "ping", entry[1]);

        // One shot: the baseline. No rollback, so the walk is not even engaged.
        const whole = feed(bytes, bytes.length, Protocol);
        assert.ok(
          !whole.err && whole.results.length === 1,
          label + ": reads back in one piece",
        );

        // One byte at a time: the walk is engaged and must resume across pieces
        // and complete at exactly the same point.
        const byByte = feed(bytes, 1, Protocol);
        assert.ok(
          !byByte.err && byByte.results.length === 1,
          label +
            ": reads back one byte at a time" +
            (byByte.err ? " (" + byByte.err.message + ")" : ""),
        );
        assert.equal(
          byByte.results.length && byByte.results[0].fname,
          "ping",
          label + ": method name preserved",
        );

        // Missing the final byte (the argument struct's stop) must stay
        // incomplete -- the walk must not report completion before the last byte.
        if (bytes.length > 1) {
          const short = feed(bytes.subarray(0, bytes.length - 1), 1, Protocol);
          assert.ok(
            !short.err && short.results.length === 0,
            label + ": one byte short is still incomplete",
          );
        }
      });
      assert.end();
    },
  );

  test(
    proto.name +
      ": two messages pipelined in one buffer both read back when fed in pieces",
    function (assert) {
      const m1 = buildMessage(Protocol, "ping", function (p) {
        p.writeFieldBegin("s", Type.STRING, 1);
        p.writeString("first");
        p.writeFieldEnd();
      });
      const m2 = buildMessage(Protocol, "pong", function (p) {
        p.writeFieldBegin("i", Type.I32, 1);
        p.writeI32(2);
        p.writeFieldEnd();
      });
      const out = feed(Buffer.concat([m1, m2]), 3, Protocol);
      assert.ok(
        !out.err,
        "no error" + (out.err ? " (" + out.err.message + ")" : ""),
      );
      assert.equal(out.results.length, 2, "both messages read back");
      assert.equal(out.results[0].fname, "ping", "first method name");
      assert.equal(out.results[1].fname, "pong", "second method name");
      assert.end();
    },
  );

  test(
    proto.name +
      ": the work to scan an unfinished message stays linear in the number of pieces",
    function (assert) {
      const N = 4096;
      const unfinished = unfinishedListMessage(Protocol, N);
      const seen = feedWork(unfinished, 1, Protocol);
      assert.equal(seen.completed, 0, "the unfinished message never completed");
      assert.comment(
        "inbound bytes examined for " +
          unfinished.length +
          " bytes in 1-byte pieces: " +
          seen.work,
      );
      // Re-decoding the whole prefix on every piece is about N*N/2 ~ 8.4 million
      // here. A resumed walk reads each byte a small number of times, so a small
      // multiple of N is the line between the two.
      assert.ok(
        seen.work < 20 * unfinished.length,
        "bytes examined (" +
          seen.work +
          ") is a small multiple of " +
          unfinished.length,
      );
      assert.end();
    },
  );

  test(
    proto.name +
      ": the work to scan an unfinished struct of many fields also stays linear",
    function (assert) {
      const N = 4096;
      const unfinished = unfinishedStructMessage(Protocol, N);
      const seen = feedWork(unfinished, 1, Protocol);
      assert.equal(seen.completed, 0, "the unfinished struct never completed");
      assert.comment(
        "inbound bytes examined for a " +
          unfinished.length +
          "-byte field run in 1-byte pieces: " +
          seen.work,
      );
      assert.ok(
        seen.work < 20 * unfinished.length,
        "bytes examined (" +
          seen.work +
          ") is a small multiple of " +
          unfinished.length,
      );
      assert.end();
    },
  );

  test(
    proto.name +
      ": a message nested past the reader's limit is refused, not left waiting",
    function (assert) {
      // 200 nested lists is well past DEFAULT_RECURSION_DEPTH (64). The walk hands
      // it to the generated reader, which refuses it with a DEPTH_LIMIT error --
      // not an underrun -- so the server does not keep waiting for more.
      const bytes = buildMessage(Protocol, "ping", function (p) {
        p.writeFieldBegin("l", Type.LIST, 1);
        for (let i = 0; i < 199; i++) {
          p.writeListBegin(Type.LIST, 1);
        }
        p.writeListBegin(Type.BYTE, 0);
        p.writeFieldEnd();
      });
      const out = feed(bytes, 7, Protocol);
      assert.equal(out.results.length, 0, "the message was not accepted");
      assert.ok(out.err, "an error was raised");
      assert.notOk(
        out.err instanceof InputBufferUnderrunError,
        "the error is not an underrun",
      );
      assert.equal(
        out.err && out.err.type,
        DEPTH_LIMIT,
        "the error is a DEPTH_LIMIT TProtocolException",
      );
      assert.end();
    },
  );
});
