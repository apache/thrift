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

// TJSONProtocol.readMessageBegin scans the inbound buffer for the end of the
// message. A message can arrive in several pieces, with the read position
// rolled back after each piece, so the scan has to resume where it stopped
// rather than start again from the first byte; otherwise an N-piece message
// costs O(N^2) scanning. These cover that the resumed scan stays linear, that a
// message nested past the cap is refused before json_parse sees it, and that a
// message split into single bytes still reads back correctly.

const test = require("tape");
const TJSONProtocol = require("thrift/lib/nodejs/lib/thrift/json_protocol");
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");
const InputBufferUnderrunError = require("thrift/lib/nodejs/lib/thrift/input_buffer_underrun_error");
const Thrift = require("thrift/lib/nodejs/lib/thrift/thrift");

const DEPTH_LIMIT = Thrift.TProtocolExceptionType.DEPTH_LIMIT;

// Feed "fullBuf" to readMessageBegin in fixed-size pieces, exactly as server.js
// drives a buffered connection: on underrun roll the position back and wait for
// the next piece. Returns the last transport, carrying the result or the error.
function feed(fullBuf, chunk) {
  const outcome = { done: false, result: null, err: null };
  const onData = TBufferedTransport.receiver(function (twd) {
    const p = new TJSONProtocol(twd);
    try {
      outcome.result = p.readMessageBegin();
      outcome.done = true;
    } catch (e) {
      if (e instanceof InputBufferUnderrunError) {
        twd.rollbackPosition();
      } else {
        outcome.err = e;
      }
    }
  });
  for (
    let o = 0;
    o < fullBuf.length && !outcome.done && !outcome.err;
    o += chunk
  ) {
    onData(fullBuf.subarray(o, Math.min(o + chunk, fullBuf.length)));
  }
  return outcome;
}

// Like feed, but hands readMessageBegin a counting view of the inbound buffer so
// the test can see how many bytes the scan reads across all the pieces. The same
// real buffer must map to the same view (cached in a WeakMap): the saved scan
// state is matched by buffer identity, so a fresh view per call would make the
// scan start over every time and read O(N^2) even when fixed.
function feedCountingReads(fullBuf, chunk) {
  let bytesRead = 0;
  const views = new WeakMap();
  function viewFor(buf) {
    let view = views.get(buf);
    if (!view) {
      view = new Proxy(buf, {
        get(target, prop) {
          if (typeof prop === "string" && /^[0-9]+$/.test(prop)) {
            bytesRead += 1;
            return target[prop];
          }
          const value = target[prop];
          return typeof value === "function" ? value.bind(target) : value;
        },
      });
      views.set(buf, view);
    }
    return view;
  }

  // Take borrow from the prototype, not the instance: the same transport is
  // reused for every piece, so reading its (already wrapped) borrow each time
  // would nest the wrappers.
  const realBorrow = TBufferedTransport.prototype.borrow;
  let completed = false;
  const onData = TBufferedTransport.receiver(function (twd) {
    twd.borrow = function () {
      const b = realBorrow.call(twd);
      return {
        buf: viewFor(b.buf),
        readIndex: b.readIndex,
        writeIndex: b.writeIndex,
      };
    };
    const p = new TJSONProtocol(twd);
    try {
      p.readMessageBegin();
      completed = true;
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
  return { bytesRead: bytesRead, completed: completed };
}

test("a valid ping split into single bytes reads back", function (assert) {
  const outcome = feed(Buffer.from('[1,"ping",1,7,{}]'), 1);
  assert.ok(outcome.done && !outcome.err, "the message completed");
  assert.equal(outcome.result.fname, "ping", "method name");
  assert.equal(outcome.result.rseqid, 7, "sequence id");
  assert.end();
});

test("a backslash-escaped bracket inside a string spanning a boundary does not end the message", function (assert) {
  // The method name is a\]b. The ] is inside the string and must not be read as
  // the end of the message, even though the \ and the ] land in different
  // pieces when fed one byte at a time.
  const outcome = feed(Buffer.from('[1,"a\\\\]b",1,7,{}]'), 1);
  assert.ok(outcome.done && !outcome.err, "the message completed");
  assert.equal(
    outcome.result.fname,
    "a\\]b",
    "the escaped bracket stayed in the name",
  );
  assert.end();
});

test("a message nested to 64 levels is still accepted", function (assert) {
  const body = "[".repeat(64) + "]".repeat(64);
  const outcome = feed(
    Buffer.from('[1,"ping",1,7,{"1":{"rec":' + body + "}}]"),
    7,
  );
  assert.ok(
    outcome.done && !outcome.err,
    outcome.err ? outcome.err.message : "completed",
  );
  assert.end();
});

test("a message nested past the cap is refused before it is parsed", function (assert) {
  const body = "[".repeat(2000) + "]".repeat(2000);
  const outcome = feed(
    Buffer.from('[1,"ping",1,8,{"1":{"lst":' + body + "}}]"),
    64,
  );
  assert.notOk(outcome.done, "the message was not accepted");
  assert.ok(outcome.err, "an error was raised");
  assert.notOk(
    outcome.err instanceof InputBufferUnderrunError,
    "the error is not an underrun, so the server does not keep waiting for more",
  );
  assert.ok(
    outcome.err instanceof Thrift.TProtocolException,
    "the error is a TProtocolException",
  );
  assert.equal(
    outcome.err && outcome.err.type,
    DEPTH_LIMIT,
    "of type DEPTH_LIMIT",
  );
  assert.end();
});

test("the scan of an unfinished message stays linear in the number of pieces", function (assert) {
  // An open bracket followed by an unterminated string: the scan never finds
  // the end, so every piece triggers an underrun and a rollback, which is the
  // shape that used to rescan the whole prefix each time.
  const N = 4096;
  const unfinished = Buffer.concat([
    Buffer.from('["'),
    Buffer.alloc(N - 2, 0x61 /* a */),
  ]);
  const seen = feedCountingReads(unfinished, 1);

  assert.notOk(seen.completed, "the unfinished message never completed");
  assert.comment(
    "bytes scanned for " +
      unfinished.length +
      " bytes in 1-byte pieces: " +
      seen.bytesRead,
  );
  // Linear resumption reads each byte a small number of times (plus one rescan
  // per buffer growth). Rescanning the whole prefix on every piece would be
  // about N*N/2 ~ 8.4 million here, so a linear multiple of N is the two-state
  // line between fixed and unfixed.
  assert.ok(
    seen.bytesRead < 10 * unfinished.length,
    "bytes scanned (" +
      seen.bytesRead +
      ") is a small multiple of " +
      unfinished.length,
  );
  assert.end();
});
