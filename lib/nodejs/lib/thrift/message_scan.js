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

var InputBufferUnderrunError = require("./input_buffer_underrun_error");

// Deepest container nesting the pre-scan stacks before it hands the message to
// the generated reader. The generated reader and skip() enforce the real
// per-message nesting limit (Thrift.DEFAULT_RECURSION_DEPTH); this bound only
// keeps the pre-scan's own stack small when a message is nested far past that
// limit, at which point the generated reader rejects it with DEPTH_LIMIT.
var MAX_SCAN_DEPTH = 1000;

// Walk one top-level struct -- a message's argument or result struct -- that
// begins at the transport's current read position, to learn whether the whole
// struct has arrived yet, without building any objects.
//
// server.js (and the connection.js TCP/StdIO client receivers) re-run the
// generated reader from the start of the accumulated buffer on every piece and
// roll the read position back while the struct is incomplete. Without this, a
// struct that arrives in P pieces is read P times -- the total work grows with
// the square of the number of pieces. The walk below resumes where the previous
// piece stopped: its state is kept on the transport and keyed by buffer
// identity, so it restarts only when the receiver grows the buffer (a geometric
// doubling) or commits a message (a fresh buffer), which keeps the restarts few
// and the total walk linear. The generated reader then runs once, when the
// struct is known to be complete.
//
// The walk only ever does one of two things:
//   - throws InputBufferUnderrunError, when a well-formed struct needs more
//     bytes than have arrived;
//   - returns, when the struct is fully present, OR when a token is not
//     well-formed (an unknown type, a negative size, an over-long varint, or
//     nesting past MAX_SCAN_DEPTH). In the latter case the generated reader runs
//     next and reports exactly the error it would have reported without the
//     pre-scan.
// It never returns before the struct's final byte, so the generated reader is
// never entered on an incomplete struct.
//
// `desc` describes the protocol's wire format with two pure functions:
//   desc.fieldHeader(buf, c, w) -> one of
//       { underrun: true }            not enough bytes for the field header
//       { yield: true }               the header is not well-formed
//       { stop: true, next }          struct terminator; next = cursor past it
//       { novalue: true, next }       field whose value is carried in the header
//       { type, next }                field of wire type `type`; next = cursor
//                                     past the header (type + id)
//   desc.value(buf, c, w, type, isField) -> one of
//       { underrun: true }            not enough bytes for the value / its header
//       { yield: true }               the value / its header is not well-formed
//       { next }                      a scalar or string fully present; next =
//                                     cursor past it
//       { next, push }                a container; next = cursor past its header,
//                                     push = the frame to descend into
// A frame is { t: 0 } for a struct, { t: 1, et, rem } for a list or set, or
// { t: 2, kt, vt, rem, phase } for a map (phase 0 = a key is next, 1 = a value).
function scanMessage(trans, desc) {
  var peek = trans.borrow();
  var buf = peek.buf;
  var writeIndex = peek.writeIndex;

  var state = trans._msgScan;
  if (
    !state ||
    state.buf !== buf ||
    state.readIndex !== peek.readIndex ||
    state.cursor > writeIndex
  ) {
    state = {
      buf: buf,
      readIndex: peek.readIndex,
      cursor: peek.readIndex,
      stack: [{ t: 0 }], // the message's argument / result struct
    };
  }

  var cursor = state.cursor;
  var stack = state.stack;

  while (stack.length > 0) {
    var frame = stack[stack.length - 1];
    var step;

    if (frame.t === 0) {
      var fh = desc.fieldHeader(buf, cursor, writeIndex);
      if (fh.underrun) {
        break;
      }
      if (fh.yield) {
        trans._msgScan = undefined;
        return;
      }
      if (fh.stop) {
        cursor = fh.next;
        stack.pop();
        continue;
      }
      if (fh.novalue) {
        cursor = fh.next;
        continue;
      }
      step = desc.value(buf, fh.next, writeIndex, fh.type, true);
    } else if (frame.t === 1) {
      if (frame.rem <= 0) {
        stack.pop();
        continue;
      }
      step = desc.value(buf, cursor, writeIndex, frame.et, false);
    } else {
      if (frame.rem <= 0) {
        stack.pop();
        continue;
      }
      step = desc.value(
        buf,
        cursor,
        writeIndex,
        frame.phase === 0 ? frame.kt : frame.vt,
        false,
      );
    }

    if (step.underrun) {
      break;
    }
    if (step.yield) {
      trans._msgScan = undefined;
      return;
    }
    if (step.push && stack.length >= MAX_SCAN_DEPTH) {
      trans._msgScan = undefined;
      return;
    }

    // The step is fully present: advance the cursor and account for the element
    // in one move, so a later underrun resumes exactly here and never twice.
    cursor = step.next;
    if (frame.t === 1) {
      frame.rem -= 1;
    } else if (frame.t === 2) {
      if (frame.phase === 0) {
        frame.phase = 1;
      } else {
        frame.phase = 0;
        frame.rem -= 1;
      }
    }
    if (step.push) {
      stack.push(step.push);
    }
  }

  if (stack.length === 0) {
    // The whole struct is present; let the generated reader read it.
    trans._msgScan = undefined;
    return;
  }

  // More bytes are needed. Remember where to resume and wait for the next piece.
  state.cursor = cursor;
  state.stack = stack;
  trans._msgScan = state;
  throw new InputBufferUnderrunError();
}

module.exports = scanMessage;
module.exports.MAX_SCAN_DEPTH = MAX_SCAN_DEPTH;
