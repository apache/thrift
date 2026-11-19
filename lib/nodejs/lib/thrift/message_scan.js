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
var Thrift = require("./thrift");

// Deepest container nesting the pre-scan stacks before it hands the message to
// the generated reader. The generated reader and skip() enforce the real
// per-message nesting limit (Thrift.DEFAULT_RECURSION_DEPTH); this bound only
// keeps the pre-scan's own stack small when a message is nested far past that
// limit, at which point the generated reader rejects it with DEPTH_LIMIT.
var MAX_SCAN_DEPTH = 1000;

// Walk one top-level struct -- a message's argument or result struct -- to
// learn whether the whole struct has arrived yet, without building any objects.
//
// server.js (and the connection.js TCP/StdIO client receivers) re-run the
// generated reader from the start of the accumulated buffer on every piece and
// roll the read position back while the struct is incomplete. Without this, a
// struct that arrives in P pieces is read P times -- the total work grows with
// the square of the number of pieces. The walk resumes where the previous piece
// stopped: its state is kept on the transport, keyed by buffer identity and by
// the position the message starts at. readMessageBegin calls resume() before it
// reads the message header, so a piece that continues a walk goes straight back
// to it, and calls scanMessage() after reading the header when there is no walk
// to continue. The state restarts only when the receiver grows the buffer (a
// geometric doubling) or commits a message (a fresh buffer), which keeps the
// restarts few and the total work linear. The generated reader then runs once,
// when the struct is known to be complete.
//
// The walk only ever does one of two things:
//   - throws InputBufferUnderrunError, when a well-formed struct needs more
//     bytes than have arrived;
//   - returns, when the struct is fully present, OR when a token is not
//     well-formed (an unknown type, a negative size, an over-long varint, or
//     nesting past MAX_SCAN_DEPTH). In the latter case the generated reader runs
//     next and reports exactly the error it would have reported without the
//     pre-scan.
// It does not return before the struct's final byte as the struct's own field
// and element types lay it out. The generated reader reads the declared types,
// so it can still run out of bytes when the two differ -- for example a list
// whose element type is not the declared one. When the walk returns it leaves a
// mark for the message, and a later piece that finds the mark ends the read
// with a TProtocolException.
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
function walk(trans, state, writeIndex, desc) {
  var buf = state.buf;
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
        release(trans, state, "token");
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
      release(trans, state, "token");
      return;
    }
    if (step.push && stack.length >= MAX_SCAN_DEPTH) {
      release(trans, state, "depth");
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
    release(trans, state, "end");
    return;
  }

  // More bytes are needed. Remember where to resume and wait for the next piece.
  state.cursor = cursor;
  state.stack = stack;
  trans._msgScan = state;
  throw new InputBufferUnderrunError();
}

// The walk is done and the generated reader reads next. The mark tells a later
// piece for the same message that the reader ran out of bytes after the walk
// let it read; commitPosition() clears it once the message has been read.
function release(trans, state, reason) {
  trans._msgScan = {
    buf: state.buf,
    msgStart: state.msgStart,
    released: reason,
  };
}

// Start the walk of the struct that begins at the transport's read position,
// right after the header of a message that begins at msgStart.
function scanMessage(trans, desc, msgStart) {
  var peek = trans.borrow();
  walk(
    trans,
    {
      buf: peek.buf,
      msgStart: msgStart,
      cursor: peek.readIndex,
      stack: [{ t: 0 }], // the message's argument / result struct
    },
    peek.writeIndex,
    desc,
  );
}

// Continue the walk saved for the message that begins at the transport's read
// position, without reading the message header again. Returns false when no
// walk is saved for it, so the caller reads the header and calls scanMessage();
// returns true once the struct is complete, and throws InputBufferUnderrunError
// while it is not. If the walk already let the generated reader read this
// message on an earlier piece, the reader ran out of bytes, so the message does
// not match the types the reader expects: end the read.
function resumeMessage(trans, desc) {
  var peek = trans.borrow();
  var state = trans._msgScan;
  if (!state || state.buf !== peek.buf || state.msgStart !== peek.readIndex) {
    return false;
  }
  if (state.released) {
    trans._msgScan = undefined;
    if (state.released === "depth") {
      throw new Thrift.TProtocolException(
        Thrift.TProtocolExceptionType.DEPTH_LIMIT,
        "Message nested deeper than " + MAX_SCAN_DEPTH + " levels",
      );
    }
    throw new Thrift.TProtocolException(
      Thrift.TProtocolExceptionType.INVALID_DATA,
      "Message does not match the types its reader expects",
    );
  }
  if (state.cursor > peek.writeIndex) {
    return false;
  }
  walk(trans, state, peek.writeIndex, desc);
  return true;
}

module.exports = scanMessage;
module.exports.resume = resumeMessage;
module.exports.MAX_SCAN_DEPTH = MAX_SCAN_DEPTH;
