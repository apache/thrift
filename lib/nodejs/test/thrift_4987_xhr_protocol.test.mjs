#!/usr/bin/env node
/**
 * Regression test for THRIFT-4987:
 *   "TProtocolException: Bad version in readMessageBegin when using XHR
 *    client with binary protocol against a C++ server"
 *
 * Disclaimer: This file, except for this disclaimer, was written entirely by
 * Claude. Claude needed a lot of guidance on getting to this solution to test
 * the actual code in question which lead to finding an issue in the previous
 * proposed solution and it was subsequently fixed. The Claude output was fixed
 * using eslint.
 *
 * ─────────────────────────────────────────────────────────────────────────
 * BUG ANATOMY
 * ─────────────────────────────────────────────────────────────────────────
 * File:  lib/nodejs/lib/thrift/xhr_connection.js
 *
 * XHRConnection.flush() (line 103–121) builds an XMLHttpRequest and fires
 * it.  The onreadystatechange handler reads the server reply as:
 *
 *   self.setRecvBuffer(this.responseText);            // line 117 — THE BUG
 *
 * setRecvBuffer() (line 134) then does:
 *
 *   if ([object ArrayBuffer]) { data = new Uint8Array(buf); }  // line 140
 *   var thing = new Buffer(data || buf);                       // line 143
 *
 * When `buf` is a string (which it always is from line 117), the ArrayBuffer
 * guard on line 140 is never reached — it is dead code.  `new Buffer(string)`
 * encodes the string as UTF-8: any character with code point 0x80–0xFF
 * becomes a 2-byte UTF-8 sequence, inflating the buffer and shifting all
 * subsequent byte positions.
 *
 * TBinaryProtocol and TCompactProtocol both start their REPLY messages with
 * a byte >= 0x80 (the protocol identifier / version word), so after the
 * corruption the first readI32/readByte reads a garbage value and throws
 * TProtocolException.
 *
 * TJSONProtocol is NOT affected because its wire format is pure 7-bit ASCII
 * — the Latin-1→UTF-8 round-trip is a no-op for bytes <= 0x7F.  This is
 * why xhr_connection.js defaulting to TJSONProtocol masked the bug for years.
 *
 * THE FIX (two lines in XHRConnection.flush()):
 *   xreq.responseType = 'arraybuffer';    // BEFORE xreq.open() — line ~105
 *   self.setRecvBuffer(this.response);    // not this.responseText — line 117
 *
 * With responseType='arraybuffer', this.response is an ArrayBuffer, the
 * guard on line 140 fires, and new Buffer(Uint8Array) copies bytes verbatim.
 *
 * ─────────────────────────────────────────────────────────────────────────
 * TEST APPROACH
 * ─────────────────────────────────────────────────────────────────────────
 * We exercise XHRConnection.flush() and XHRConnection.setRecvBuffer()
 * directly by replacing getXmlHttpRequestObject() with a MockXHR.
 *
 * MockXHR:
 *   - holds both responseText (Latin-1 string of the raw bytes) and
 *     response (actual ArrayBuffer of the raw bytes) pre-loaded from
 *     the raw wire-level REPLY buffer produced by the server protocol
 *   - its send() method fires onreadystatechange immediately, using
 *     whichever property flush() selected (responseType decides this):
 *       responseType === 'arraybuffer'  → delivers this.response (ArrayBuffer)
 *       responseType === ''             → delivers this.responseText (string)
 *
 * We also supply a minimal stub Thrift client attached to the connection
 * (connection.client) so that __decodeCallback can look up recv_testI32.
 * That stub captures the decoded result or error so the test can assert on it.
 *
 * Three scenarios per protocol:
 *   A  reproduceIssue=true — _brokenFlush() active:
 *      verbatim copy of the unpatched XHRConnection.flush(), which uses
 *      responseText with no responseType set → new Buffer(string) → UTF-8
 *      inflation → TProtocolException thrown from __decodeCallback
 *
 *   B  reproduceIssue=false — real XHRConnection.prototype.flush() called:
 *      no flush() override on the instance; the library's own flush() runs.
 *      Once the fix (responseType='arraybuffer' + this.response) is applied
 *      to the library, this path produces a clean decode.  If the library is
 *      still unpatched, this path fails too — making the regression visible.
 *
 * ─────────────────────────────────────────────────────────────────────────
 * RUNNING (standalone, no server needed)
 * ─────────────────────────────────────────────────────────────────────────
 *   cd <thrift-repo>/test/nodejs
 *   npm install thrift
 *   node thrift_4987_xhr_protocol.test.mjs
 *
 * ─────────────────────────────────────────────────────────────────────────
 * WIRING INTO THE CROSS-LANGUAGE MAKE TARGET
 * ─────────────────────────────────────────────────────────────────────────
 *   In test/nodejs/Makefile.am, add alongside the existing client.mjs target:
 *
 *     check-THRIFT-4987:
 *         $(NODE) thrift_4987_xhr_protocol.test.mjs && echo "THRIFT-4987 PASS"
 *
 *   No server is required; the test is entirely self-contained.
 */

import { createRequire } from "module";
const require = createRequire(import.meta.url);

const thrift = require("thrift");
const XHRConnection =
  require("thrift/lib/nodejs/lib/thrift/xhr_connection").XHRConnection;
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");
const TBinaryProtocol = require("thrift/lib/nodejs/lib/thrift/binary_protocol");
const TCompactProtocol = require("thrift/lib/nodejs/lib/thrift/compact_protocol");
const TJSONProtocol = require("thrift/lib/nodejs/lib/thrift/json_protocol");
const { Thrift } = thrift;

const GREEN = (s) => `\x1b[32m${s}\x1b[0m`;
const RED = (s) => `\x1b[31m${s}\x1b[0m`;
const BOLD = (s) => `\x1b[1m${s}\x1b[0m`;
const DIM = (s) => `\x1b[2m${s}\x1b[0m`;

// ─────────────────────────────────────────────────────────────────────────────
// Encode a server-side TBinaryProtocol / TCompactProtocol / TJSONProtocol
// REPLY for testI32(42).  This is the raw buffer the server would send over
// HTTP; what arrives at the browser depends on how XHR decodes the body.
// ─────────────────────────────────────────────────────────────────────────────
function encodeServerReply(ProtoClass, methodName, seqId, i32Value) {
  let encoded = null;
  const t = new TBufferedTransport(null, (buf) => {
    encoded = buf;
  });
  const p = new ProtoClass(t);
  p.writeMessageBegin(methodName, Thrift.MessageType.REPLY, seqId);
  p.writeStructBegin("testI32_result");
  p.writeFieldBegin("success", Thrift.Type.I32, 0);
  p.writeI32(i32Value);
  p.writeFieldEnd();
  p.writeFieldStop();
  p.writeStructEnd();
  p.writeMessageEnd();
  t.flush();
  if (!encoded) throw new Error("TBufferedTransport did not flush");
  return encoded;
}

// ─────────────────────────────────────────────────────────────────────────────
// MockXHR — drop-in replacement for the browser XMLHttpRequest object.
//
// Holds two representations of the raw server reply:
//   responseText  — Latin-1 string (charCode === byte value), as a browser
//                   delivers when Content-Type has no charset or charset=latin-1
//   response      — actual ArrayBuffer of the raw bytes, as a browser delivers
//                   when xreq.responseType = 'arraybuffer'
//
// send() fires onreadystatechange synchronously (simulating a completed XHR).
// It inspects this.responseType (set by flush() before open() in the fix) to
// decide which property to hand back, exactly mirroring browser behaviour.
// ─────────────────────────────────────────────────────────────────────────────
class MockXHR {
  constructor(rawBuf) {
    this.readyState = 0;
    this.status = 0;
    this.responseType = ""; // flush() writes this; send() reads it back

    // responseText: browser Latin-1 decoding of the raw HTTP body bytes.
    // Each byte value maps 1:1 to a character code point.
    let latin1 = "";
    for (let i = 0; i < rawBuf.length; i++)
      latin1 += String.fromCharCode(rawBuf[i]);
    this.responseText = latin1;

    // response: ArrayBuffer view of the same raw bytes (responseType='arraybuffer').
    const ab = new ArrayBuffer(rawBuf.length);
    new Uint8Array(ab).set(rawBuf);
    this.response = ab;
  }

  overrideMimeType() {}

  open() {
    this.readyState = 1;
  }

  setRequestHeader() {}

  send() {
    this.readyState = 4;
    this.status = 200;
    // Mirror browser: deliver response vs responseText based on responseType.
    // This is the decision point under test:
    //   - responseType === ''            → this.responseText is a string   → bug path
    //   - responseType === 'arraybuffer' → this.response is an ArrayBuffer → fix path
    if (this.responseType === "arraybuffer") {
      // Temporarily shadow responseText so setRecvBuffer can only get the ArrayBuffer
      const savedText = this.responseText;
      this.responseText = null;
      this.onreadystatechange();
      this.responseText = savedText;
    } else {
      // Temporarily shadow response so setRecvBuffer can only get the string
      const savedResponse = this.response;
      this.response = null;
      this.onreadystatechange();
      this.response = savedResponse;
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
// TestableXHRConnection — subclass of XHRConnection that injects MockXHR.
//
// getXmlHttpRequestObject() is always overridden so the real network is never
// touched; MockXHR pre-loads the raw server reply and fires onreadystatechange
// synchronously when send() is called.
//
// reproduceIssue (boolean):
//   true  — also overrides flush() with the original broken implementation
//            (responseText, no responseType) to confirm the bug is present in
//            the unpatched code path.  _brokenFlush() is a verbatim copy of
//            the buggy XHRConnection.flush() so the test documents exactly
//            what is wrong.
//
//   false — does NOT override flush(); the real XHRConnection.flush() from
//            the library is called as-is.  Once the fix is applied to the
//            library, this path must produce a clean decode.  If flush() is
//            still broken in the library, this path will also fail — which
//            is precisely the point of the regression test.
// ─────────────────────────────────────────────────────────────────────────────
class TestableXHRConnection extends XHRConnection {
  constructor(rawReplyBuf, options, reproduceIssue) {
    // XHRConnection constructor accesses window.location when host/port are
    // omitted; supply dummy values to prevent that reference error in Node.js.
    super("localhost", 9090, options);
    this._rawReplyBuf = rawReplyBuf;
    this._reproduceIssue = reproduceIssue;

    // Bind the broken flush onto this instance only when reproducing the bug.
    // This leaves the prototype chain untouched so the fixed path genuinely
    // calls XHRConnection.prototype.flush with no override in the way.
    if (reproduceIssue) {
      this.flush = this._brokenFlush.bind(this);
    }
    // When reproduceIssue is false, flush is not set here, so this.flush
    // resolves to XHRConnection.prototype.flush — the real library code.
  }

  getXmlHttpRequestObject() {
    return new MockXHR(this._rawReplyBuf);
  }

  // Verbatim copy of the buggy XHRConnection.flush() (xhr_connection.js
  // lines 103-127) before the fix was applied.
  // Used only when reproduceIssue=true.
  // The two lines that constitute the fix are deliberately absent:
  //   MISSING: xreq.responseType = 'arraybuffer';
  //   MISSING: self.setRecvBuffer(this.response);  (uses responseText instead)
  /* eslint-disable */
  _brokenFlush() {
    var self = this;
    if (this.url === undefined || this.url === "") {
      return this.send_buf;
    }

    var xreq = this.getXmlHttpRequestObject();

    if (xreq.overrideMimeType) {
      xreq.overrideMimeType("application/json");
    }

    xreq.onreadystatechange = function () {
      if (this.readyState == 4 && this.status == 200) {
        self.setRecvBuffer(this.responseText);
      }
    };

    xreq.open("POST", this.url, true);

    Object.keys(this.headers).forEach(function (headerKey) {
      xreq.setRequestHeader(headerKey, self.headers[headerKey]);
    });

    xreq.send(this.send_buf);
  }
  /* eslint-enable */
}

// ─────────────────────────────────────────────────────────────────────────────
// Minimal stub Thrift client.
//
// XHRConnection.__decodeCallback() calls:
//   client._reqs[dummy_seqid] = function(err, success) { ... }
//   client['recv_' + header.fname](proto, header.mtype, dummy_seqid)
//
// The real generated client's recv_testI32 reads the i32 from the protocol
// and invokes the callback stored in _reqs[seqid].  We supply a lightweight
// version that captures the result so the test can assert on it.
// ─────────────────────────────────────────────────────────────────────────────
function makeStubClient(ProtoClass, seqId, onResult) {
  return {
    _reqs: {
      // The real seqid callback — called by our recv_ after it reads the value
      [seqId]: (err, val) => onResult(err, val),
    },
    recv_testI32(proto) {
      // Mirrors what the generated client recv_ method does:
      //   read the success field from the REPLY struct, then invoke callback
      let result = null;
      proto.readStructBegin();
      while (true) {
        const field = proto.readFieldBegin();
        if (field.ftype === Thrift.Type.STOP) break;
        if (field.fid === 0 && field.ftype === Thrift.Type.I32) {
          result = proto.readI32();
        } else {
          proto.skip(field.ftype);
        }
        proto.readFieldEnd();
      }
      proto.readStructEnd();
      proto.readMessageEnd();
      // Invoke the callback registered by __decodeCallback under dummy_seqid
      const dummy_seqid = seqId * -1;
      const cb = this._reqs[dummy_seqid];
      if (cb) cb(null, result);
    },
  };
}

// ─────────────────────────────────────────────────────────────────────────────
// Run one scenario against a real XHRConnection.flush() call.
//
// Flow:
//   1. Build raw server REPLY buffer (what the server would write on the wire)
//   2. Create a TestableXHRConnection with a MockXHR pre-loaded with that buffer
//   3. Attach a stub client with a recv_testI32 and a seqid callback
//   4. Write the encoded client REQUEST into the connection's send buffer,
//      then call flush() — this is either _brokenFlush() (reproduceIssue=true)
//      or the real XHRConnection.prototype.flush() (reproduceIssue=false)
//   5. MockXHR.send() fires onreadystatechange synchronously; depending on
//      whether responseType='arraybuffer' was set by flush(), it delivers
//      either this.response (ArrayBuffer) or this.responseText (string)
//   6. setRecvBuffer() → transport.receiver() → __decodeCallback()
//      → proto.readMessageBegin() → recv_testI32 (success) or throw (bug)
// ─────────────────────────────────────────────────────────────────────────────
async function runScenario(ProtoClass, reproduceIssue) {
  const METHOD = "testI32",
    SEQ_ID = 1,
    VALUE = 42;

  // The raw buffer the server sends on the wire
  const rawReplyBuf = encodeServerReply(ProtoClass, METHOD, SEQ_ID, VALUE);

  return new Promise((resolve) => {
    let settled = false;
    const done = (err, val) => {
      if (!settled) {
        settled = true;
        resolve({ err, val });
      }
    };

    const conn = new TestableXHRConnection(
      rawReplyBuf,
      { transport: TBufferedTransport, protocol: ProtoClass },
      reproduceIssue,
    );

    // Listen for errors emitted by the connection on non-protocol failures.
    // NOTE: __decodeCallback in xhr_connection.js throws TProtocolException
    // directly (it does not call self.emit('error')), so protocol errors are
    // caught via the try/catch around conn.flush() below, not here.
    conn.on("error", (e) => done(e, null));

    // Attach the stub client
    conn.client = makeStubClient(ProtoClass, SEQ_ID, done);

    // Encode a minimal client-side REQUEST (just enough to have a send_buf
    // so flush() proceeds past the early-return guard).
    // We write directly to the underlying transport the same way createClient does.
    const writeCb = (buf) => {
      conn.send_buf = buf;
    };
    const sendTransport = new TBufferedTransport(undefined, writeCb);
    sendTransport.setCurrSeqId(SEQ_ID);
    const sendProto = new ProtoClass(sendTransport);
    sendProto.writeMessageBegin(METHOD, Thrift.MessageType.CALL, SEQ_ID);
    sendProto.writeStructBegin("testI32_args");
    sendProto.writeFieldBegin("thing", Thrift.Type.I32, 1);
    sendProto.writeI32(VALUE);
    sendProto.writeFieldEnd();
    sendProto.writeFieldStop();
    sendProto.writeStructEnd();
    sendProto.writeMessageEnd();
    sendTransport.flush(); // populates conn.send_buf via writeCb

    // Now fire the actual XHRConnection.flush().
    // NOTE: XHRConnection.__decodeCallback() re-throws TProtocolException
    // directly (line 199 of xhr_connection.js) rather than emitting 'error'
    // as http_connection.js does.  The throw propagates synchronously back
    // through setRecvBuffer → MockXHR.send() → flush() so we catch it here.
    try {
      conn.flush();
    } catch (e) {
      done(e, null);
    }

    // Guard: if neither the error event nor the recv_ callback fired
    // synchronously (nor a thrown exception), the mock wiring is broken.
    if (!settled)
      done(
        new Error("Neither result nor error received — check MockXHR wiring"),
        null,
      );
  });
}

// ─────────────────────────────────────────────────────────────────────────────
// Protocol table
// ─────────────────────────────────────────────────────────────────────────────
const PROTOCOLS = [
  {
    name: "TBinaryProtocol",
    Class: TBinaryProtocol,
    affected: true,
    // Version word first byte: 0x80 → inflates to 0xC2 0x80 via UTF-8
    symptom: "Bad version in readMessageBegin",
  },
  {
    name: "TCompactProtocol",
    Class: TCompactProtocol,
    affected: true,
    // PROTOCOL_ID first byte: 0x82 → inflates to 0xC2 0x82 via UTF-8
    symptom: "Bad protocol identifier",
  },
  {
    name: "TJSONProtocol",
    Class: TJSONProtocol,
    affected: false,
    // Wire format is pure 7-bit ASCII — no bytes >= 0x80, no inflation
    symptom: null,
  },
];

// ─────────────────────────────────────────────────────────────────────────────
// Main
// ─────────────────────────────────────────────────────────────────────────────
async function main() {
  console.log(
    BOLD(
      "\nTHRIFT-4987 — XHRConnection.flush() + setRecvBuffer() regression test",
    ),
  );
  console.log(
    DIM("Exercises real XHRConnection code paths via MockXHR injection\n"),
  );

  let totalPass = 0,
    totalFail = 0;
  const summary = [];

  for (const proto of PROTOCOLS) {
    console.log(BOLD(`${"─".repeat(68)}`));
    console.log(
      BOLD(
        `  ${proto.name}  ${proto.affected ? RED("(AFFECTED)") : GREEN("(NOT AFFECTED)")}`,
      ),
    );
    console.log(BOLD(`${"─".repeat(68)}`));
    console.log(
      `  ${DIM(
        proto.affected
          ? `First wire byte >= 0x80 → inflates under UTF-8 → corrupt → "${proto.symptom}"`
          : "Wire format is 7-bit ASCII → Latin-1→UTF-8 round-trip is a no-op",
      )}\n`,
    );

    let scenPass = 0,
      scenFail = 0;

    // ── Scenario A: BUG path — _brokenFlush() active (reproduceIssue=true) ──
    {
      const { err, val } = await runScenario(proto.Class, true);
      process.stdout.write(
        `  Scenario A  ${DIM("reproduceIssue=true  — _brokenFlush() (responseText, no responseType)")}:\n`,
      );

      if (proto.affected) {
        // Expect an error containing the known symptom string
        if (err && err.message && err.message.includes(proto.symptom)) {
          console.log(
            GREEN(
              `    ✓ PASS — XHRConnection emitted error: ${err.constructor.name}: ${err.message}`,
            ),
          );
          scenPass++;
        } else if (err) {
          console.log(
            RED(
              `    ✗ FAIL — got error but wrong message: ${err.constructor.name}: ${err.message}`,
            ),
          );
          scenFail++;
        } else {
          console.log(
            RED(
              `    ✗ FAIL — expected error "${proto.symptom}" but got val=${val}`,
            ),
          );
          scenFail++;
        }
      } else {
        // JSON: not affected, should decode cleanly on bug path too
        if (!err && val === 42) {
          console.log(
            GREEN(
              `    ✓ PASS — decoded correctly (JSON is immune): val=${val}`,
            ),
          );
          scenPass++;
        } else if (err) {
          console.log(
            RED(
              `    ✗ FAIL — unexpected error: ${err.constructor.name}: ${err.message}`,
            ),
          );
          scenFail++;
        } else {
          console.log(RED(`    ✗ FAIL — unexpected val=${val}`));
          scenFail++;
        }
      }
    }

    // ── Scenario B: FIX path — real XHRConnection.flush() (reproduceIssue=false) ──
    {
      const { err, val } = await runScenario(proto.Class, false);
      process.stdout.write(
        `  Scenario B  ${DIM("reproduceIssue=false — real XHRConnection.prototype.flush() from library")}:\n`,
      );

      if (!err && val === 42) {
        console.log(GREEN(`    ✓ PASS — decoded correctly: val=${val}`));
        scenPass++;
      } else if (err) {
        console.log(
          RED(
            `    ✗ FAIL — unexpected error on fix path: ${err.constructor.name}: ${err.message}`,
          ),
        );
        scenFail++;
      } else {
        console.log(RED(`    ✗ FAIL — wrong value: val=${val}`));
        scenFail++;
      }
    }

    totalPass += scenPass;
    totalFail += scenFail;
    const status = scenFail === 0 ? GREEN("PASS") : RED("FAIL");
    summary.push({
      name: proto.name,
      affected: proto.affected,
      scenPass,
      scenFail,
      status,
    });
    console.log();
  }

  // ── Summary ────────────────────────────────────────────────────────────────
  console.log(BOLD(`${"═".repeat(68)}`));
  console.log(BOLD("  SUMMARY"));
  console.log(BOLD(`${"═".repeat(68)}`));
  console.log(
    `  ${"Protocol".padEnd(22)} ${"Affected".padEnd(14)} ${"Scenarios".padEnd(14)} Result`,
  );
  console.log(`  ${"─".repeat(64)}`);
  for (const r of summary) {
    const aff = r.affected ? RED("YES") : GREEN("NO");
    const sc = `${r.scenPass}/${r.scenPass + r.scenFail} passed`;
    console.log(
      `  ${r.name.padEnd(22)} ${aff.padEnd(22)} ${sc.padEnd(14)} ${r.status}`,
    );
  }
  console.log(BOLD(`${"═".repeat(68)}`));
  console.log(`\n  Total: ${totalPass} passed, ${totalFail} failed\n`);

  if (totalFail > 0) {
    console.log(
      RED("  REGRESSION: unexpected scenario result(s) — see above.\n"),
    );
    process.exit(1);
  } else {
    console.log(GREEN("  All scenarios passed."));
    console.log(
      "  TBinaryProtocol and TCompactProtocol fail on the bug path (scenario A)",
    );
    console.log(
      "  and pass on the fix path (scenario B, real XHRConnection.prototype.flush).",
    );
    console.log(
      "  TJSONProtocol passes on both paths (immune due to 7-bit ASCII wire format).\n",
    );
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
