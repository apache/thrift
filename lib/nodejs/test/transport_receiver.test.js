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
const net = require("net");
const TBufferedTransport = require("thrift/lib/nodejs/lib/thrift/buffered_transport");
const TFramedTransport = require("thrift/lib/nodejs/lib/thrift/framed_transport");
const thriftServer = require("thrift/lib/nodejs/lib/thrift/server");

function frame(payload) {
  const f = Buffer.alloc(4 + payload.length);
  f.writeInt32BE(payload.length, 0);
  payload.copy(f, 4);
  return f;
}

const cases = {
  "TBufferedTransport.receiver rejects a message larger than maxLength":
    function (assert) {
      const onData = TBufferedTransport.receiver(function () {}, undefined, 10);
      assert.throws(function () {
        onData(Buffer.alloc(20, 0x61));
      });
      assert.end();
    },

  "TBufferedTransport.receiver reassembles a message across chunks": function (
    assert,
  ) {
    let captured;
    const onData = TBufferedTransport.receiver(
      function (reader) {
        captured = reader;
      },
      undefined,
      1 << 20,
    );
    const a = Buffer.from("hello");
    const b = Buffer.from(" framed");
    const c = Buffer.from(" world!!");
    onData(a);
    onData(b);
    onData(c);
    assert.deepEqual(
      captured.inBuf.slice(0, captured.writeCursor),
      Buffer.concat([a, b, c]),
    );
    assert.end();
  },

  "TBufferedTransport.receiver without maxLength accepts large input":
    function (assert) {
      const onData = TBufferedTransport.receiver(function () {}, undefined);
      assert.doesNotThrow(function () {
        onData(Buffer.alloc(5000000, 0x61));
      });
      assert.end();
    },

  "TFramedTransport.receiver rejects an oversized frame on the size field":
    function (assert) {
      const onData = TFramedTransport.receiver(function () {}, undefined, 100);
      const header = Buffer.alloc(4);
      header.writeInt32BE(1000, 0); // exceeds maxLength, only size field sent
      assert.throws(function () {
        onData(header);
      });
      assert.end();
    },

  "TFramedTransport.receiver rejects a negative frame size when capped":
    function (assert) {
      const onData = TFramedTransport.receiver(function () {}, undefined, 100);
      const header = Buffer.alloc(4);
      header.writeInt32BE(-1, 0);
      assert.throws(function () {
        onData(header);
      });
      assert.end();
    },

  "TFramedTransport.receiver reassembles a frame fed one byte at a time":
    function (assert) {
      const payload = Buffer.from("a-complete-thrift-frame-payload");
      const f = frame(payload);
      let captured;
      const onData = TFramedTransport.receiver(
        function (t) {
          captured = t;
        },
        undefined,
        1 << 20,
      );
      for (let i = 0; i < f.length; i++) {
        onData(f.subarray(i, i + 1));
      }
      assert.ok(captured, "callback invoked");
      assert.deepEqual(captured.inBuf, payload);
      assert.end();
    },

  "TFramedTransport.receiver delivers multiple frames from one chunk":
    function (assert) {
      const p1 = Buffer.from("first");
      const p2 = Buffer.from("second!!");
      const both = Buffer.concat([frame(p1), frame(p2)]);
      const seen = [];
      const onData = TFramedTransport.receiver(
        function (t) {
          seen.push(t.inBuf);
        },
        undefined,
        1 << 20,
      );
      onData(both);
      assert.deepEqual(seen, [p1, p2]);
      assert.end();
    },

  "TFramedTransport.receiver without maxLength waits for a large frame":
    function (assert) {
      let called = false;
      const onData = TFramedTransport.receiver(function () {
        called = true;
      }, undefined);
      const header = Buffer.alloc(4);
      header.writeInt32BE(5000000, 0);
      assert.doesNotThrow(function () {
        onData(header);
      });
      assert.notOk(called, "callback not invoked while frame is incomplete");
      assert.end();
    },

  "createMultiplexServer rejects an oversized frame and stays alive": function (
    assert,
  ) {
    const server = thriftServer.createMultiplexServer(
      {
        process: function () {
          assert.fail("processor should not be reached for an oversized frame");
        },
      },
      { transport: TFramedTransport, maxLength: 100 },
    );
    let emittedError = false;
    server.on("error", function () {
      emittedError = true;
    });
    server.listen(0, "127.0.0.1", function () {
      const port = server.address().port;
      const c = net.connect(port, "127.0.0.1", function () {
        const header = Buffer.alloc(4);
        header.writeInt32BE(1000, 0); // exceeds maxLength 100
        c.write(header);
      });
      c.on("error", function () {}); // ignore reset on close
      c.on("close", function () {
        assert.ok(
          emittedError,
          "server emitted an error for the oversized frame",
        );
        server.close(function () {
          assert.end();
        });
      });
    });
  },
};

Object.keys(cases).forEach(function (caseName) {
  test(caseName, cases[caseName]);
});
