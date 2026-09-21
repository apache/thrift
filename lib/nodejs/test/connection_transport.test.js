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
const XHRConnection =
  require("thrift/lib/nodejs/lib/thrift/xhr_connection").XHRConnection;
const TWebSocketTransport = require("thrift/lib/nodejs/lib/thrift/ws_transport");

const transports = {
  XHRConnection: new XHRConnection("localhost", 9090, {
    // Skip RPC decoding so the sample text can be read directly.
    transport: { receiver: () => () => {} },
  }),
  TWebSocketTransport: new TWebSocketTransport(),
};

Object.entries(transports).forEach(function ([name, transport]) {
  test(name + " reads from the receive buffer", function (assert) {
    transport.setRecvBuffer("abcdef");

    assert.equal(transport.read(2), "ab");
    assert.equal(transport.read(10), "cdef");
    assert.equal(transport.read(1), "");
    assert.end();
  });
});

test("XHRConnection reads bytes from an ArrayBuffer receive buffer", function (assert) {
  // flush() hands the replies of the binary protocols to setRecvBuffer() as an
  // ArrayBuffer. Bytes from 0x80 up must come back as they were sent.
  const transport = transports.XHRConnection;
  transport.setRecvBuffer(
    new Uint8Array([0x80, 0x01, 0x00, 0xff, 0x42]).buffer,
  );

  const head = transport.read(2);
  assert.ok(Buffer.isBuffer(head), "binary data is read as a Buffer");
  assert.deepEqual([...head], [0x80, 0x01]);
  assert.deepEqual([...transport.read(10)], [0x00, 0xff, 0x42]);
  assert.deepEqual([...transport.read(1)], []);
  assert.end();
});
