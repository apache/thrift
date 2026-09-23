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

const TFramedTransport = require("../lib/thrift/framed_transport");
const THeaderTransport = require("../lib/thrift/header_transport");
const THeaderProtocol = require("../lib/thrift/header_protocol");
const thrift = require("../lib/thrift");
const fs = require("fs");
const test = require("tape");
const path = require("path");

const headerPayload = fs.readFileSync(
  path.join(__dirname, "test_header_payload"),
);

function readHeadersError(buf) {
  const transport = new TFramedTransport();
  transport.inBuf = buf;
  try {
    transport.readHeaders();
  } catch (err) {
    return err;
  }
  return null;
}

const cases = {
  "Should read headers from payload": function (assert) {
    const transport = new TFramedTransport();
    transport.inBuf = Buffer.from(headerPayload);

    const headers = transport.readHeaders();
    assert.equals(headers.Parent, "shoobar");
    assert.equals(headers.Trace, "abcde");
    assert.end();
  },
  "Should read different headers from different payload": function (assert) {
    const transport = new TFramedTransport();
    const buf = Buffer.from(headerPayload);
    buf[24] = 115; // Change Parent to Parens
    buf[32] = 122; // Change shoobar to shoobaz
    transport.inBuf = buf;

    const headers = transport.readHeaders();
    assert.equals(headers.Parent, undefined);
    assert.equals(headers.Parens, "shoobaz");
    assert.equals(headers.Trace, "abcde");
    assert.end();
  },
  "Should read headers when reading message begin": function (assert) {
    const transport = new TFramedTransport();
    transport.inBuf = Buffer.from(headerPayload);
    const protocol = new THeaderProtocol(transport);
    const result = protocol.readMessageBegin();

    const headers = transport.getReadHeaders();
    assert.equals(headers.Parent, "shoobar");
    assert.equals(headers.Trace, "abcde");
    assert.equals(result.fname, "add");
    assert.equals(result.mtype, thrift.Thrift.MessageType.CALL);
    assert.end();
  },
  "Should be able to write headers": function (assert) {
    const writeTransport = new TFramedTransport();
    writeTransport.setProtocolId(THeaderTransport.SubprotocolId.BINARY);
    writeTransport.setWriteHeader("Hihihihi", "hohohoho");
    writeTransport.setWriteHeader("boobooboo", "fooshoopoo");
    writeTransport.setWriteHeader("a", "z");
    writeTransport.writeHeaders();
    const writeBuffer = writeTransport.outBuffers[0];

    const readTransport = new TFramedTransport();
    readTransport.inBuf = writeBuffer;
    readTransport.readHeaders();

    const headers = readTransport.getReadHeaders();
    assert.equals(headers.Hihihihi, "hohohoho");
    assert.equals(headers.boobooboo, "fooshoopoo");
    assert.equals(headers.a, "z");
    assert.end();
  },
  "Separate transports should have separate headers": function (assert) {
    const writeTransport = new TFramedTransport();
    writeTransport.setProtocolId(THeaderTransport.SubprotocolId.BINARY);
    writeTransport.setWriteHeader("foo", "bar");
    const headers = writeTransport.getWriteHeaders();

    const otherWriteTransport = new TFramedTransport();
    otherWriteTransport.setProtocolId(THeaderTransport.SubprotocolId.BINARY);
    otherWriteTransport.setWriteHeader("otherfoo", "baz");
    const otherHeaders = otherWriteTransport.getWriteHeaders();

    assert.equals(headers.foo, "bar");
    assert.equals(headers.otherfoo, undefined);
    assert.equals(otherHeaders.foo, undefined);
    assert.equals(otherHeaders.otherfoo, "baz");
    assert.end();
  },
  "Should handle large messages without crashing": function (assert) {
    const callback = function () {};
    const onData = TFramedTransport.receiver(callback);

    const largeChunkSize = 2 * 100 * 1024 * 1024;
    const largeChunk = Buffer.alloc(largeChunkSize, "A");
    const sizeBuffer = Buffer.alloc(4);
    sizeBuffer.writeInt32BE(largeChunkSize + 4, 0);
    onData(Buffer.concat([sizeBuffer, largeChunk]));

    assert.end();
  },
  "Should read the header size as an unsigned field": function (assert) {
    const buf = Buffer.from(headerPayload);
    buf.writeUInt16BE(0x8001, 12); // header size, in 4-byte words
    const err = readHeadersError(buf);
    assert.equals(err && err.name, "THeaderTransportError");
    assert.equals(err && err.message, "Header size is greater than frame size");
    assert.end();
  },
  "Should reject a frame too short for the fields it needs": function (assert) {
    const frames = [
      [],
      [0x00],
      [0x00, 0x00, 0x00],
      // no complete header magic
      [0x00, 0x00, 0x00, 0x01, 0x0f],
      // header magic and flags, but no complete sequence number
      [0x00, 0x00, 0x00, 0x06, 0x0f, 0xff, 0x00, 0x00, 0x00, 0x00],
      // header magic, but no complete header size
      [
        0x00, 0x00, 0x00, 0x09, 0x0f, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
        0x00,
      ],
    ];
    frames.forEach(function (bytes) {
      const err = readHeadersError(Buffer.from(bytes));
      assert.equals(
        err && err.name,
        "THeaderTransportError",
        bytes.length + " bytes: THeaderTransportError",
      );
      assert.equals(
        err && err.message,
        "Frame is too small",
        bytes.length + " bytes: frame too small",
      );
    });
    assert.end();
  },
  "Should still detect a short unframed compact message": function (assert) {
    const transport = new TFramedTransport();
    transport.inBuf = Buffer.from([0x82, 0x21, 0x00, 0x00, 0x00]);
    assert.doesNotThrow(function () {
      transport.readHeaders();
    });
    assert.equals(
      transport.getProtocolId(),
      THeaderTransport.SubprotocolId.COMPACT,
    );
    assert.end();
  },
};

Object.keys(cases).forEach(function (caseName) {
  test(caseName, cases[caseName]);
});
