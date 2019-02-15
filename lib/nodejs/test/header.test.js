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
  path.join(__dirname, "test_header_payload")
);

const cases = {
  "Should read headers from payload": function(assert) {
    const transport = new TFramedTransport();
    transport.inBuf = Buffer.from(headerPayload);

    const headers = transport.readHeaders();
    assert.equals(headers.Parent, "shoobar");
    assert.equals(headers.Trace, "abcde");
    assert.end();
  },
  "Should read headers when reading message begin": function(assert) {
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
  "Should be able to write headers": function(assert) {
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
  }
};

Object.keys(cases).forEach(function(caseName) {
  test(caseName, cases[caseName]);
});
