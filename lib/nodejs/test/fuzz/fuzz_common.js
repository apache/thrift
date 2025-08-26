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

const thrift = require("../../lib/thrift");
const FuzzTest = require("./gen-nodejs/FuzzTestNoUuid_types");
// const { FuzzedDataProvider } = require("@jazzer.js/core");

/**
 * Creates a parser fuzzer function for a specific protocol
 * @param {Function} protocolFactory - The Thrift protocol factory function
 * @param {boolean} [readMessageBegin=false] - Whether to call readMessageBegin before reading the test instance
 * This is needed for protocols that do not support parsing just a struct, such as TJSONProtocol.
 * @returns {Function} A fuzzer function that can be used with Jazzer.js
 */
function createParserFuzzer(protocolFactory, readMessageBegin = false) {
  return function fuzz(data) {
    if (data.length < 2) {
      return;
    }

    try {
      // Set up transport with input data
      const transport = new thrift.TFramedTransport(data);
      const protocol = protocolFactory(transport);
      const testInstance = new FuzzTest.FuzzTest();
      if (readMessageBegin) {
        protocol.readMessageBegin();
      }
      testInstance[Symbol.for("read")](protocol);
    } catch (e) {
      if (
        !(
          e.name === "InputBufferUnderrunError" ||
          e.name === "TProtocolException"
        )
      ) {
        // TODO: Are other exceptions expected?
        // console.log(e);
      }
    }
  };
}

/**
 * Creates a roundtrip fuzzer function for a specific protocol
 * @param {Function} protocolFactory - The Thrift protocol factory function
 * @param {boolean} [readMessageBegin=false] - Whether to call readMessageBegin before reading the test instance
 * This is needed for protocols that do not support parsing just a struct, such as TJSONProtocol.
 * @returns {Function} A fuzzer function that can be used with Jazzer.js
 */
function createRoundtripFuzzer(protocolFactory, readMessageBegin = false) {
  return function fuzz(data) {
    if (data.length < 2) {
      return;
    }

    try {
      // First deserialize using framed transport for input
      const transport1 = new thrift.TFramedTransport(data);
      const protocol1 = protocolFactory(transport1);
      const testInstance = new FuzzTest.FuzzTest();
      if (readMessageBegin) {
        protocol1.readMessageBegin();
      }
      testInstance[Symbol.for("read")](protocol1);

      // Then serialize using buffered transport with callback
      let serializedData;
      const transport2 = new thrift.TBufferedTransport(null, function (buf) {
        serializedData = buf;
      });
      const protocol2 = protocolFactory(transport2);
      testInstance[Symbol.for("write")](protocol2);
      protocol2.flush();

      if (!serializedData) {
        throw new Error("Serialization failed - no data produced");
      }

      // Finally deserialize again and compare using framed transport
      const transport3 = new thrift.TFramedTransport(serializedData);
      const protocol3 = protocolFactory(transport3);
      const deserialized = new FuzzTest.FuzzTest();
      if (readMessageBegin) {
        protocol3.readMessageBegin();
      }
      deserialized[Symbol.for("read")](protocol3);

      // Compare the objects
      if (!testInstance.equals(deserialized)) {
        throw new Error("Roundtrip comparison failed");
      }
    } catch (e) {
      if (
        !(
          e.name === "InputBufferUnderrunError" ||
          e.name === "TProtocolException"
        )
      ) {
        // TODO: Are other exceptions expected?
        // console.log(e);
      }
    }
  };
}

module.exports = {
  createParserFuzzer,
  createRoundtripFuzzer,
};
