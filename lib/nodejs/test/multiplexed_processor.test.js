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

// The service part of a multiplexed call name is looked up among the
// registered services only. Any other name is an unknown service.

const test = require("tape");
const thrift = require("thrift");

const Thrift = thrift.Thrift;

// A protocol to read a CALL message named "fname" with empty arguments from.
function callMessage(fname) {
  let message;
  const transport = new thrift.TBufferedTransport(undefined, function (buf) {
    message = buf;
  });
  const output = new thrift.TBinaryProtocol(transport);
  output.writeMessageBegin(fname, Thrift.MessageType.CALL, 1);
  output.writeStructBegin("args");
  output.writeFieldStop();
  output.writeStructEnd();
  output.writeMessageEnd();
  output.flush();
  return new thrift.TBinaryProtocol(new thrift.TFramedTransport(message));
}

function processError(processor, fname) {
  try {
    processor.process(callMessage(fname), null);
  } catch (err) {
    return err;
  }
  return null;
}

const cases = {
  "MultiplexedProcessor dispatches a call to its registered service": function (
    assert,
  ) {
    const processor = new thrift.MultiplexedProcessor();
    const seen = [];
    processor.registerProcessor("Calculator", {
      process: function (input) {
        seen.push(input.readMessageBegin().fname);
      },
    });
    assert.equal(processError(processor, "Calculator:add"), null, "no error");
    assert.deepEqual(seen, ["add"], "the service got the method name");
    assert.end();
  },

  "MultiplexedProcessor reports any name it did not register as unknown":
    function (assert) {
      const processor = new thrift.MultiplexedProcessor();
      processor.registerProcessor("Calculator", {
        process: function () {
          assert.fail("the registered service is not called");
        },
      });
      [
        "NoSuchService",
        "__proto__",
        "constructor",
        "toString",
        "hasOwnProperty",
      ].forEach(function (name) {
        const err = processError(processor, name + ":add");
        assert.ok(err instanceof Thrift.TException, name + ": a TException");
        assert.equal(
          err && err.message,
          "TMultiplexedProcessor: Unknown service: " + name,
          name + ": the unknown service error",
        );
      });
      assert.end();
    },
};

Object.keys(cases).forEach(function (caseName) {
  test(caseName, cases[caseName]);
});
