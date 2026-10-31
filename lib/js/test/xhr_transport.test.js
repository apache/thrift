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

/* jshint node: true, esversion: 8 */
"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");
const vm = require("vm");

const context = vm.createContext({ console });
const thriftPath = path.join(__dirname, "..", "src", "thrift.js");
vm.runInContext(fs.readFileSync(thriftPath, "utf8"), context, {
  filename: "thrift.js",
});
const Thrift = context.Thrift;

function createTransport() {
  const transport = new Thrift.TXHRTransport("/service");
  const xhr = {
    readyState: 0,
    status: 0,
    responseText: "",
    open() {},
    setRequestHeader() {},
    send() {},
  };
  transport.getXmlHttpRequestObject = () => xhr;
  return { transport, xhr };
}

function startRequest() {
  const { transport, xhr } = createTransport();
  transport.setRecvBuffer("previous response");
  transport.rpos = 5;

  const calls = [];
  function callback(...args) {
    calls.push({
      args,
      response: transport.readAll(),
      rpos: transport.rpos,
      wpos: transport.wpos,
      size: transport.recv_buf_sz,
    });
  }
  transport.flush(true, callback);
  return { transport, xhr, calls, callback };
}

function assertFailed(calls) {
  assert.deepStrictEqual(calls, [
    { args: [], response: "", rpos: 0, wpos: 0, size: 0 },
  ]);
}

let failed = 0;
function check(name, fn) {
  try {
    fn();
    console.log("ok   - " + name);
  } catch (error) {
    failed += 1;
    console.error("FAIL - " + name + ": " + error.message);
  }
}

check(
  "HTTP 200 replaces the buffer before invoking the callback without arguments",
  () => {
    const { xhr, calls } = startRequest();
    Object.assign(xhr, {
      readyState: 4,
      status: 200,
      responseText: "new response",
    });
    xhr.onreadystatechange();
    assert.deepStrictEqual(calls, [
      { args: [], response: "new response", rpos: 0, wpos: 12, size: 12 },
    ]);
  },
);

[1, 2, 3].forEach((readyState) => {
  check("does not complete at readyState " + readyState, () => {
    const { transport, xhr, calls } = startRequest();
    Object.assign(xhr, { readyState, status: 503 });
    xhr.onreadystatechange();
    assert.strictEqual(calls.length, 0);
    assert.strictEqual(transport.readAll(), "previous response");
  });
});

[201, 204, 400, 401, 415, 500, 503].forEach((status) => {
  check("HTTP " + status + " completes with an empty receive buffer", () => {
    const { xhr, calls } = startRequest();
    Object.assign(xhr, {
      readyState: 4,
      status,
      responseText: "<html>not a Thrift response</html>",
    });
    xhr.onreadystatechange();
    assertFailed(calls);
  });
});

["error", "timeout", "abort"].forEach((type) => {
  check("waits for the " + type + " event after DONE with status 0", () => {
    const { transport, xhr, calls } = startRequest();
    Object.assign(xhr, { readyState: 4, status: 0 });
    xhr.onreadystatechange();
    assert.strictEqual(calls.length, 0);
    assert.strictEqual(transport.readAll(), "previous response");

    xhr["on" + type]({ type });
    assertFailed(calls);
  });
});

check("an abort at readyState 0 completes with an empty receive buffer", () => {
  const { xhr, calls } = startRequest();
  xhr.onabort({ type: "abort" });
  assertFailed(calls);
});

check(
  "a failure clears the previous response and allows a later success",
  () => {
    const { transport, xhr, calls, callback } = startRequest();
    transport.setRecvBuffer('[1,"method",2,0,{}]');
    Object.assign(xhr, { readyState: 4, status: 503 });
    xhr.onreadystatechange();
    assertFailed(calls);
    assert.throws(() => new Thrift.TJSONProtocol(transport).readMessageBegin());

    transport.flush(true, callback);
    Object.assign(xhr, {
      readyState: 4,
      status: 200,
      responseText: '[1,"method",2,0,{}]',
    });
    xhr.onreadystatechange();
    assert.strictEqual(calls.length, 2);
    assert.strictEqual(calls[1].response, xhr.responseText);
    assert.strictEqual(calls[1].rpos, 0);
    const protocol = new Thrift.TJSONProtocol(transport);
    assert.strictEqual(protocol.readMessageBegin().fname, "method");
  },
);

[200, 503].forEach((status) => {
  check("synchronous HTTP " + status, () => {
    const { transport, xhr } = createTransport();
    Object.assign(xhr, { readyState: 4, status, responseText: "response" });
    xhr.open = (method, url, async) => {
      assert.strictEqual(async, false);
    };
    if (status === 200) {
      transport.flush(false);
      assert.strictEqual(transport.readAll(), "response");
    } else {
      assert.throws(() => transport.flush(false), /503/);
    }
    assert.strictEqual(xhr.ontimeout, undefined);
    assert.strictEqual(xhr.onabort, undefined);
  });
});

check("flushing without an XHR still returns the send buffer", () => {
  [undefined, "", "/service"].forEach((url) => {
    const transport = new Thrift.TXHRTransport(url);
    transport.getXmlHttpRequestObject = () => {
      throw new Error("XHR should not be created");
    };
    transport.write("request");
    assert.strictEqual(transport.flush(true), "request");
  });
});

if (failed > 0) {
  console.error("\n" + failed + " assertion(s) failed");
  process.exit(1);
}
console.log("\nall XHR transport assertions passed");
