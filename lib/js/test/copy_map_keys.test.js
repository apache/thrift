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

// A map key arrives from the peer, and a generated map is a plain object. The
// name "__proto__" is not an ordinary member name on such an object, and the
// generated reader now stores it with defineProperty so the pair survives. The
// struct constructors every generated type runs copy their map fields through
// Thrift.copyMap, which must keep the same guarantee: the copied map keeps an
// own "__proto__" member, its prototype is left alone, and a "hasOwnProperty"
// key does not derail the copy or the Thrift.objectLength helper.
//
// The browser library has no Node test runner of its own (its suite is QUnit
// driven through a headless browser), so this regression test loads the real
// src/thrift.js into a vm context -- every object and prototype below is the
// context's own -- and drives Thrift.copyMap / Thrift.objectLength directly.

const vm = require("vm");
const fs = require("fs");
const path = require("path");
const assert = require("assert");

const thriftPath = path.join(__dirname, "..", "src", "thrift.js");
const ctx = vm.createContext({ console });
vm.runInContext(fs.readFileSync(thriftPath, "utf8"), ctx, {
  filename: "thrift.js",
});

// Build the inputs inside the context. A "__proto__" or "hasOwnProperty" key is
// never written as a source literal -- that would hit the very setter under
// test -- so it is installed with defineProperty, which is the own-member shape
// the generated reader produces for a key read off the wire.
const results = vm.runInContext(
  `
(function () {
  function Inner(args) { this.s = null; if (args && args.s !== undefined) { this.s = args.s; } }
  function withKey(key, value) {
    var h = {};
    Object.defineProperty(h, key, { value: value, writable: true, enumerable: true, configurable: true });
    return h;
  }
  var hasOwn = Object.prototype.hasOwnProperty;
  var out = {};

  // 1) map<string,string> carrying a "__proto__" key (primitive value).
  var m1 = withKey('__proto__', 'PEER'); m1.benign = 'ok';
  var c1 = Thrift.copyMap(m1, [null]);
  out.strProtoOwnKey = hasOwn.call(c1, '__proto__');
  out.strProtoValue  = hasOwn.call(c1, '__proto__') ? c1.__proto__ : null;
  out.strProtoIntact = Object.getPrototypeOf(c1) === Object.prototype;
  out.strBenign      = c1.benign;

  // 2) map<string,Inner> carrying a "__proto__" key (object value -> would swap
  //    the prototype of the copy if the write were a plain assignment).
  var m2 = withKey('__proto__', { s: 'PEER' }); m2.benign = { s: 'ok' };
  var c2 = Thrift.copyMap(m2, [Inner]);
  out.recProtoOwnKey = hasOwn.call(c2, '__proto__');
  out.recProtoValueS = hasOwn.call(c2, '__proto__') ? c2.__proto__.s : null;
  out.recProtoIntact = Object.getPrototypeOf(c2) === Object.prototype;
  out.recValueIsInner = hasOwn.call(c2, '__proto__') && (c2.__proto__ instanceof Inner);

  // 3) map<string,string> carrying a "hasOwnProperty" key.
  var m3 = withKey('hasOwnProperty', 'PEER'); m3.benign = 'ok';
  var threw3 = false, c3 = null;
  try { c3 = Thrift.copyMap(m3, [null]); } catch (e) { threw3 = true; }
  out.hopThrew = threw3;
  out.hopOwnKey = c3 && hasOwn.call(c3, 'hasOwnProperty');
  out.hopValue = c3 && hasOwn.call(c3, 'hasOwnProperty') ? c3.hasOwnProperty : null;
  out.hopBenign = c3 && c3.benign;

  // 4) Thrift.objectLength over a "hasOwnProperty"-keyed object.
  var m4 = withKey('hasOwnProperty', 'PEER'); m4.a = 1; m4.b = 2;
  var threw4 = false, len4 = -1;
  try { len4 = Thrift.objectLength(m4); } catch (e) { threw4 = true; }
  out.objLenThrew = threw4;
  out.objLen = len4;

  // 5) a plain map still copies unchanged.
  var m5 = { one: new Inner({ s: 'a' }), two: new Inner({ s: 'b' }) };
  var c5 = Thrift.copyMap(m5, [Inner]);
  out.plainKeys = Object.keys(c5).join(',');
  out.plainValueS = c5.one.s + c5.two.s;

  // 6) the global Object.prototype must be left unchanged throughout.
  out.prototypeChanged = ({}).s !== undefined || ({}).benign !== undefined || ({}).PEER !== undefined;

  return out;
})()
`,
  ctx,
);

let failed = 0;
function check(name, fn) {
  try {
    fn();
    console.log("ok   - " + name);
  } catch (e) {
    failed += 1;
    console.log("FAIL - " + name + ": " + e.message);
  }
}

// f24: the "__proto__" entry survives the copy as an own member, prototype intact.
check("map<string,string>: __proto__ kept as own key", () =>
  assert.strictEqual(results.strProtoOwnKey, true),
);
check("map<string,string>: __proto__ value preserved", () =>
  assert.strictEqual(results.strProtoValue, "PEER"),
);
check("map<string,string>: copy prototype is Object.prototype", () =>
  assert.strictEqual(results.strProtoIntact, true),
);
check("map<string,string>: benign entry survives", () =>
  assert.strictEqual(results.strBenign, "ok"),
);
check("map<string,struct>: __proto__ kept as own key", () =>
  assert.strictEqual(results.recProtoOwnKey, true),
);
check("map<string,struct>: __proto__ value preserved and typed", () => {
  assert.strictEqual(results.recProtoValueS, "PEER");
  assert.strictEqual(results.recValueIsInner, true);
});
check("map<string,struct>: copy prototype NOT swapped", () =>
  assert.strictEqual(results.recProtoIntact, true),
);

// 96-S7: a "hasOwnProperty" key does not throw in copyMap or objectLength.
check("copyMap: hasOwnProperty key does not throw", () =>
  assert.strictEqual(results.hopThrew, false),
);
check("copyMap: hasOwnProperty entry round-trips", () => {
  assert.strictEqual(results.hopOwnKey, true);
  assert.strictEqual(results.hopValue, "PEER");
  assert.strictEqual(results.hopBenign, "ok");
});
check("objectLength: hasOwnProperty key does not throw", () =>
  assert.strictEqual(results.objLenThrew, false),
);
check("objectLength: counts every own key (a, b, hasOwnProperty)", () =>
  assert.strictEqual(results.objLen, 3),
);

// regression: a plain map copies unchanged.
check("plain map copies unchanged", () => {
  assert.strictEqual(results.plainKeys, "one,two");
  assert.strictEqual(results.plainValueS, "ab");
});

// the global prototype is left unchanged.
check("the global Object.prototype is left unchanged", () =>
  assert.strictEqual(results.prototypeChanged, false),
);

if (failed > 0) {
  console.log("\n" + failed + " assertion(s) failed");
  process.exit(1);
}
console.log("\nall copyMap / objectLength map-key assertions passed");
