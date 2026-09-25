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

// A service that extends another service must be loadable in every
// generated variant. The esm variant used to import the parent service
// with require(), which fails in an ES module.

import test from "tape";
import * as DerivedEs5 from "./gen-nodejs/ExtendsTestDerived.js";
import * as DerivedEs6 from "./gen-nodejs-es6/ExtendsTestDerived.js";
import * as DerivedEsm from "./gen-nodejs-esm/ExtendsTestDerived.mjs";

function extendsTest(derived) {
  return function (t) {
    t.equal(typeof derived.Client.prototype.hello, "function");
    t.equal(typeof derived.Client.prototype.ping, "function");
    t.equal(typeof derived.Processor.prototype.process_hello, "function");
    t.equal(typeof derived.Processor.prototype.process_ping, "function");
    t.end();
  };
}

test("service extends es5", extendsTest(DerivedEs5));
test("service extends es6", extendsTest(DerivedEs6));
test("service extends esm", extendsTest(DerivedEsm));
