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

import test from "tape";
import { IncludeTest as IncludeTestEs5 } from "./gen-nodejs/Include_types.js";
import { IncludeTest as IncludeTestEs6 } from "./gen-nodejs-es6/Include_types.js";
import { IncludeTest as IncludeTestEsm } from "./gen-nodejs-esm/Include_types.mjs";

function constructTest(classVariant) {
  return function (t) {
    const obj = new classVariant({ bools: { im_true: true, im_false: false } });

    t.assert(obj.bools.im_true === true);
    t.assert(obj.bools.im_false === false);
    t.end();
  };
}

test("construct es5", constructTest(IncludeTestEs5));
test("construct es6", constructTest(IncludeTestEs6));
test("construct esm", constructTest(IncludeTestEsm));
