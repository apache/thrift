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
 /* jshint -W100 */

var Int64 = require("node-int64");
var JSONInt64 = require("json-int64");
import { Int64Test } from "./gen-js/Int64Test_types";


// Work around for old API used by QUnitAdapter of jsTestDriver
if (typeof QUnit.log == 'function') {
  // When using real QUnit (fron PhantomJS) log failures to console
  QUnit.log(function(details) {
    if (!details.result) {
      console.log('======== FAIL ========');
      console.log('TestName: ' + details.name);
      if (details.message) console.log(details.message);
      console.log('Expected: ' + details.expected);
      console.log('Actual  : ' + details.actual);
      console.log('======================');
    }
  });
}

QUnit.module('Int64');

  QUnit.test('Int64', function(assert) {
    console.log('Int64 test -- starts');
    const EXPECTED_SMALL_INT64_AS_NUMBER: number = 42;
    const EXPECTED_SMALL_INT64: typeof Int64 = new Int64(42);
    const EXPECTED_MAX_JS_SAFE_INT64: typeof Int64 = new Int64(Number.MAX_SAFE_INTEGER);
    const EXPECTED_MIN_JS_SAFE_INT64: typeof Int64 = new Int64(Number.MIN_SAFE_INTEGER);
    const EXPECTED_MAX_JS_SAFE_PLUS_ONE_INT64: typeof Int64 = new Int64("0020000000000000"); // hex-encoded
    const EXPECTED_MIN_JS_SAFE_MINUS_ONE_INT64: typeof Int64 = new Int64("ffe0000000000000"); // hex-encoded 2's complement
    const EXPECTED_MAX_SIGNED_INT64: typeof Int64 = new Int64("7fffffffffffffff"); // hex-encoded
    const EXPECTED_MIN_SIGNED_INT64: typeof Int64 = new Int64("8000000000000000"); // hex-encoded 2's complement
    const EXPECTED_INT64_LIST = [
      EXPECTED_SMALL_INT64,
      EXPECTED_MAX_JS_SAFE_INT64,
      EXPECTED_MIN_JS_SAFE_INT64,
      EXPECTED_MAX_JS_SAFE_PLUS_ONE_INT64,
      EXPECTED_MIN_JS_SAFE_MINUS_ONE_INT64,
      EXPECTED_MAX_SIGNED_INT64,
      EXPECTED_MIN_SIGNED_INT64
    ];
    assert.ok(EXPECTED_SMALL_INT64.equals(Int64Test.SMALL_INT64));
    assert.ok(EXPECTED_MAX_JS_SAFE_INT64.equals(Int64Test.MAX_JS_SAFE_INT64));
    assert.ok(EXPECTED_MIN_JS_SAFE_INT64.equals(Int64Test.MIN_JS_SAFE_INT64));
    assert.ok(
      EXPECTED_MAX_JS_SAFE_PLUS_ONE_INT64.equals(
        Int64Test.MAX_JS_SAFE_PLUS_ONE_INT64
      )
    );
    assert.ok(
      EXPECTED_MIN_JS_SAFE_MINUS_ONE_INT64.equals(
        Int64Test.MIN_JS_SAFE_MINUS_ONE_INT64
      )
    );
    assert.ok(EXPECTED_MAX_SIGNED_INT64.equals(Int64Test.MAX_SIGNED_INT64));
    assert.ok(EXPECTED_MIN_SIGNED_INT64.equals(Int64Test.MIN_SIGNED_INT64));
    assert.equal(
      EXPECTED_SMALL_INT64_AS_NUMBER,
      Int64Test.SMALL_INT64.toNumber()
    );
    assert.equal(
      Number.MAX_SAFE_INTEGER,
      Int64Test.MAX_JS_SAFE_INT64.toNumber()
    );
    assert.equal(
      Number.MIN_SAFE_INTEGER,
      Int64Test.MIN_JS_SAFE_INT64.toNumber()
    );

    for (let i = 0; i < EXPECTED_INT64_LIST.length; ++i) {
      assert.ok(EXPECTED_INT64_LIST[i].equals(Int64Test.INT64_LIST[i]));
    }

    for (let i = 0; i < EXPECTED_INT64_LIST.length; ++i){
      let int64Object = EXPECTED_INT64_LIST[i];
      assert.ok(Int64Test.INT64_2_INT64_MAP[JSONInt64.toDecimalString(int64Object)].equals(int64Object));
    }

    console.log('Int64 test -- ends');
  });

