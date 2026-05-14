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
const Thrift = require("thrift/lib/nodejs/lib/thrift/thrift");

const cases = {
  "Should accept a unique numeric set": function (assert) {
    Thrift.checkSetUniqueness([10, 43, 38, -2]);
    assert.pass("no exception thrown");
    assert.end();
  },
  "Should accept an empty set": function (assert) {
    Thrift.checkSetUniqueness([]);
    assert.pass("no exception thrown");
    assert.end();
  },
  "Should accept a single-element set": function (assert) {
    Thrift.checkSetUniqueness([42]);
    assert.pass("no exception thrown");
    assert.end();
  },
  "Should accept a unique string set": function (assert) {
    Thrift.checkSetUniqueness(["a", "b", "c"]);
    assert.pass("no exception thrown");
    assert.end();
  },
  "Should accept a unique boolean set": function (assert) {
    Thrift.checkSetUniqueness([true, false]);
    assert.pass("no exception thrown");
    assert.end();
  },
  "Should reject duplicate i8 elements": function (assert) {
    assert.throws(function () {
      Thrift.checkSetUniqueness([10, 43, 38, 38, -2]);
    }, /duplicate element/);
    assert.end();
  },
  "Should reject duplicate i32 elements": function (assert) {
    assert.throws(function () {
      Thrift.checkSetUniqueness([1, 2, 2, 3]);
    }, /duplicate element/);
    assert.end();
  },
  "Should reject all-same elements": function (assert) {
    assert.throws(function () {
      Thrift.checkSetUniqueness([1, 1, 1]);
    }, /duplicate element/);
    assert.end();
  },
  "Should reject duplicate string elements": function (assert) {
    assert.throws(function () {
      Thrift.checkSetUniqueness(["a", "b", "a"]);
    }, /duplicate element/);
    assert.end();
  },
  "Should reject duplicate boolean elements": function (assert) {
    assert.throws(function () {
      Thrift.checkSetUniqueness([true, true]);
    }, /duplicate element/);
    assert.end();
  },
  "Should throw TProtocolException with INVALID_DATA type": function (assert) {
    try {
      Thrift.checkSetUniqueness([1, 1]);
      assert.fail("expected exception was not thrown");
    } catch (e) {
      assert.equal(
        e.type,
        Thrift.TProtocolExceptionType.INVALID_DATA,
        "exception type is INVALID_DATA",
      );
    }
    assert.end();
  },
};

Object.keys(cases).forEach(function (caseName) {
  test(caseName, cases[caseName]);
});
