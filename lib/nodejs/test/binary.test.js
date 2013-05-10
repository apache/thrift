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

var testCase = require('nodeunit').testCase;
var binary = require('thrift/binary');

module.exports = testCase({
  "Should read signed byte": function(test){
    test.equal(1, binary.readByte([0x01]));
    test.equal(-1, binary.readByte([0xFF]));
    
    test.equal(127, binary.readByte([0x7F]));
    test.equal(-128, binary.readByte([0x80]));
    test.done();
  },
  "Should write byte": function(test){
  	//Protocol simply writes to the buffer. Nothing to test.. yet.
  	test.ok(true);
  	test.done();
  },
  "Should read I16": function(test) {
    test.equal(0, binary.readI16([0x00, 0x00]));
    test.equal(1, binary.readI16([0x00, 0x01]));
    test.equal(-1, binary.readI16([0xff, 0xff]));

    // Min I16
    test.equal(-32768, binary.readI16([0x80, 0x00]));
    // Max I16
    test.equal(32767, binary.readI16([0x7f, 0xff]));
    test.done();
  },

  "Should write I16": function(test) {
    test.deepEqual([0x00, 0x00], binary.writeI16([], 0));
    test.deepEqual([0x00, 0x01], binary.writeI16([], 1));
    test.deepEqual([0xff, 0xff], binary.writeI16([], -1));

    // Min I16
    test.deepEqual([0x80, 0x00], binary.writeI16([], -32768));
    // Max I16
    test.deepEqual([0x7f, 0xff], binary.writeI16([], 32767));
    test.done();
  },

  "Should read I32": function(test) {
    test.equal(0, binary.readI32([0x00, 0x00, 0x00, 0x00]));
    test.equal(1, binary.readI32([0x00, 0x00, 0x00, 0x01]));
    test.equal(-1, binary.readI32([0xff, 0xff, 0xff, 0xff]));

    // Min I32
    test.equal(-2147483648, binary.readI32([0x80, 0x00, 0x00, 0x00]));
    // Max I32
    test.equal(2147483647, binary.readI32([0x7f, 0xff, 0xff, 0xff]));
    test.done();
  },

  "Should write I32": function(test) {
    test.deepEqual([0x00, 0x00, 0x00, 0x00], binary.writeI32([], 0));
    test.deepEqual([0x00, 0x00, 0x00, 0x01], binary.writeI32([], 1));
    test.deepEqual([0xff, 0xff, 0xff, 0xff], binary.writeI32([], -1));

    // Min I32
    test.deepEqual([0x80, 0x00, 0x00, 0x00], binary.writeI32([], -2147483648));
    // Max I32
    test.deepEqual([0x7f, 0xff, 0xff, 0xff], binary.writeI32([], 2147483647));
 	test.done();
  },

  "Should read doubles": function(test) {
    test.equal(0, binary.readDouble([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
    test.equal(0, binary.readDouble([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
    test.equal(1, binary.readDouble([0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
    test.equal(2, binary.readDouble([0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
    test.equal(-2, binary.readDouble([0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))

    test.equal(Math.PI, binary.readDouble([0x40, 0x9, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18]))

    test.equal(Infinity, binary.readDouble([0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
    test.equal(-Infinity, binary.readDouble([0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))

    test.ok(isNaN(binary.readDouble([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])))

    test.equal(1/3, binary.readDouble([0x3f, 0xd5, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55]))

    // Min subnormal positive double
    test.equal(4.9406564584124654e-324, binary.readDouble([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]))
    // Min normal positive double
    test.equal(2.2250738585072014e-308, binary.readDouble([0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]))
    // Max positive double
    test.equal(1.7976931348623157e308, binary.readDouble([0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]))
  	test.done();
  },

  "Should write doubles": function(test) {
    test.deepEqual([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], 0));
    test.deepEqual([0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], 1));
    test.deepEqual([0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], 2));
    test.deepEqual([0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], -2));

    test.deepEqual([0x40, 0x9, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18], binary.writeDouble([], Math.PI));

    test.deepEqual([0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], Infinity));
    test.deepEqual([0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], -Infinity));

    test.deepEqual([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], NaN));

    test.deepEqual([0x3f, 0xd5, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55], binary.writeDouble([], 1/3));

    // Min subnormal positive double
    test.deepEqual([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], binary.writeDouble([], 4.9406564584124654e-324)); 
    // Min normal positive double
    test.deepEqual([0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], binary.writeDouble([], 2.2250738585072014e-308)); 
    // Max positive double
    test.deepEqual([0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], binary.writeDouble([], 1.7976931348623157e308)); 
  	test.done();
  }
});