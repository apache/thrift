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
var thrift = require('thrift');
var assert = require('assert');

var ThriftTest = require('./gen-nodejs/ThriftTest'),
    ttypes = require('./gen-nodejs/ThriftTest_types');

var connection = thrift.createConnection('localhost', 9090),
    client = thrift.createClient(ThriftTest, connection);

connection.on('error', function(err) {
  assert(false, err);
});



client.testVoid(function(err, response) {
  assert( ! err);
  assert.equal(undefined, response);
});

client.testString("Test", function(err, response) {
  assert( ! err);
  assert.equal("Test", response);
});

client.testByte(1, function(err, response) {
  assert( ! err);
  assert.equal(1, response);
});

client.testI32(-1, function(err, response) {
  assert( ! err);
  assert.equal(-1, response);
});

client.testI64(5, function(err, response) {
  assert( ! err);
  assert.equal(5, response);
});

client.testI64(-5, function(err, response) {
  assert( ! err);
  assert.equal(-5, response);
});

client.testI64(-34359738368, function(err, response) {
  assert( ! err);
  assert.equal(-34359738368, response);
});

client.testDouble(-5.2098523, function(err, response) {
  assert( ! err);
  assert.equal(-5.2098523, response);
});

var out = new ttypes.Xtruct({
  string_thing: 'Zero',
  byte_thing: 1,
  i32_thing: -3,
  //i64_thing: 1000000
});
client.testStruct(out, function(err, response) {
  assert( ! err);
  assert.deepEqual(out, response);
});

var out2 = new ttypes.Xtruct2();
out2.byte_thing = 1;
out2.struct_thing = out;
out2.i32_thing = 5;
client.testNest(out2, function(err, response) {
  assert( ! err);
  assert.deepEqual(out2, response);
});

var mapout = {};
for (var i = 0; i < 5; ++i) {
  mapout[i] = i-10;
}
client.testMap(mapout, function(err, response) {
  assert( ! err);
  assert.deepEqual(mapout, response);
});

/*
 * TODO: testSet, testList, testEnum, testTypedef, testMapMap, testInsanity
 */


client.testException('ApplicationException', function(err, response) {
  //assert.equal('ApplicationException', err);
  assert( ! response);
});

client.testException('Xception', function(err, response) {
  assert.equal('Xception', err.message);
  assert( ! response);
});

client.testException('success', function(err, response) {
  assert( ! err);
  //assert.equal('success', response);
});


setTimeout(function() {
  console.log("Server successfully tested!");
  connection.end();
}, 200);

// to make it also run on expresso
exports.expressoTest = function() {};

