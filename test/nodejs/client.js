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

var ThriftTest = require('./gen-nodejs/ThriftTest'),
    ttypes = require('./gen-nodejs/ThriftTest_types');

var connection = thrift.createConnection('localhost', 9090),
    client = thrift.createClient(ThriftTest, connection);

var tfailed = 0;
var tpassed = 0;

function failed(err) {
  console.trace(err);
	return tfailed++;
}
function passed() {
	return tpassed++;
}

connection.on('error', function(err) {
  failed(err);
});

console.time("Tests completed in");

client.testVoid(function(err, response) {
  if (err) { return failed(err); }
  console.log("testVoid() = ", response);
	passed();
});

client.testString("Test", function(err, response) {
  if (err) { return failed(err); }
  console.log("testString('Test') = ", response);
	passed();
});

client.testByte(1, function(err, response) {
  if (err) { return failed(err); }
  console.log("testByte(1) = ", response);
	passed();
});

client.testI32(-1, function(err, response) {
  if (err) { return failed(err); }
  console.log("testI32(-1) = ", response);
	passed();
});

client.testI64(5, function(err, response) {
  if (err) { return failed(err); }
  console.log("testI64(5) = ", response);
	passed();
});

client.testI64(-5, function(err, response) {
  if (err) { return failed(err); }
  console.log("testI64(-5) = ", response);
	passed();
});

client.testI64(-34359738368, function(err, response) {
  if (err) { return failed(err); }
  console.log("testI64(-34359738368) = ", response);
	passed();
});

client.testDouble(-5.2098523, function(err, response) {
  if (err) { return failed(err); }
  console.log("testDouble(-5.2098523) = ", response);
	passed();
});

var out = new ttypes.Xtruct({
	string_thing: 'Zero',
	byte_thing: 1,
	i32_thing: -3,
	i64_thing: 1000000
});
client.testStruct(out, function(err, response) {
  if (err) { return failed(err); }
  console.log("testStruct(", out, ") = \n", response);
	passed();
});

var out2 = new ttypes.Xtruct2();
out2.byte_thing = 1;
out2.struct_thing = out;
out2.i32_thing = 5;
client.testNest(out2, function(err, response) {
  if (err) { return failed(err); }
  console.log("testNest(", out2, ") = \n", response);
	passed();
});

var mapout = {};
for (var i = 0; i < 5; ++i) {
  mapout[i] = i-10;
}
client.testMap(mapout, function(err, response) {
  if (err) { return failed(err); }
  console.log("testMap(", mapout, ") = \n", response);
	passed();
});

/*
 * TODO: testSet, testList, testEnum, testTypedef, testMapMap, testInsanity
 */


client.testException('ApplicationException', function(err, response) {
  console.log("testException('ApplicationException') = ", err);
  if (response) { return failed(response); }
	passed();
});

client.testException('Xception', function(err, response) {
  console.log("testException('Xception') = ", err);
  if (response) { return failed(response); }
	passed();
});

client.testException('success', function(err, response) {
  if (err) { return failed(err); }
  console.log("testException('success') = ", response);
	passed();
});

setTimeout(function(){
  console.timeEnd("Tests completed in");
	console.log(tpassed + " passed, " + tfailed + " failed");
	connection.end();
}, 200);
