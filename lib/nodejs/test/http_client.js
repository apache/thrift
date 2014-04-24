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

//This is the client side test for the standard Apache Thrift
//"ThriftTest" suite. This client will test any protocol/transport
//combination specified on the command line.

var fs = require('fs');
var assert = require('assert');
var thrift = require('thrift');
var ThriftTest = require('./gen-nodejs/ThriftTest');
var ThriftTestDriver = require('./thrift_test_driver').ThriftTestDriver;
var ThriftTestDriverPromise = require('./thrift_test_driver_promise').ThriftTestDriver;

var program = require('commander');

program
  .option('-p, --protocol <protocol>', 'Set thrift protocol (binary|json) [protocol]')
  .option('-t, --transport <transport>', 'Set thrift transport (buffered|framed) [transport]')
  .option('--ssl', 'use SSL transport')
  .option('--promise', 'test with promise style functions')
  .parse(process.argv);


var protocol = thrift.TBinaryProtocol;
if (program.protocol === "json") {
  protocol = thrift.TJSONProtocol;
} 

var transport =  thrift.TBufferedTransport;
if (program.transport === "framed") {
  transport = thrift.TFramedTransport;
}

var options = {
   transport: transport,
   protocol: protocol,
   headers: {"Connection": "close"},
   path: "/test"
};

if (program.ssl) {
  options.nodeOptions = { rejectUnauthorized: false };
  options.https = true;
} 

var connection = thrift.createHttpConnection("localhost", 9090, options);

var client = thrift.createHttpClient(ThriftTest, connection);

connection.on('error', function(err) {
  assert(false, err);
});

var testDriver = ThriftTestDriver;
if (program.promise) {
  console.log("    --Testing promise style client");
  testDriver = ThriftTestDriverPromise;
} 
testDriver(client, function (status) {
  console.log(status);
  process.exit(0);
});

// to make it also run on expresso
exports.expressoTest = function() {};
