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
var ThriftTransports = require('thrift/transport');
var ThriftProtocols = require('thrift/protocol');
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

var protocol = undefined;
var transport =  undefined;
var testDriver = undefined;

if (program.protocol === "binary") {
  protocol = ThriftProtocols.TBinaryProtocol;
} else if (program.protocol === "json") {
  protocol = ThriftProtocols.TJSONProtocol;
} else {
  //default
  protocol = ThriftProtocols.TBinaryProtocol;
}

if (program.transport === "framed") {
  transport = ThriftTransports.TFramedTransport;
} else if (program.transport === "buffered") {
  transport = ThriftTransports.TBufferedTransport;
} else {
  //default
  transport = ThriftTransports.TBufferedTransport;
}

if (program.promise) {
  testDriver = ThriftTestDriverPromise;
} else {
  testDriver = ThriftTestDriver;
}

var options = {
  transport: transport,
  protocol: protocol
};

var connection = undefined;

if (program.ssl) {
  options.rejectUnauthorized = false;
  connection = thrift.createSSLConnection('localhost', 9090, options);
} else {
  connection = thrift.createConnection('localhost', 9090, options);
}

var client = thrift.createClient(ThriftTest, connection);

connection.on('error', function(err) {
  assert(false, err);
});

testDriver(client, function (status) {
  console.log(status);
  connection.end();
});

// to make it also run on expresso
exports.expressoTest = function() {};
