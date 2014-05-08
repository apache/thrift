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
    SecondService = require('./gen-nodejs/SecondService'),
    ttypes = require('./gen-nodejs/ThriftTest_types');
var ThriftTestDriver = require('./thrift_test_driver').ThriftTestDriver;

var program = require('commander');

program
  .option('-p, --protocol <protocol>', 'Set thift protocol (binary|json) [protocol]')
  .option('-t, --transport <transport>', 'Set thift transport (buffered|framed) [transport]')
  .option('--ssl', 'use ssl transport')
  .parse(process.argv);

var transport =  thrift.TBufferedTransport;
if (program.transport === "framed") {
  transport = thrift.TFramedTransport;
}

var protocol = thrift.TBinaryProtocol;
if (program.protocol === "json") {
  protocol = thrift.TJSONProtocol;
} 

var options = {
  transport: transport,
  protocol: protocol
};

var connection;
if (program.ssl) {
  options.rejectUnauthorized = false;
  connection = thrift.createSSLConnection('localhost', 9090, options);
} else {
  connection = thrift.createConnection('localhost', 9090, options);
}

var mp = new thrift.Multiplexer();

client = mp.createClient("ThriftTest", ThriftTest, connection);
secondclient = mp.createClient("SecondService", SecondService, connection);

connection.on('error', function(err) {
    assert(false, err);
});

connection.on('connect', function() {
  secondclient.secondtestString("Test", function(err, response) {
    assert(!err);
    assert.equal("Test", response);
  });

  ThriftTestDriver(client, function (status) {
    console.log(status);
    connection.end();
  });
});

// to make it also run on expresso
exports.expressoTest = function() {};
