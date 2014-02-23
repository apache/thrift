/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

//Server test for the following I/O stack:
//    TBinaryProtocol
//    TFramedTransport
//    TSocket

var fs = require('fs');
var path = require('path');
var thrift = require('thrift');
var ThriftTransports = require('thrift/transport');
var ThriftProtocols = require('thrift/protocol');
var ThriftTest = require('./gen-nodejs/ThriftTest');
var ThriftTestHandler = require('./test_handler').ThriftTestHandler;
var ThriftTestHandlerPromise = require('./test_handler_promise').ThriftTestHandler;


var program = require('commander');

program
  .option('-p, --protocol <protocol>', 'Set thift protocol (binary|json) [protocol]')
  .option('-t, --transport <transport>', 'Set thift transport (buffered|framed) [transport]')
  .option('--ssl', 'use ssl transport')
  .option('--promise', 'test with promise style functions')
  .parse(process.argv);

var protocol = undefined;
var transport =  undefined;
var handler = undefined;

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
  handler = ThriftTestHandlerPromise;
} else {
  handler = ThriftTestHandler;
}

var options = {
  protocol: protocol,
  transport: transport
};

if (program.ssl) {
  //ssl options
  options.key = fs.readFileSync(path.resolve(__dirname, 'server.key'));
  options.cert = fs.readFileSync(path.resolve(__dirname, 'server.crt'));
  thrift.createSSLServer(ThriftTest, handler, options).listen(9090);

} else {
  //default
  thrift.createServer(ThriftTest, handler, options).listen(9090);
}
