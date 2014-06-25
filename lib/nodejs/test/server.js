#!/usr/bin/env nodejs

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

var fs = require('fs');
var path = require('path');
var thrift = require('thrift');
var ThriftTest = require('./gen-nodejs/ThriftTest');
var ThriftTestHandler = require('./test_handler').ThriftTestHandler;
var ThriftTestHandlerPromise = require('./test_handler_promise').ThriftTestHandler;

var program = require('commander');

program
  .option('-p, --protocol <protocol>', 'Set thift protocol (binary|json) [protocol]')
  .option('-t, --transport <transport>', 'Set thift transport (buffered|framed) [transport]')
  .option('--port <port>', 'Set thift server port')
  .option('--ssl', 'use ssl transport')
  .option('--promise', 'test with promise style functions')
  .parse(process.argv);

var transport =  thrift.TBufferedTransport;
if (program.transport === "framed") {
  transport = thrift.TFramedTransport;
} 

var protocol = thrift.TBinaryProtocol;
if (program.protocol === "json") {
  protocol = thrift.TJSONProtocol;
} else if (program.protocol === "compact") {
  protocol = thrift.TCompactProtocol;
} 

var port = 9090;
if (String(program.port) === "undefined"){
} else {
  port = program.port;
}

var handler = ThriftTestHandler;
if (program.promise) {
  handler = ThriftTestHandlerPromise;
} 

var options = {
  protocol: protocol,
  transport: transport
};
if (program.ssl) {
  options.tls = {
    key: fs.readFileSync(path.resolve(__dirname, 'server.key')),
    cert: fs.readFileSync(path.resolve(__dirname, 'server.crt'))
  };
}
thrift.createServer(ThriftTest, handler, options).listen(port);

