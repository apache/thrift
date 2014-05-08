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
var thrift = require('thrift');

var ThriftTest = require('./gen-nodejs/ThriftTest'),
  SecondService = require('./gen-nodejs/SecondService'),
  ttypes = require('./gen-nodejs/ThriftTest_types');

var fs = require("fs");
var path = require("path");

var program = require('commander');

program
  .option('-p, --protocol <protocol>', 'Set thift protocol (binary|json) [protocol]')
  .option('-t, --transport <transport>', 'Set thift transport (buffered|framed) [transport]')
  .option('--ssl', 'use ssl transport')
  .parse(process.argv);

var protocol = thrift.TBinaryProtocol;
if (program.protocol === "json") {
  protocol = thrift.TJSONProtocol;
}

var transport =  thrift.TBufferedTransport;
if (program.transport === "framed") {
  transport = thrift.TFramedTransport;
}

var ThriftTestHandler = require("./test_handler").ThriftTestHandler;

var SecondServiceHandler = {
  secondtestString: function(thing, result) {
    console.log('testString(\'' + thing + '\')');
    result(null, thing);
  }
};

var processor = new thrift.MultiplexedProcessor();

processor.registerProcessor(
  "ThriftTest",
  new ThriftTest.Processor(ThriftTestHandler));

processor.registerProcessor(
  "SecondService",
  new SecondService.Processor(SecondServiceHandler));

var options = {
  transport: transport,
  protocol: protocol
};

if (program.ssl) {
  //ssl options
  options.tls = {
    key: fs.readFileSync(path.resolve(__dirname, 'server.key')),
    cert: fs.readFileSync(path.resolve(__dirname, 'server.crt'))
  };
}

thrift.createMultiplexServer(processor, options).listen(9090);
