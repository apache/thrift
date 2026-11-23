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

import thrift = require("thrift");
import { program } from 'commander';
import ThriftTest = require('./gen-nodejs/ThriftTest');
import test_handler = require('./test_handler');


program
  .option('--port <port>', 'Set thrift server port', (v) => parseInt(v, 10), 9090)
  .option('--promise', 'test with promise style functions')
  .option('--protocol <protocol>', '"Set thrift protocol (binary) [protocol]"')
  .option('--transport <transport>', '"Set thrift transport (buffered) [transport]"')
  .parse(process.argv);

var opts = program.opts();
var port: number = opts.port;

var options: thrift.ServerOptions = {
  transport: thrift.TBufferedTransport,
  protocol: thrift.TBinaryProtocol,
};

var server: thrift.Server;
if (opts.promise) {
  server = thrift.createServer(ThriftTest.Processor, new test_handler.AsyncThriftTestHandler(), options);
} else {
  server = thrift.createServer(
    ThriftTest.Processor,
    new test_handler.SyncThriftTestHandler(),
    options,
  );
}
server.listen(port);
