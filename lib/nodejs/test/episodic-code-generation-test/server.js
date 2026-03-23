#!/usr/bin/env node

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

const thrift = require("../../lib/thrift");
const { program } = require("commander");

program
  .option("--port <port>", "Set the thrift server port", 9090)
  .option("--base <base>", "Set the base: 'pure', 'base' or 'extend'", "pure")
  .parse(process.argv);

const ServiceBase = require("types-package/first-episode/BaseService");
const ServicePure = require("./gen-2/second-episode/gen-nodejs/Service");
const ServiceExtended = require("./gen-2/second-episode/gen-nodejs/ExtendedService");
const Types = require("types-package/first-episode/Types_types");

const opts = program.opts();
const port = opts.port;

let Service;
if (opts.base === "pure") {
  Service = ServicePure;
} else if (opts.base === "base") {
  Service = ServiceBase;
} else if (opts.base === "extend") {
  Service = ServiceExtended;
}

const options = {
  transport: thrift.TBufferedTransport,
  protocol: thrift.TJSONProtocol,
};

const ServiceHandler = {
  testEpisode: function (receivedType1Object) {
    const type1Object = new Types.Type1();
    type1Object.number = receivedType1Object.number + 1;
    type1Object.message =
      receivedType1Object.message + " [Hello from the server]";
    return type1Object;
  },
  testEpisodeExtend: function (receivedType1Object) {
    const type1Object = new Types.Type1();
    type1Object.number = receivedType1Object.number + 1;
    type1Object.message =
      receivedType1Object.message + " [Hello from the extended server]";
    return type1Object;
  },
};

const server = thrift.createServer(Service, ServiceHandler, options);
server.listen(port);
