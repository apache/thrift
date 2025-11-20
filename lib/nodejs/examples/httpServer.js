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

var thrift = require("thrift");
var helloSvc = require("./gen-nodejs/HelloSvc");

//ServiceHandler: Implement the hello service
var helloHandler = {
  hello_func: function (result) {
    console.log("Received Hello call");
    result(null, "Hello from Node.js");
  },
};

//ServiceOptions: The I/O stack for the service
var helloSvcOpt = {
  handler: helloHandler,
  processor: helloSvc,
  protocol: thrift.TJSONProtocol,
  transport: thrift.TBufferedTransport,
};

//ServerOptions: Define server features
var serverOpt = {
  services: {
    "/hello": helloSvcOpt,
  },
};

//Create and start the web server
var port = 9090;
thrift.createWebServer(serverOpt).listen(port);
console.log("Http/Thrift Server running on port: " + port);
