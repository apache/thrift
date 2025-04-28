#!/usr/bin/env node

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

import assert from "assert";
import thrift from "thrift";
import helpers from "./helpers.js";

const ThriftTest = await import(
  `./${helpers.genPath}/ThriftTest.${helpers.moduleExt}`
);
import { ThriftTestDriver, ThriftTestDriverPromise } from "./test_driver.mjs";
const SecondService = await import(
  `./${helpers.genPath}/SecondService.${helpers.moduleExt}`
);

import { program } from "commander";

program
  .option(
    "-p, --protocol <protocol>",
    "Set thrift protocol (binary|compact|json) [protocol]",
  )
  .option(
    "-t, --transport <transport>",
    "Set thrift transport (buffered|framed|http) [transport]",
  )
  .option("--port <port>", "Set thrift server port number to connect", 9090)
  .option("--host <host>", "Set thrift server host to connect", "localhost")
  .option(
    "--domain-socket <path>",
    "Set thrift server unix domain socket to connect",
  )
  .option("--ssl", "use SSL transport")
  .option("--callback", "test with callback style functions")
  .option(
    "--type <type>",
    "Select server type (http|multiplex|tcp|websocket)",
    "tcp",
  )
  .option("--es6", "Use es6 code")
  .option("--es5", "Use es5 code")
  .option("--esm", "Use es modules")
  .parse(process.argv);

const opts = program.opts();
const host = opts.host;
const port = opts.port;
const domainSocket = opts.domainSocket;
const ssl = opts.ssl;
let type = opts.type;

/* for compatibility with cross test invocation for http transport testing */
if (opts.transport === "http") {
  opts.transport = "buffered";
  type = "http";
}

if (opts.transport === "websocket") {
  opts.transport = "buffered";
  type = "websocket";
}

const options = {
  transport: helpers.transports[opts.transport],
  protocol: helpers.protocols[opts.protocol],
};

if (type === "http" || type === "websocket") {
  options.path = "/test";
}

if (type === "http") {
  options.headers = { Connection: "close" };
}

if (ssl) {
  if (type === "tcp" || type === "multiplex") {
    options.rejectUnauthorized = false;
  } else if (type === "http") {
    options.nodeOptions = { rejectUnauthorized: false };
    options.https = true;
  } else if (type === "websocket") {
    options.wsOptions = { rejectUnauthorized: false };
    options.secure = true;
  }
}

let connection;
let client;
const testDriver = opts.callback ? ThriftTestDriver : ThriftTestDriverPromise;
if (helpers.ecmaMode === "es6" && opts.callback) {
  console.log("ES6 does not support callback style");
  process.exit(0);
}

if (type === "tcp" || type === "multiplex") {
  if (domainSocket) {
    connection = thrift.createUDSConnection(domainSocket, options);
  } else {
    connection = ssl
      ? thrift.createSSLConnection(host, port, options)
      : thrift.createConnection(host, port, options);
  }
} else if (type === "http") {
  if (domainSocket) {
    connection = thrift.createHttpUDSConnection(domainSocket, options);
  } else {
    connection = thrift.createHttpConnection(host, port, options);
  }
} else if (type === "websocket") {
  connection = thrift.createWSConnection(host, port, options);
  connection.open();
}

connection.on("error", function (err) {
  assert(false, err);
});

if (type === "tcp") {
  client = thrift.createClient(ThriftTest, connection);
  runTests();
} else if (type === "multiplex") {
  const mp = new thrift.Multiplexer();
  client = mp.createClient("ThriftTest", ThriftTest, connection);
  const secondclient = mp.createClient(
    "SecondService",
    SecondService,
    connection,
  );

  connection.on("connect", function () {
    secondclient.secondtestString("Test", function (err, response) {
      assert(!err);
      assert.equal('testString("Test")', response);
    });

    runTests();
  });
} else if (type === "http") {
  client = thrift.createHttpClient(ThriftTest, connection);
  runTests();
} else if (type === "websocket") {
  client = thrift.createWSClient(ThriftTest, connection);
  runTests();
}

function runTests() {
  testDriver(client, function (status) {
    console.log(status);
    if (type !== "http" && type !== "websocket") {
      connection.end();
    }
    if (type !== "multiplex") {
      process.exit(0);
    }
  });
}

export const expressoTest = function () {};
