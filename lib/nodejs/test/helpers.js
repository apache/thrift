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

"use strict";
const thrift = require("thrift");

module.exports.transports = {
  buffered: thrift.TBufferedTransport,
  framed: thrift.TFramedTransport,
};

module.exports.protocols = {
  json: thrift.TJSONProtocol,
  binary: thrift.TBinaryProtocol,
  compact: thrift.TCompactProtocol,
  header: thrift.THeaderProtocol,
};

const variant = (function () {
  if (process.argv.includes("--es6")) {
    return "es6";
  } else if (process.argv.includes("--esm")) {
    return "esm";
  } else {
    return "es5";
  }
})();

module.exports.ecmaMode = ["esm", "es6"].includes(variant) ? "es6" : "es5";
const genPath = (module.exports.genPath = (function () {
  if (variant == "es5") {
    return "gen-nodejs";
  } else {
    return `gen-nodejs-${variant}`;
  }
})());

const moduleExt = (module.exports.moduleExt = variant === "esm" ? "mjs" : "js");

/**
 * Imports a types module, correctly handling the differences in esm and commonjs
 */
module.exports.importTypes = async function (filename) {
  const typesModule = await import(`./${genPath}/${filename}.${moduleExt}`);

  if (variant === "esm") {
    return typesModule;
  } else {
    return typesModule.default;
  }
};
