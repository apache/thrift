/**

  This is a standalone deserialize example if you just want to deserialize
  thrift decoupled from cassandra server

  1.  acquire thrift template specification files from who ever built it (eg: data.thrift)

  2.  Install thrift on local machine (ie, via "brew install thrift")

  3.  generate thrift clients for nodejs using template specification files (#1)
      thrift --gen js:node schema/data.thrift

      This creates creates gen-node.js directory containing data_types.js

  4.  Inside data_types.js is a class you will want to instanciate.  Find this class name and plug 
      it into the example code below  (ie, "SpecificClassName")
 */

function parseThrift(thriftEncodedData, callback) {
  var thrift = require('thrift');
  var transport = new thrift.TFramedTransport(thriftEncodedData);
  var protocol  = new thrift.TBinaryProtocol(transport);

  var clientClass = require('../gen-nodejs/data_types').SpecificClassName;
  var client = new clientClass();
  client.read(protocol);
  callback(null, client);
}