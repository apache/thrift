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
var util = require('util');
var http = require('http');
var https = require('https');
var EventEmitter = require("events").EventEmitter;
var thrift = require('./thrift');
var ttransport = require('./transport');
var tprotocol = require('./protocol');

// TODO add a logger that is off by default
// should support info/error/debug/etc interface

/**
 * @class
 * @name ConnectOptions
 * @property {string} transport - The Thrift layered transport to use (TBufferedTransport, etc).
 * @property {string} protocol - The Thrift serialization protocol to use (TBinaryProtocol, etc.).
 * @property {string} path - The URL path to POST to (e.g. "/", "/mySvc", "/thrift/quoteSvc", etc.).
 * @property {object} headers - A standard Node.js header hash, an object hash containing key/value 
 *        pairs where the key is the header name string and the value is the header value string.
 * @property {boolean} https - True causes the connection to use https, otherwise http is used.
 * @property {object} nodeOptions - Options passed on to node.
 * @example
 *     //Use a connection that requires ssl/tls, closes the connection after each request,
 *     //  uses the buffered transport layer, uses the JSON protocol and directs RPC traffic
 *     //  to https://thrift.example.com:9090/hello 
 *     var thrift = require('thrift');
 *     var options = {
 *        transport: thrift.TBufferedTransport,
 *        protocol: thrift.TJSONProtocol,
 *        path: "/hello",
 *        headers: {"Connection": "close"},
 *        https: true
 *     };
 *     var con = thrift.createHttpConnection("thrift.example.com", 9090, options);
 *     var client = thrift.createHttpClient(myService, connection);
 *     client.myServiceFunction();
 */

/**
 * Initializes a Thrift HttpConnection instance (use createHttpConnection() rather than 
 *    instantiating directly).
 * @constructor
 * @param {string} host - The host name or IP to connect to.
 * @param {number} port - The TCP port to connect to.
 * @param {ConnectOptions} options - The configuration options to use.
 * @throws {error} Exceptions other than ttransport.InputBufferUnderrunError are rethrown
 * @event {error} The "error" event is fired when a Node.js error event occurs during
 *     request or response processing, in which case the node error is passed on. An "error"
 *     event may also be fired when the connection can not map a response back to the
 *     appropriate client (an internal error), generating a TApplicationException.
 * @classdesc HttpConnection objects provide Thrift end point transport 
 *     semantics implemented over the Node.js http.request() method.
 * @see {@link createHttpConnection}
 */
var HttpConnection = exports.HttpConnection = function(host, port, options) {
  //Initialize the emitter base object
  EventEmitter.call(this);

  //Set configuration
  var self = this;
  this.options = options || {};
  this.host = host;
  this.port = port;
  this.https = this.options.https || false;
  this.transport = this.options.transport || ttransport.TBufferedTransport;
  this.protocol = this.options.protocol || tprotocol.TBinaryProtocol;

  //Prepare Node.js options
  // TODO consider where to put these defaults instead of
  // of a magic configuration object
  // TODO not sure why we put these in both self config and nodeOptions
  this.nodeOptions = {
    host: this.host,
    port: this.port || 80,
    path: this.options.path || '/',
    method: 'POST',
    headers: this.options.headers || {},
    responseType: this.options.responseType || null
  };
  for (var attrname in this.options.nodeOptions) { 
    this.nodeOptions[attrname] = this.options.nodeOptions[attrname]; 
  }
  /*jshint -W069 */
  if (! this.nodeOptions.headers["Connection"]) {
    this.nodeOptions.headers["Connection"] = "keep-alive";
  }
  /*jshint +W069 */

  //The sequence map is used to map seqIDs back to the 
  //  calling client in multiplexed scenarios
  this.seqId2Service = {};

  // gets singleton transport that has data in its buffers
  // flush causes this to decode as much is on the transport
  // as is possible
  function decodeCallback(transport_with_data) {
    // create (another) new protocol for the transport
    // not sure why this is needed since each call to the
    // service client also does `new this.pClass(this.output)`
    // in the send_ method seems like duplication
    var proto = new self.protocol(transport_with_data);
    // TODO hopefully scrap this with better loop flow control
    try {
      // TODO better loop flow control
      // I just can not get behind using exceptions to break this loop
      while (true) {
        // TODO check if this throws
        var header = proto.readMessageBegin();
        var dummy_seqid = header.rseqid * -1;
        var client = self.client;
        //The Multiplexed Protocol stores a hash of seqid to service names
        //  in seqId2Service. If the SeqId is found in the hash we need to
        //  lookup the appropriate client for this call.
        //  The client var is a single client object when not multiplexing, 
        //  when using multiplexing it is a service name keyed hash of client
        //  objects.
        //NOTE: The 2 way interdependencies between protocols, transports,
        //  connections and clients in the Node.js implementation are irregular
        //  and make the implementation difficult to extend and maintain. We 
        //  should bring this stuff inline with typical thrift I/O stack 
        //  operation soon.
        //  --ra
        // I am pretty sure that this does not get called in most cases
        // Since unless you are using multiplex_protocol nothing sets values
        // in to the seqId2Service map --Tom
        var service_name = self.seqId2Service[header.rseqid];
        if (service_name) {
          client = self.client[service_name];
          delete self.seqId2Service[header.rseqid];
        }
        // This replaces the called back of the real seqid in the client's
        // stack with a callback on its negative compliment
        // when called it commits the read head on the transport
        // removes the real seqid and calls the callback if any
        // relies on a slightly gross closure reference to 
        // header.rseqid to know what the callback should be --Tom
        /*jshint -W083 */
        client._reqs[dummy_seqid] = function(err, success){ 
          transport_with_data.commitPosition();
          var clientCallback = client._reqs[header.rseqid];
          delete client._reqs[header.rseqid];
          if (clientCallback) {
            clientCallback(err, success);
          }
        };
        /*jshint +W083 */
        // Call recv_ function in client if it can be found
        if(client['recv_' + header.fname]) {
          client['recv_' + header.fname](proto, header.mtype, dummy_seqid);
        } else {
          delete client._reqs[dummy_seqid];
          // TODO decide if this is correctly emitting or should be associated
          // with a calbback site.
          // This might be the only correct use of self.emit('error')
          // I think this should only happen in the case of multiplexing
          // or something else very weird
          // That said a given http request should be tied to a single
          // callback so we should be able to associate it anyway
          // until we have actual multiplexing
          // This might be something that could be caused by a server error --Tom
          self.emit("error",
                    new thrift.TApplicationException(
                       thrift.TApplicationExceptionType.WRONG_METHOD_NAME,
                       "Received a response to an unknown RPC function"));
        }
      }
    }
    catch (e) {
      if (e instanceof ttransport.InputBufferUnderrunError) {
        // TODO Unclear if this is an error or an expected
        // case in TFramed. We should add logging if it is an error
        // Also unclear what the recovery is from rollback to 0
        transport_with_data.rollbackPosition();
      } else {
        // TODO do not throw in callbacks
        // this should return an error to the callback
        // or if it happens after it should be logged and
        // noop.
        // TODO logging here
        throw e;
      }
    }
  }
          
      
  //Response handler
  //////////////////////////////////////////////////
  // this obviously breaks streaming
  // probably we do not care about that
  // but maybe we do
  // TODO replace with a stream safe buffering implementation
  this.responseCallback = function(response) {
    var data = [];
    var dataLen = 0;

    // TODO callback the error instead of just vomitting
    // all errors at the singleton client
    // this will requiring have access to the callback 
    // or at least its seqid
    response.on('error', function (e) {
      self.emit("error", e);
    });

    // When running directly under node, chunk will be a buffer,
    // however, when running in a Browser (e.g. Browserify), chunk
    // will be a string or an ArrayBuffer.
    response.on('data', function (chunk) {
      if ((typeof chunk == 'string') ||
          (Object.prototype.toString.call(chunk) == '[object Uint8Array]')) {
        // Wrap ArrayBuffer/string in a Buffer so data[i].copy will work
        data.push(new Buffer(chunk));
      } else {
        data.push(chunk);
      }
      dataLen += chunk.length; 
    });

    response.on('end', function(){
      var buf = new Buffer(dataLen); 
      for (var i=0, len=data.length, pos=0; i<len; i++) { 
        data[i].copy(buf, pos); 
        pos += data[i].length; 
      }
      //Get the receiver function for the transport and
      //  call it with the buffer
      //  TODO ensure that reciever can not throw
      //  or if it does catch and callback those errors
      self.transport.receiver(decodeCallback)(buf);
    });
  };
};
util.inherits(HttpConnection, EventEmitter);

/**
 * Writes Thrift message data to the connection
 * @param {Buffer} data - A Node.js Buffer containing the data to write
 * @returns {void} No return value.
 * @event {error} the "error" event is raised upon request failure passing the 
 *     Node.js error object to the listener.  
 */
HttpConnection.prototype.write = function(data) {
  var self = this;
  // TODO if we are framing or multiplexing then we probably
  // want to do Multipart here. See also pooling
  self.nodeOptions.headers["Content-length"] = data.length;
  // The node global pooling is ok for now but does not support
  // keep-alive
  // TODO improve http pooling with something like lb_pool
  var req = (self.https) ?
      https.request(self.nodeOptions, self.responseCallback) :
      http.request(self.nodeOptions, self.responseCallback);
  // TODO return HTTP errors back to the callback site
  // This will require getting access to the callback
  // Either via having the seqid or the callback itself
  req.on('error', function(err) {
    self.emit("error", err);
  });  
  req.write(data);
  req.end();
};

/**
 * Creates a new HttpConnection object, used by Thrift clients to connect
 *    to Thrift HTTP based servers.
 * @param {string} host - The host name or IP to connect to.
 * @param {number} port - The TCP port to connect to.
 * @param {ConnectOptions} options - The configuration options to use.
 * @returns {HttpConnection} The connection object.
 * @see {@link ConnectOptions}
 */
exports.createHttpConnection = function(host, port, options) {
  return new HttpConnection(host, port, options);
};

/**
 * Creates a new client object for the specified Thrift service.
 * @param {object} cls - The module containing the service client
 * @param {HttpConnection} httpConnection - The connection to use.
 * @returns {object} The client object.
 * @see {@link createHttpConnection}
 */
exports.createHttpClient = function(cls, httpConnection) {
  // TODO validate required options and throw otherwise
  if (cls.Client) {
    cls = cls.Client;
  }
  // TODO detangle these initialization calls
  // creating http "client" requires
  //   - new service client instance
  //
  // New service client instance requires
  //   - new transport instance
  //   - protocol class reference
  //
  // New transport instance requires
  //   - Buffer to use (or none)
  //   - Callback to call on flush
  httpConnection.client = 
    new cls(new httpConnection.transport(undefined, function(buf) {httpConnection.write(buf);}), 
            httpConnection.protocol);
  return httpConnection.client;
};

