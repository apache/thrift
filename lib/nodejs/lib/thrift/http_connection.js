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
var thrift = require('./thrift');
var ttransport = require('./transport');
var tprotocol = require('./protocol');

var http = require('http');

var HttpConnection = exports.HttpConnection = function(host, port, options) {
  //Set configuration
  var self = this;
  this.options = options || {};
  this.host = host;
  this.port = port;
  this.transport = this.options.transport || ttransport.TBufferedTransport;
  this.protocol = this.options.protocol || tprotocol.TBinaryProtocol;

  //Prepare Node.js options
  this.nodeOptions = {
    host: this.host,
    port: this.port || 80,
    path: this.options.path || '/',
    method: 'POST',
    headers: this.options.headers || {},
    tls: options.tls || {},
  };

  //The sequence map is used to map seqIDs back to the 
  //  calling client in multiplexed scenarios
  this.seqId2Service = {};

  function decodeCallback(transport_with_data) {
    var proto = new self.protocol(transport_with_data);
    try {
      while (true) {
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
        var service_name = self.seqId2Service[header.rseqid];
        if (service_name) {
          client = self.client[service_name];
          delete self.seqId2Service[header.rseqid];
        }
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
        if(client['recv_' + header.fname]) {
          client['recv_' + header.fname](proto, header.mtype, dummy_seqid);
        } else {
          delete client._reqs[dummy_seqid];
          throw new thrift.TApplicationException(thrift.TApplicationExceptionType.WRONG_METHOD_NAME,
                             "Received a response to an unknown RPC function");
        }
      }
    }
    catch (e) {
      if (e instanceof ttransport.InputBufferUnderrunError) {
        transport_with_data.rollbackPosition();
      } else {
        throw e;
      }
    }
  };
          
      
  //Response handler
  //////////////////////////////////////////////////
  this.responseCallback = function(response) {
    var data = [];
    var dataLen = 0;

    response.on('error', function (err) {
      console.log("Error in response: " + err); 
    });

    response.on('data', function (chunk) {
      data.push(chunk); 
      dataLen += chunk.length; 
    });

    response.on('end', function(){
      var buf = new Buffer(dataLen); 
      for (var i=0, len=data.length, pos=0; i<len; i++) { 
        data[i].copy(buf, pos); 
        pos += data[i].length; 
      }
      //Get thre receiver function for the transport and 
      //  call it with the buffer
      self.transport.receiver(decodeCallback)(buf);
    });
  };
};

HttpConnection.prototype.write = function(data) {
  var req = http.request(this.nodeOptions, this.responseCallback);

  req.on('error', function(e) {
    throw new thrift.TApplicationException(thrift.TApplicationExceptionType.UNKNOWN,
                                           "Request failed");
  });

  req.write(data);
  req.end();
};

exports.createHttpConnection = function(host, port, options) {
  return new HttpConnection(host, port, options);
};

exports.createHttpClient = function(cls, httpConnection) {
  if (cls.Client) {
    cls = cls.Client;
  }
  return httpConnection.client = 
    new cls(new httpConnection.transport(undefined, function(buf) {httpConnection.write(buf);}), 
            httpConnection.protocol);
};

