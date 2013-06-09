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
var util = require('util'),
    EventEmitter = require("events").EventEmitter,
    net = require('net'),
    ttransport = require('./transport'),
    tprotocol = require('./protocol');

var binary = require('./binary');

var Connection = exports.Connection = function(stream, options) {
  var self = this;
  EventEmitter.call(this);

  this.connection = stream;
  this.options = options || {};
  this.transport = this.options.transport || ttransport.TBufferedTransport;
  this.protocol = this.options.protocol || tprotocol.TBinaryProtocol;
  this.offline_queue = [];
  this.connected = false;

  this.connection.addListener("connect", function() {
    self.connected = true;

    this.setTimeout(self.options.timeout || 0);
    this.setNoDelay();
    this.frameLeft = 0;
    this.framePos = 0;
    this.frame = null;

    self.offline_queue.forEach(function(data) {
      self.connection.write(data);
    });

    self.emit("connect");
  });

  this.connection.addListener("error", function(err) {
    // Only emit the error if no-one else is listening on the connection
    // or if someone is listening on us
    if (self.connection.listeners('error').length === 1
        || self.listeners('error').length > 0) {
      self.emit("error", err)
    }
  });

  // Add a close listener
  this.connection.addListener("close", function() {
    self.emit("close");
  });

  this.connection.addListener("timeout", function() {
    self.emit("timeout");
  });

  this.connection.addListener("data", self.transport.receiver(function(transport_with_data) {
    var message = new self.protocol(transport_with_data);
    try {
      var header = message.readMessageBegin();
      var dummy_seqid = header.rseqid * -1;
      var client = self.client;
      client._reqs[dummy_seqid] = function(err, success){
        transport_with_data.commitPosition();

        var callback = client._reqs[header.rseqid];
        delete client._reqs[header.rseqid];
        if (callback) {
          callback(err, success);
        }
      };
      client['recv_' + header.fname](message, header.mtype, dummy_seqid);
    }
    catch (e) {
      if (e instanceof ttransport.InputBufferUnderrunError) {
        transport_with_data.rollbackPosition();
      }
      else {
        throw e;
      }
    }
  }));
};
util.inherits(Connection, EventEmitter);

Connection.prototype.end = function() {
  this.connection.end();
}

Connection.prototype.write = function(data) {
  if (!this.connected) {
    this.offline_queue.push(data);
    return;
  }
  this.connection.write(data);
}

exports.createConnection = function(host, port, options) {
  var stream = net.createConnection(port, host);
  var connection = new Connection(stream, options);
  connection.host = host;
  connection.port = port;

  return connection;
}

exports.createClient = function(cls, connection) {
  if (cls.Client) {
    cls = cls.Client;
  }
  var client = new cls(new connection.transport(undefined, function(buf) {
    connection.write(buf);
  }), connection.protocol);

  // TODO clean this up
  connection.client = client;

  return client;
}

var child_process = require('child_process');
var StdIOConnection = exports.StdIOConnection = function(command, options) {
  var command_parts = command.split(' ');
  command = command_parts[0];
  var args = command_parts.splice(1,command_parts.length -1);
  var child = this.child = child_process.spawn(command,args);

  var self = this;
  EventEmitter.call(this);

  this._debug = options.debug || false;
  this.connection = child.stdin;
  this.options = options || {};
  this.transport = this.options.transport || ttransport.TBufferedTransport;
  this.protocol = this.options.protocol || tprotocol.TBinaryProtocol;
  this.offline_queue = [];

  if(this._debug === true){
    this.child.stderr.on('data',function(err){
      console.log(err.toString(),'CHILD ERROR');
    });

    this.child.on('exit',function(code,signal){
      console.log(code+':'+signal,'CHILD EXITED');
    });
  }

  this.frameLeft = 0;
  this.framePos = 0;
  this.frame = null;
  this.connected = true;

  self.offline_queue.forEach(function(data) {
      self.connection.write(data);
  });


  this.connection.addListener("error", function(err) {
    self.emit("error", err);
  });

  // Add a close listener
  this.connection.addListener("close", function() {
    self.emit("close");
  });

  child.stdout.addListener("data", self.transport.receiver(function(transport_with_data) {
    var message = new self.protocol(transport_with_data);
    try {
      var header = message.readMessageBegin();
      var dummy_seqid = header.rseqid * -1;
      var client = self.client;
      client._reqs[dummy_seqid] = function(err, success){
        transport_with_data.commitPosition();

        var callback = client._reqs[header.rseqid];
        delete client._reqs[header.rseqid];
        if (callback) {
          callback(err, success);
        }
      };
      client['recv_' + header.fname](message, header.mtype, dummy_seqid);
    }
    catch (e) {
      if (e instanceof ttransport.InputBufferUnderrunError) {
        transport_with_data.rollbackPosition();
      }
      else {
        throw e;
      }
    }
  }));

};

util.inherits(StdIOConnection, EventEmitter);     

StdIOConnection.prototype.end = function() {
  this.connection.end();
}

StdIOConnection.prototype.write = function(data) {
  if (!this.connected) {
    this.offline_queue.push(data);
    return;
  }
  this.connection.write(data);
}
exports.createStdIOConnection = function(command,options){
  return new StdIOConnection(command,options);

};

exports.createStdIOClient = function(cls,connection) {
  if (cls.Client) {
    cls = cls.Client;
  }

  var client = new cls(new connection.transport(undefined, function(buf) {
    connection.write(buf);
  }), connection.protocol);

  // TODO clean this up
  connection.client = client;

  return client;
}
