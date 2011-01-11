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
var sys = require('sys'),
    EventEmitter = require("events").EventEmitter,
    net = require('net'),
    TMemoryBuffer = require('./transport').TMemoryBuffer,
    TBinaryProtocol = require('./protocol').TBinaryProtocol;

var BinaryParser = require('./binary_parser').BinaryParser;
BinaryParser.bigEndian = true;

var int32FramedReceiver = exports.int32FramedReceiver = function (callback) {
  var frameLeft = 0,
      framePos = 0,
      frame = null;

  return function(data) {
    // var buf = new Buffer(data, 'binary');
    // console.log(buf);
    // framed transport
    while (data.length) {
      if (frameLeft === 0) {
        // TODO assumes we have all 4 bytes
        if (data.length < 4) {
          throw Error("Not enough bytes");
        }
        frameLeft = BinaryParser.toInt(data.slice(0,4));
        frame = new Buffer(frameLeft);
        framePos = 0;
        data = data.slice(4, data.length);
      }

      if (data.length >= frameLeft) {
        data.copy(frame, framePos, 0, frameLeft);
        data = data.slice(frameLeft, data.length);

        frameLeft = 0;
        callback(frame);
      } else if (data.length) {
        data.copy(frame, framePos, 0, data.length);
        frameLeft -= data.length;
        framePos += data.length;
        data = data.slice(data.length, data.length);
      }
    }
  };
}

var Connection = exports.Connection = function(stream, options) {
  var self = this;
  EventEmitter.call(this);

  this.connection = stream;
  this.offline_queue = [];
  this.options = options || {};
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
    self.emit("error", err);
  });

  // Add a close listener
  this.connection.addListener("close", function() {
    self.emit("close");
  });

  this.connection.addListener("timeout", function() {
    self.emit("timeout");
  });

  this.connection.addListener("data", int32FramedReceiver(function(data) {
    // console.log(typeof(data));
    var input = new TBinaryProtocol(new TMemoryBuffer(data));
    var r = input.readMessageBegin();
    // console.log(r);
    self.client['recv_' + r.fname](input, r.mtype, r.rseqid);
    // self.emit("data", data);
  }));
}
sys.inherits(Connection, EventEmitter);

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
  var client = new cls(new TMemoryBuffer(undefined, function(buf) {
    connection.write(buf);
  }), TBinaryProtocol);

  // TODO clean this up
  connection.client = client;

  return client;
}

Connection.prototype.end = function() {
  this.connection.end();
}

Connection.prototype.write = function(buf) {
  // TODO: optimize this better, allocate one buffer instead of both:
  var msg = new Buffer(buf.length + 4);
  BinaryParser.fromInt(buf.length).copy(msg, 0, 0, 4);
  buf.copy(msg, 4, 0, buf.length);
  if (!this.connected) {
    this.offline_queue.push(msg);
  } else {
    this.connection.write(msg);
  }
}
