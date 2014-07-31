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
var emptyBuf = new Buffer(0);

var binary = require('./binary');
var util = require("util");

var InputBufferUnderrunError = exports.InputBufferUnderrunError = function(message) {
  Error.call(this, message);
};
util.inherits(InputBufferUnderrunError, Error);

var TFramedTransport = exports.TFramedTransport = function(buffer, callback) {
  this.inBuf = buffer || emptyBuf;
  this.outBuffers = [];
  this.outCount = 0;
  this.readPos = 0;
  this.onFlush = callback;
};
TFramedTransport.receiver = function(callback) {
  var residual = null;

  return function(data) {
    // Prepend any residual data from our previous read
    if (residual) {
      data = Buffer.concat([residual, data]);
      residual = null;
    }

    // framed transport
    while (data.length) {
      if (data.length < 4) {
        // Not enough bytes to continue, save and resume on next packet
        residual = data;
        return;
      }
      var frameSize = binary.readI32(data, 0);
      if (data.length < 4 + frameSize) {
        // Not enough bytes to continue, save and resume on next packet
        residual = data;
        return;
      }

      var frame = data.slice(4, 4 + frameSize);
      residual = data.slice(4 + frameSize);

      callback(new TFramedTransport(frame));

      data = residual;
      residual = null;
    }
  };
};

TFramedTransport.prototype = {
  commitPosition: function(){},
  rollbackPosition: function(){},

  // TODO: Implement open/close support
  isOpen: function() {return true;},
  open: function() {},
  close: function() {},

  ensureAvailable: function(len) {
    if (this.readPos + len > this.inBuf.length) {
      throw new InputBufferUnderrunError();
    }
  },

  read: function(len) { // this function will be used for each frames.
    this.ensureAvailable(len);
    var end = this.readPos + len;

    if (this.inBuf.length < end) {
      throw new Error('read(' + len + ') failed - not enough data');
    }

    var buf = this.inBuf.slice(this.readPos, end);
    this.readPos = end;
    return buf;
  },

  readByte: function() {
    this.ensureAvailable(1);
    return binary.readByte(this.inBuf[this.readPos++]);
  },

  readI16: function() {
    this.ensureAvailable(2);
    var i16 = binary.readI16(this.inBuf, this.readPos);
    this.readPos += 2;
    return i16;
  },

  readI32: function() {
    this.ensureAvailable(4);
    var i32 = binary.readI32(this.inBuf, this.readPos);
    this.readPos += 4;
    return i32;
  },

  readDouble: function() {
    this.ensureAvailable(8);
    var d = binary.readDouble(this.inBuf, this.readPos);
    this.readPos += 8;
    return d;
  },

  readString: function(len) {
    this.ensureAvailable(len);
    var str = this.inBuf.toString('utf8', this.readPos, this.readPos + len);
    this.readPos += len;
    return str;
  },

  borrow: function() {
    return { buf: this.inBuf, readIndex: this.readPos, writeIndex: this.inBuf.length };
  },

  consume: function(bytesConsumed) {
    this.readPos += bytesConsumed;
  },

  write: function(buf, encoding) {
    if (typeof(buf) === "string") {
      buf = new Buffer(buf, encoding || 'utf8');
    }
    this.outBuffers.push(buf);
    this.outCount += buf.length;
  },

  flush: function() {
    var out = new Buffer(this.outCount),
        pos = 0;
    this.outBuffers.forEach(function(buf) {
      buf.copy(out, pos, 0);
      pos += buf.length;
    });
    
    if (this.onFlush) {
      // TODO: optimize this better, allocate one buffer instead of both:
      var msg = new Buffer(out.length + 4);
      binary.writeI32(msg, out.length);
      out.copy(msg, 4, 0, out.length);
      this.onFlush(msg);
    }

    this.outBuffers = [];
    this.outCount = 0;
  }
};

var TBufferedTransport = exports.TBufferedTransport = function(buffer, callback) {
  this.defaultReadBufferSize = 1024;
  this.writeBufferSize = 512; // Soft Limit
  this.inBuf = new Buffer(this.defaultReadBufferSize);
  this.readCursor = 0;
  this.writeCursor = 0; // for input buffer
  this.outBuffers = [];
  this.outCount = 0;
  this.onFlush = callback;
};
TBufferedTransport.receiver = function(callback) {
  var reader = new TBufferedTransport();

  return function(data) {
    if (reader.writeCursor + data.length > reader.inBuf.length) {
      var buf = new Buffer(reader.writeCursor + data.length);
      reader.inBuf.copy(buf, 0, 0, reader.writeCursor);
      reader.inBuf = buf;
    }
    data.copy(reader.inBuf, reader.writeCursor, 0);
    reader.writeCursor += data.length;

    callback(reader);
  };
};

TBufferedTransport.prototype = {
  commitPosition: function(){
    var unreadSize = this.writeCursor - this.readCursor;
    var bufSize = (unreadSize * 2 > this.defaultReadBufferSize) ? 
      unreadSize * 2 : this.defaultReadBufferSize;
    var buf = new Buffer(bufSize);
    if (unreadSize > 0) {
      this.inBuf.copy(buf, 0, this.readCursor, this.writeCursor);
    }
    this.readCursor = 0;
    this.writeCursor = unreadSize;
    this.inBuf = buf;
  },
  rollbackPosition: function(){
    this.readCursor = 0;
  },

  // TODO: Implement open/close support
  isOpen: function() {return true;},
  open: function() {},
  close: function() {},

  ensureAvailable: function(len) {
    if (this.readCursor + len > this.writeCursor) {
      throw new InputBufferUnderrunError();
    }
  },

  read: function(len) {
    this.ensureAvailable(len);
    var buf = new Buffer(len);
    this.inBuf.copy(buf, 0, this.readCursor, this.readCursor + len);
    this.readCursor += len;
    return buf;
  },

  readByte: function() {
    this.ensureAvailable(1);
    return binary.readByte(this.inBuf[this.readCursor++]);
  },

  readI16: function() {
    this.ensureAvailable(2);
    var i16 = binary.readI16(this.inBuf, this.readCursor);
    this.readCursor += 2;
    return i16;
  },

  readI32: function() {
    this.ensureAvailable(4);
    var i32 = binary.readI32(this.inBuf, this.readCursor);
    this.readCursor += 4;
    return i32;
  },

  readDouble: function() {
    this.ensureAvailable(8);
    var d = binary.readDouble(this.inBuf, this.readCursor);
    this.readCursor += 8;
    return d;
  },

  readString: function(len) {
    this.ensureAvailable(len);
    var str = this.inBuf.toString('utf8', this.readCursor, this.readCursor + len);
    this.readCursor += len;
    return str;
  },

  borrow: function() {
    var obj = {buf: this.inBuf, readIndex: this.readCursor, writeIndex: this.writeCursor};
    return obj;
  },

  consume: function(bytesConsumed) {
    this.readCursor += bytesConsumed;
  },

  write: function(buf) {
    if (typeof(buf) === "string") {
      buf = new Buffer(buf, 'utf8');
    }
    this.outBuffers.push(buf);
    this.outCount += buf.length;
  },

  flush: function() {
    if (this.outCount < 1) {
      return;
    }
    
    var msg = new Buffer(this.outCount),
        pos = 0;
    this.outBuffers.forEach(function(buf) {
      buf.copy(msg, pos, 0);
      pos += buf.length;
    });
    
    if (this.onFlush) {
      this.onFlush(msg);
    }

    this.outBuffers = [];
    this.outCount = 0;
  }
};
