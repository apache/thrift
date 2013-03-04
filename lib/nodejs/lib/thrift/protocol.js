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
    Thrift = require('./thrift'),
    Type = Thrift.Type;

var binary = require('./binary'),
    Int64 = require('node-int64');

var UNKNOWN = 0,
    INVALID_DATA = 1,
    NEGATIVE_SIZE = 2,
    SIZE_LIMIT = 3,
    BAD_VERSION = 4;

var TProtocolException = function(type, message) {
  Error.call(this, message);
  this.name = 'TProtocolException';
  this.type = type;
}
util.inherits(TProtocolException, Error);

var TBinaryProtocol = exports.TBinaryProtocol = function(trans, strictRead, strictWrite) {
  this.trans = trans;
  this.strictRead = (strictRead !== undefined ? strictRead : false);
  this.strictWrite = (strictWrite !== undefined ? strictWrite : true);
}

TBinaryProtocol.prototype.flush = function() {
  return this.trans.flush();
}

// NastyHaxx. JavaScript forces hex constants to be
// positive, converting this into a long. If we hardcode the int value
// instead it'll stay in 32 bit-land.

var VERSION_MASK = -65536, // 0xffff0000
    VERSION_1 = -2147418112, // 0x80010000
    TYPE_MASK = 0x000000ff;

TBinaryProtocol.prototype.writeMessageBegin = function(name, type, seqid) {
    if (this.strictWrite) {
      this.writeI32(VERSION_1 | type);
      this.writeString(name);
      this.writeI32(seqid);
    } else {
      this.writeString(name);
      this.writeByte(type);
      this.writeI32(seqid);
    }
}

TBinaryProtocol.prototype.writeMessageEnd = function() {
}

TBinaryProtocol.prototype.writeStructBegin = function(name) {
}

TBinaryProtocol.prototype.writeStructEnd = function() {
}

TBinaryProtocol.prototype.writeFieldBegin = function(name, type, id) {
  this.writeByte(type);
  this.writeI16(id);
}

TBinaryProtocol.prototype.writeFieldEnd = function() {
}

TBinaryProtocol.prototype.writeFieldStop = function() {
  this.writeByte(Type.STOP);
}

TBinaryProtocol.prototype.writeMapBegin = function(ktype, vtype, size) {
  this.writeByte(ktype);
  this.writeByte(vtype);
  this.writeI32(size);
}

TBinaryProtocol.prototype.writeMapEnd = function() {
}

TBinaryProtocol.prototype.writeListBegin = function(etype, size) {
  this.writeByte(etype);
  this.writeI32(size);
}

TBinaryProtocol.prototype.writeListEnd = function() {
}

TBinaryProtocol.prototype.writeSetBegin = function(etype, size) {
  console.log('write set', etype, size);
  this.writeByte(etype);
  this.writeI32(size);
}

TBinaryProtocol.prototype.writeSetEnd = function() {
}

TBinaryProtocol.prototype.writeBool = function(bool) {
  if (bool) {
    this.writeByte(1);
  } else {
    this.writeByte(0);
  }
}

TBinaryProtocol.prototype.writeByte = function(byte) {
  this.trans.write(new Buffer([byte]));
}

TBinaryProtocol.prototype.writeI16 = function(i16) {
  this.trans.write(binary.writeI16(new Buffer(2), i16));
}

TBinaryProtocol.prototype.writeI32 = function(i32) {
  this.trans.write(binary.writeI32(new Buffer(4), i32));
}

TBinaryProtocol.prototype.writeI64 = function(i64) {
  if (i64.buffer) {
    this.trans.write(i64.buffer);
  } else {
    this.trans.write(new Int64(i64).buffer)
  }
}

TBinaryProtocol.prototype.writeDouble = function(dub) {
  this.trans.write(binary.writeDouble(new Buffer(8), dub));
}

TBinaryProtocol.prototype.writeString = function(arg) {
  if (typeof(arg) === 'string') {
    this.writeI32(Buffer.byteLength(arg, 'utf8'))
    this.trans.write(arg, 'utf8');
  } else if (arg instanceof Buffer) {
    this.writeI32(arg.length)
    this.trans.write(arg);
  } else {
    throw new Error('writeString called without a string/Buffer argument: ' + arg)
  }
}

TBinaryProtocol.prototype.writeBinary = function(arg) {
  if (typeof(arg) === 'string') {
    this.writeI32(Buffer.byteLength(arg, 'utf8'))
    this.trans.write(arg, 'utf8');
  } else if (arg instanceof Buffer) {
    this.writeI32(arg.length)
    this.trans.write(arg);
  } else {
    throw new Error('writeBinary called without a string/Buffer argument: ' + arg)
  }
}

TBinaryProtocol.prototype.readMessageBegin = function() {
  var sz = this.readI32();
  var type, name, seqid;

  if (sz < 0) {
    var version = sz & VERSION_MASK;
    if (version != VERSION_1) {
      console.log("BAD: " + version);
      throw TProtocolException(BAD_VERSION, "Bad version in readMessageBegin: " + sz);
    }
    type = sz & TYPE_MASK;
    name = this.readString();
    seqid = this.readI32();
  } else {
    if (this.strictRead) {
      throw TProtocolException(BAD_VERSION, "No protocol version header");
    }
    name = this.trans.read(sz);
    type = this.readByte();
    seqid = this.readI32();
  }
  return {fname: name, mtype: type, rseqid: seqid};
}

TBinaryProtocol.prototype.readMessageEnd = function() {
}

TBinaryProtocol.prototype.readStructBegin = function() {
  return {fname: ''}
}

TBinaryProtocol.prototype.readStructEnd = function() {
}

TBinaryProtocol.prototype.readFieldBegin = function() {
  var type = this.readByte();
  if (type == Type.STOP) {
    return {fname: null, ftype: type, fid: 0};
  }
  var id = this.readI16();
  return {fname: null, ftype: type, fid: id};
}

TBinaryProtocol.prototype.readFieldEnd = function() {
}

TBinaryProtocol.prototype.readMapBegin = function() {
  var ktype = this.readByte();
  var vtype = this.readByte();
  var size = this.readI32();
  return {ktype: ktype, vtype: vtype, size: size};
}

TBinaryProtocol.prototype.readMapEnd = function() {
}

TBinaryProtocol.prototype.readListBegin = function() {
  var etype = this.readByte();
  var size = this.readI32();
  return {etype: etype, size: size};
}

TBinaryProtocol.prototype.readListEnd = function() {
}

TBinaryProtocol.prototype.readSetBegin = function() {
  var etype = this.readByte();
  var size = this.readI32();
  return {etype: etype, size: size};
}

TBinaryProtocol.prototype.readSetEnd = function() {
}

TBinaryProtocol.prototype.readBool = function() {
  var byte = this.readByte();
  if (byte == 0) {
    return false;
  }
  return true;
}

TBinaryProtocol.prototype.readByte = function() {
  return this.trans.readByte();
}

TBinaryProtocol.prototype.readI16 = function() {
  return this.trans.readI16();
}

TBinaryProtocol.prototype.readI32 = function() {
  return this.trans.readI32();
}

TBinaryProtocol.prototype.readI64 = function() {
  var buff = this.trans.read(8);
  return new Int64(buff);
}

TBinaryProtocol.prototype.readDouble = function() {
  return this.trans.readDouble();
}

TBinaryProtocol.prototype.readBinary = function() {
  var len = this.readI32();
  return this.trans.read(len);
}

TBinaryProtocol.prototype.readString = function() {
  var len = this.readI32();
  return this.trans.readString(len);
}

TBinaryProtocol.prototype.getTransport = function() {
  return this.trans;
}

TBinaryProtocol.prototype.skip = function(type) {
  // console.log("skip: " + type);
  switch (type) {
    case Type.STOP:
      return;
    case Type.BOOL:
      this.readBool();
      break;
    case Type.BYTE:
      this.readByte();
      break;
    case Type.I16:
      this.readI16();
      break;
    case Type.I32:
      this.readI32();
      break;
    case Type.I64:
      this.readI64();
      break;
    case Type.DOUBLE:
      this.readDouble();
      break;
    case Type.STRING:
      this.readString();
      break;
    case Type.STRUCT:
      this.readStructBegin();
      while (true) {
        var r = this.readFieldBegin();
        if (r.ftype === Type.STOP) {
          break;
        }
        this.skip(r.ftype);
        this.readFieldEnd();
      }
      this.readStructEnd();
      break;
    case Type.MAP:
      var r = this.readMapBegin();
      for (var i = 0; i < r.size; ++i) {
        this.skip(r.ktype);
        this.skip(r.vtype);
      }
      this.readMapEnd();
      break;
    case Type.SET:
      var r = this.readSetBegin();
      for (var i = 0; i < r.size; ++i) {
        this.skip(r.etype);
      }
      this.readSetEnd();
      break;
    case Type.LIST:
      var r = this.readListBegin();
      for (var i = 0; i < r.size; ++i) {
        this.skip(r.etype);
      }
      this.readListEnd();
      break;
    default:
      throw Error("Invalid type: " + type);
  }
}
