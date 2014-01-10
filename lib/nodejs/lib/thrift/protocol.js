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

var InputBufferUnderrunError = require('./transport').InputBufferUnderrunError;

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

// NastyHaxx. JavaScript forces hex constants to be
// positive, converting this into a long. If we hardcode the int value
// instead it'll stay in 32 bit-land.
var VERSION_MASK = -65536, // 0xffff0000
    VERSION_1 = -2147418112, // 0x80010000
    TYPE_MASK = 0x000000ff;

var TBinaryProtocol = exports.TBinaryProtocol = function(trans, strictRead, strictWrite) {
  this.trans = trans;
  this.strictRead = (strictRead !== undefined ? strictRead : false);
  this.strictWrite = (strictWrite !== undefined ? strictWrite : true);
}

TBinaryProtocol.prototype.flush = function() {
  return this.trans.flush();
}

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
      throw new TProtocolException(BAD_VERSION, "Bad version in readMessageBegin: " + sz);
    }
    type = sz & TYPE_MASK;
    name = this.readString();
    seqid = this.readI32();
  } else {
    if (this.strictRead) {
      throw new TProtocolException(BAD_VERSION, "No protocol version header");
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
      throw new  Error("Invalid type: " + type);
  }
}

var TJSONProtocol = exports.TJSONProtocol = function(trans) {
  this.trans = trans;
}

TJSONProtocol.Type = {};
TJSONProtocol.Type[Thrift.Type.BOOL] = '"tf"';
TJSONProtocol.Type[Thrift.Type.BYTE] = '"i8"';
TJSONProtocol.Type[Thrift.Type.I16] = '"i16"';
TJSONProtocol.Type[Thrift.Type.I32] = '"i32"';
TJSONProtocol.Type[Thrift.Type.I64] = '"i64"';
TJSONProtocol.Type[Thrift.Type.DOUBLE] = '"dbl"';
TJSONProtocol.Type[Thrift.Type.STRUCT] = '"rec"';
TJSONProtocol.Type[Thrift.Type.STRING] = '"str"';
TJSONProtocol.Type[Thrift.Type.MAP] = '"map"';
TJSONProtocol.Type[Thrift.Type.LIST] = '"lst"';
TJSONProtocol.Type[Thrift.Type.SET] = '"set"';


TJSONProtocol.RType = {};
TJSONProtocol.RType.tf = Thrift.Type.BOOL;
TJSONProtocol.RType.i8 = Thrift.Type.BYTE;
TJSONProtocol.RType.i16 = Thrift.Type.I16;
TJSONProtocol.RType.i32 = Thrift.Type.I32;
TJSONProtocol.RType.i64 = Thrift.Type.I64;
TJSONProtocol.RType.dbl = Thrift.Type.DOUBLE;
TJSONProtocol.RType.rec = Thrift.Type.STRUCT;
TJSONProtocol.RType.str = Thrift.Type.STRING;
TJSONProtocol.RType.map = Thrift.Type.MAP;
TJSONProtocol.RType.lst = Thrift.Type.LIST;
TJSONProtocol.RType.set = Thrift.Type.SET;

TJSONProtocol.Version = 1;

TJSONProtocol.prototype.flush = function() {
  return this.trans.flush();
}

TJSONProtocol.prototype.writeMessageBegin = function(name, messageType, seqid) {
  this.tstack = [];
  this.tpos = [];

  this.tstack.push([TJSONProtocol.Version, '"' + name + '"', messageType, seqid]);
}

TJSONProtocol.prototype.writeMessageEnd = function() {
  var obj = this.tstack.pop();

  this.wobj = this.tstack.pop();
  this.wobj.push(obj);

  this.wbuf = '[' + this.wobj.join(',') + ']';

  this.trans.write(this.wbuf);
}

TJSONProtocol.prototype.writeStructBegin = function(name) {
  this.tpos.push(this.tstack.length);
  this.tstack.push({});
}

TJSONProtocol.prototype.writeStructEnd = function() {
  var p = this.tpos.pop();
  var struct = this.tstack[p];
  var str = '{';
  var first = true;
  for (var key in struct) {
    if (first) {
      first = false;
    } else {
      str += ',';
    }

    str += key + ':' + struct[key];
  }

  str += '}';
  this.tstack[p] = str;
}

TJSONProtocol.prototype.writeFieldBegin = function(name, fieldType, fieldId) {
  this.tpos.push(this.tstack.length);
  this.tstack.push({ 'fieldId': '"' +
    fieldId + '"', 'fieldType': TJSONProtocol.Type[fieldType]
  });
}

TJSONProtocol.prototype.writeFieldEnd = function() {
  var value = this.tstack.pop();
  var fieldInfo = this.tstack.pop();

  if (':' + value === ":[object Object]") {
    this.tstack[this.tstack.length - 1][fieldInfo.fieldId] = '{' +
      fieldInfo.fieldType + ':' + JSON.stringify(value) + '}';
  } else {
    this.tstack[this.tstack.length - 1][fieldInfo.fieldId] = '{' +
      fieldInfo.fieldType + ':' + value + '}';    
  }
  this.tpos.pop();
}

TJSONProtocol.prototype.writeFieldStop = function() {
}

TJSONProtocol.prototype.writeMapBegin = function(ktype, vtype, size) {
  //size is invalid, we'll set it on end.
  this.tpos.push(this.tstack.length);
  this.tstack.push([TJSONProtocol.Type[ktype], TJSONProtocol.Type[vtype], 0]);
}

TJSONProtocol.prototype.writeMapEnd = function() {
  var p = this.tpos.pop();

  if (p == this.tstack.length) {
    return;
  }

  if ((this.tstack.length - p - 1) % 2 !== 0) {
    this.tstack.push('');
  }

  var size = (this.tstack.length - p - 1) / 2;

  this.tstack[p][this.tstack[p].length - 1] = size;

  var map = '}';
  var first = true;
  while (this.tstack.length > p + 1) {
    var v = this.tstack.pop();
    var k = this.tstack.pop();
    if (first) {
      first = false;
    } else {
      map = ',' + map;
    }

    if (! isNaN(k)) { k = '"' + k + '"'; } //json "keys" need to be strings
    map = k + ':' + v + map;
  }
  map = '{' + map;

  this.tstack[p].push(map);
  this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
}

TJSONProtocol.prototype.writeListBegin = function(etype, size) {
  this.tpos.push(this.tstack.length);
  this.tstack.push([TJSONProtocol.Type[etype], size]);
}

TJSONProtocol.prototype.writeListEnd = function() {
  var p = this.tpos.pop();

  while (this.tstack.length > p + 1) {
    var tmpVal = this.tstack[p + 1];
    this.tstack.splice(p + 1, 1);
    this.tstack[p].push(tmpVal);
  }

  this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
}

TJSONProtocol.prototype.writeSetBegin = function(etype, size) {
    this.tpos.push(this.tstack.length);
    this.tstack.push([TJSONProtocol.Type[etype], size]);
}

TJSONProtocol.prototype.writeSetEnd = function() {
  var p = this.tpos.pop();

  while (this.tstack.length > p + 1) {
    var tmpVal = this.tstack[p + 1];
    this.tstack.splice(p + 1, 1);
    this.tstack[p].push(tmpVal);
  }

  this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
}

TJSONProtocol.prototype.writeBool = function(bool) {
  this.tstack.push(bool ? 1 : 0);
}

TJSONProtocol.prototype.writeByte = function(byte) {
  this.tstack.push(byte);
}

TJSONProtocol.prototype.writeI16 = function(i16) {
  this.tstack.push(i16);
}

TJSONProtocol.prototype.writeI32 = function(i32) {
  this.tstack.push(i32);
}

TJSONProtocol.prototype.writeI64 = function(i64) {
  this.tstack.push(i64);
}

TJSONProtocol.prototype.writeDouble = function(dub) {
  this.tstack.push(dub);
}

TJSONProtocol.prototype.writeString = function(str) {
  // We do not encode uri components for wire transfer:
  if (str === null) {
      this.tstack.push(null);
  } else {
      // concat may be slower than building a byte buffer
      var escapedString = '';
      for (var i = 0; i < str.length; i++) {
          var ch = str.charAt(i);      // a single double quote: "
          if (ch === '\"') {
              escapedString += '\\\"'; // write out as: \"
          } else if (ch === '\\') {    // a single backslash: \
              escapedString += '\\\\'; // write out as: \\
          /* Currently escaped forward slashes break TJSONProtocol.
           * As it stands, we can simply pass forward slashes into
           * our strings across the wire without being escaped.
           * I think this is the protocol's bug, not thrift.js
           * } else if(ch === '/') {   // a single forward slash: /
           *  escapedString += '\\/';  // write out as \/
           * }
           */
          } else if (ch === '\b') {    // a single backspace: invisible
              escapedString += '\\b';  // write out as: \b"
          } else if (ch === '\f') {    // a single formfeed: invisible
              escapedString += '\\f';  // write out as: \f"
          } else if (ch === '\n') {    // a single newline: invisible
              escapedString += '\\n';  // write out as: \n"
          } else if (ch === '\r') {    // a single return: invisible
              escapedString += '\\r';  // write out as: \r"
          } else if (ch === '\t') {    // a single tab: invisible
              escapedString += '\\t';  // write out as: \t"
          } else {
              escapedString += ch;     // Else it need not be escaped
          }
      }
      this.tstack.push('"' + escapedString + '"');
  }
}

TJSONProtocol.prototype.writeBinary = function(arg) {
  this.writeString(arg);
}

TJSONProtocol.prototype.readMessageBegin = function() {
  this.rstack = [];
  this.rpos = [];

  //Borrow the inbound transport buffer and ensure data is present/consistent
  var transBuf = this.trans.borrow();
  if (transBuf.readIndex >= transBuf.writeIndex) {
    throw new InputBufferUnderrunError();
  }
  var cursor = transBuf.readIndex;

  if (transBuf.buf[cursor] !== 0x5B) { //[
    throw new Error("Malformed JSON input, no opening bracket");
  }

  //Parse a single message (there may be several in the buffer)
  //  TODO: Handle characters using multiple code units
  cursor++;
  var openBracketCount = 1;
  var inString = false;
  for (; cursor < transBuf.writeIndex; cursor++) {
    var chr = transBuf.buf[cursor];
    //we use hexa charcode here because data[i] returns an int and not a char
    if (inString) {
      if (chr === 0x22) { //"
        inString = false;
      } else if (chr === 0x5C) { //\
        //escaped character, skip
        cursor += 1;
      }
    } else {
      if (chr === 0x5B) { //[
        openBracketCount += 1;
      } else if (chr === 0x5D) { //]
        openBracketCount -= 1;
        if (openBracketCount === 0) {
          //end of json message detected
          break;
        }
      } else if (chr === 0x22) { //"
        inString = true;
      }
    }
  }

  if (openBracketCount !== 0) {
    throw new Error("Malformed JSON input, mismatched backets");
  }

  //Reconstitute the JSON object and conume the necessary bytes
  this.robj = JSON.parse(transBuf.buf.slice(transBuf.readIndex, cursor+1));
  this.trans.consume(cursor + 1 - transBuf.readIndex);

  //Verify the protocol version
  var version = this.robj.shift();
  if (version != TJSONProtocol.Version) {
    throw 'Wrong thrift protocol version: ' + version;
  }

  //Objectify the thrift message {name/type/sequence-number} for return 
  // and then save the JSON object in rstack
  var r = {};
  r.fname = this.robj.shift();
  r.mtype = this.robj.shift();
  r.rseqid = this.robj.shift();
  this.rstack.push(this.robj.shift());
  return r;
}

TJSONProtocol.prototype.readMessageEnd = function() {
}

TJSONProtocol.prototype.readStructBegin = function() {
  var r = {};
  r.fname = '';

  //incase this is an array of structs
  if (this.rstack[this.rstack.length - 1] instanceof Array) {
    this.rstack.push(this.rstack[this.rstack.length - 1].shift());
  }

  return r;
}

TJSONProtocol.prototype.readStructEnd = function() {
  this.rstack.pop();
}

TJSONProtocol.prototype.readFieldBegin = function() {
  var r = {};

  var fid = -1;
  var ftype = Thrift.Type.STOP;

  //get a fieldId
  for (var f in (this.rstack[this.rstack.length - 1])) {
    if (f === null) {
      continue;
    }

    fid = parseInt(f, 10);
    this.rpos.push(this.rstack.length);

    var field = this.rstack[this.rstack.length - 1][fid];

    //remove so we don't see it again
    delete this.rstack[this.rstack.length - 1][fid];

    this.rstack.push(field);

    break;
  }

  if (fid != -1) {
    //should only be 1 of these but this is the only
    //way to match a key
    for (var i in (this.rstack[this.rstack.length - 1])) {
      if (TJSONProtocol.RType[i] === null) {
        continue;
      }

      ftype = TJSONProtocol.RType[i];
      this.rstack[this.rstack.length - 1] = this.rstack[this.rstack.length - 1][i];
    }
  }

  r.fname = '';
  r.ftype = ftype;
  r.fid = fid;

  return r;
}

TJSONProtocol.prototype.readFieldEnd = function() {
  var pos = this.rpos.pop();

  //get back to the right place in the stack
  while (this.rstack.length > pos) {
    this.rstack.pop();
  }
}

TJSONProtocol.prototype.readMapBegin = function() {
  var map = this.rstack.pop();

  var r = {};
  r.ktype = TJSONProtocol.RType[map.shift()];
  r.vtype = TJSONProtocol.RType[map.shift()];
  r.size = map.shift();


  this.rpos.push(this.rstack.length);
  this.rstack.push(map.shift());

  return r;
}

TJSONProtocol.prototype.readMapEnd = function() {
  this.readFieldEnd();
}

TJSONProtocol.prototype.readListBegin = function() {
  var list = this.rstack[this.rstack.length - 1];

  var r = {};
  r.etype = TJSONProtocol.RType[list.shift()];
  r.size = list.shift();

  this.rpos.push(this.rstack.length);
  this.rstack.push(list);

  return r;
}

TJSONProtocol.prototype.readListEnd = function() {
  this.readFieldEnd();
}

TJSONProtocol.prototype.readSetBegin = function() {
  return this.readListBegin();
}

TJSONProtocol.prototype.readSetEnd = function() {
  return this.readListEnd();
}

TJSONProtocol.prototype.readBool = function() {
  var r = this.readI32();

  if (r !== null && r.value == '1') {
    r.value = true;
  } else {
    r.value = false;
  }

  return r;
}

TJSONProtocol.prototype.readByte = function() {
  return this.readI32();
}

TJSONProtocol.prototype.readI16 = function() {
  return this.readI32();
}

TJSONProtocol.prototype.readI32 = function(f) {
  if (f === undefined) {
    f = this.rstack[this.rstack.length - 1];
  }

  var r = {};

  if (f instanceof Array) {
    if (f.length === 0) {
      r.value = undefined;
    } else {
      r.value = f.shift();
    }
  } else if (f instanceof Object) {
    for (var i in f) {
      if (i === null) {
        continue;
      }
      this.rstack.push(f[i]);
      delete f[i];

      r.value = i;
      break;
    }
  } else {
    r.value = f;
    this.rstack.pop();
  }

  return r.value;
}

TJSONProtocol.prototype.readI64 = function() {
  return new Int64(this.readI32());
}

TJSONProtocol.prototype.readDouble = function() {
  return this.readI32();
}

TJSONProtocol.prototype.readBinary = function() {
  return this.readString();
}

TJSONProtocol.prototype.readString = function() {
  var r = this.readI32();
  return r;
}

TJSONProtocol.prototype.getTransport = function() {
  return this.trans;
}

//Method to arbitrarily skip over data.
TJSONProtocol.prototype.skip = function(type) {
  throw 'skip not supported yet';
}

