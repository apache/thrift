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

var Type = exports.Type = {
  STOP: 0,
  VOID: 1,
  BOOL: 2,
  BYTE: 3,
  I08: 3,
  DOUBLE: 4,
  I16: 6,
  I32: 8,
  I64: 10,
  STRING: 11,
  UTF7: 11,
  STRUCT: 12,
  MAP: 13,
  SET: 14,
  LIST: 15,
  UTF8: 16,
  UTF16: 17,
}

exports.MessageType = {
  CALL: 1,
  REPLY: 2,
  EXCEPTION: 3,
  ONEWAY: 4,
}

var TException = exports.TException = function(message) {
  Error.call(this, message);
  this.name = 'TException';
}
util.inherits(TException, Error);

var TApplicationExceptionType = exports.TApplicationExceptionType = {
  UNKNOWN: 0,
  UNKNOWN_METHOD: 1,
  INVALID_MESSAGE_TYPE: 2,
  WRONG_METHOD_NAME: 3,
  BAD_SEQUENCE_ID: 4,
  MISSING_RESULT: 5,
  INTERNAL_ERROR: 6,
  PROTOCOL_ERROR: 7,
  INVALID_TRANSFORM: 8,
  INVALID_PROTOCOL: 9,
  UNSUPPORTED_CLIENT_TYPE: 10
}

var TApplicationException = exports.TApplicationException = function(type, message) {
  TException.call(this, message);
  this.type = type || TApplicationExceptionType.UNKNOWN;
  this.name = 'TApplicationException';
}
util.inherits(TApplicationException, TException);

TApplicationException.prototype.read = function(input) {
  var ftype
  var fid
  var ret = input.readStructBegin('TApplicationException')

  while(1){

      ret = input.readFieldBegin()

      if(ret.ftype == Type.STOP)
          break

      var fid = ret.fid

      switch(fid){
          case 1:
              if( ret.ftype == Type.STRING ){
                  ret = input.readString()
                  this.message = ret
              } else {
                  ret = input.skip(ret.ftype)
              }

              break
          case 2:
              if( ret.ftype == Type.I32 ){
                  ret = input.readI32()
                  this.type = ret
              } else {
                  ret   = input.skip(ret.ftype)
              }
              break

          default:
              ret = input.skip(ret.ftype)
              break
      }
      input.readFieldEnd()
  }

  input.readStructEnd()
}

TApplicationException.prototype.write = function(output){
  output.writeStructBegin('TApplicationException');

  if (this.message) {
      output.writeFieldBegin('message', Type.STRING, 1)
      output.writeString(this.message)
      output.writeFieldEnd()
  }

  if (this.code) {
      output.writeFieldBegin('type', Type.I32, 2)
      output.writeI32(this.code)
      output.writeFieldEnd()
  }

  output.writeFieldStop()
  output.writeStructEnd()
}

exports.objectLength = function(obj) {
  return Object.keys(obj).length;
}

exports.inherits = function(constructor, superConstructor) {
  util.inherits(constructor, superConstructor);
}
