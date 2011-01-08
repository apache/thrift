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
var TMemoryBuffer = exports.TMemoryBuffer = function(buffer, flushCallback) {
  if (buffer !== undefined) {
    this.recv_buf = buffer;
  } else {
    this.recv_buf = new Buffer(0);
  }
  this.recv_buf_sz = this.recv_buf.length;
  this.send_buf = [];
  this.rpos = 0;
  this.flushCallback = flushCallback;
}

TMemoryBuffer.prototype.isOpen = function() {
  // TODO
  return true;
}

TMemoryBuffer.prototype.open = function() {
}

TMemoryBuffer.prototype.close = function() {
}

TMemoryBuffer.prototype.read = function(len) {
  var avail = this.recv_buf_sz - this.rpos;
  // console.log("avail: " + avail);

  if(avail == 0)
      return new Buffer(0);

  var give = len

  if(avail < len) {
      console.log("asked for: " + len);
      throw new Error("asked for too much");
      give = avail
  }

  // console.log(this.rpos + "," + give);
  var ret = this.recv_buf.slice(this.rpos,this.rpos + give)
  this.rpos += give
  // console.log(ret);

  //clear buf when complete?
  return ret

}

TMemoryBuffer.prototype.readAll = function() {
  return this.recv_buf;
}

TMemoryBuffer.prototype.write = function(buf) {
  // TODO
  if (typeof(buf) === "string") {
    for (var i = 0; i < buf.length; ++i) {
      this.send_buf.push(buf.charCodeAt(i));
    }
  } else {
    for (var i = 0; i < buf.length; ++i) {
      this.send_buf.push(buf[i]);
    }
  }
}

TMemoryBuffer.prototype.flush = function() {
  this.flushCallback(new Buffer(this.send_buf));
  this.send_buf = [];
}
