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
    net = require('net');

var BinaryParser = require('./binary_parser').BinaryParser,
    TMemoryBuffer = require('./transport').TMemoryBuffer,
    TBinaryProtocol = require('./protocol').TBinaryProtocol,
    int32FramedReceiver = require('./connection').int32FramedReceiver;

exports.createServer = function(cls, handler) {
  if (cls.Processor) {
    cls = cls.Processor;
  }
  var processor = new cls(handler);

  return net.createServer(function(stream) {
    stream.on('data', int32FramedReceiver(function(data) {
      var input = new TBinaryProtocol(new TMemoryBuffer(data));
      var output = new TBinaryProtocol(new TMemoryBuffer(undefined, function(buf) {
        // TODO: optimize this better, allocate one buffer instead of both:
        var msg = new Buffer(buf.length + 4);
        BinaryParser.fromInt(buf.length).copy(msg, 0, 0, 4);
        buf.copy(msg, 4, 0, buf.length);
        stream.write(msg);
      }));

      processor.process(input, output);
    }));

    stream.on('end', function() {
      stream.end();
    });
  });
}
