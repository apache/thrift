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
var net = require('net');

var ttransport = require('./transport')
  , TBinaryProtocol = require('./protocol').TBinaryProtocol;

exports.createServer = function(cls, handler, options) {
  if (cls.Processor) {
    cls = cls.Processor;
  }
  var processor = new cls(handler);
  var transport = (options && options.transport) ? options.transport : ttransport.TBufferedTransport;
  var protocol = (options && options.protocol) ? options.protocol : TBinaryProtocol;

  return net.createServer(function(stream) {
    var self = this;
    stream.on('data', transport.receiver(function(transportWithData) {
      var input = new protocol(transportWithData);
      var output = new protocol(new transport(undefined, function(buf) {
        try {
            stream.write(buf);
        } catch (err) {
            self.emit('error', err);
            stream.end();
        }
      }));

      try {
        processor.process(input, output);
        transportWithData.commitPosition();
      }
      catch (err) {
        if (err instanceof ttransport.InputBufferUnderrunError) {
          transportWithData.rollbackPosition();
        }
        else {
          self.emit('error', err);
          stream.end();
        }
      }
    }));

    stream.on('end', function() {
      stream.end();
    });
  });
};
