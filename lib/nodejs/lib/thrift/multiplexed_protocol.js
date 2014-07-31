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
var Thrift = require('./thrift');

var Wrapper = exports.Wrapper = function(service_name, protocol, connection) {

    var MultiplexProtocol = function(trans, strictRead, strictWrite) {
        protocol.call(this, trans, strictRead, strictWrite);
    };
    util.inherits(MultiplexProtocol, protocol);

    MultiplexProtocol.prototype.writeMessageBegin = function(name, type, seqid) {
        if (type == Thrift.MessageType.CALL || type == Thrift.MessageType.ONEWAY) {
            connection.seqId2Service[seqid] = service_name;
            MultiplexProtocol.super_.prototype.writeMessageBegin.call(this, 
                                                                      service_name + ":" + name, 
                                                                      type, 
                                                                      seqid);
        } else {
            MultiplexProtocol.super_.prototype.writeMessageBegin.call(this, name, type, seqid);
        }
    };

    return MultiplexProtocol;
};

var Multiplexer = exports.Multiplexer = function() {
    this.seqid = 0;
};

Multiplexer.prototype.createClient = function(service_name, cls, connection) {
    if (cls.Client) {
        cls = cls.Client;
    }
    var self = this;
    cls.prototype.new_seqid = function() {
        self.seqid += 1;
        return self.seqid;
    };
    var client = new cls(new connection.transport(undefined, function(buf) {
        connection.write(buf);
    }), new Wrapper(service_name, connection.protocol, connection));
    
    if (typeof connection.client !== 'object') {
       connection.client = {};
    }
    connection.client[service_name] = client;

    return client;
};
