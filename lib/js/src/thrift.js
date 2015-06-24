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

/*jshint evil:true*/

/**
 * The Thrift namespace houses the Apache Thrift JavaScript library 
 * elements providing JavaScript bindings for the Apache Thrift RPC 
 * system. End users will typically only directly make use of the 
 * Transport (TXHRTransport/TWebSocketTransport) and Protocol 
 * (TJSONPRotocol/TBinaryProtocol) constructors.
 * 
 * Object methods beginning with a __ (e.g. __onOpen()) are internal 
 * and should not be called outside of the object's own methods.
 * 
 * This library creates one global object: Thrift
 * Code in this library must never create additional global identifiers,
 * all features must be scoped within the Thrift namespace.
 * @namespace
 * @example
 *     var transport = new Thrift.Transport("http://localhost:8585");
 *     var protocol  = new Thrift.Protocol(transport);
 *     var client = new MyThriftSvcClient(protocol);
 *     var result = client.MyMethod();
 */
var Thrift = {
    /**
     * Thrift JavaScript library version.
     * @readonly
     * @const {string} Version
     * @memberof Thrift
     */
    Version: '1.0.0-dev',

    /**
     * Thrift IDL type string to Id mapping.
     * @readonly
     * @property {number}  STOP   - End of a set of fields.
     * @property {number}  VOID   - No value (only legal for return types).
     * @property {number}  BOOL   - True/False integer.
     * @property {number}  BYTE   - Signed 8 bit integer.
     * @property {number}  I08    - Signed 8 bit integer.     
     * @property {number}  DOUBLE - 64 bit IEEE 854 floating point.
     * @property {number}  I16    - Signed 16 bit integer.
     * @property {number}  I32    - Signed 32 bit integer.
     * @property {number}  I64    - Signed 64 bit integer.
     * @property {number}  STRING - Array of bytes representing a string of characters.
     * @property {number}  UTF7   - Array of bytes representing a string of UTF7 encoded characters.
     * @property {number}  STRUCT - A multifield type.
     * @property {number}  MAP    - A collection type (map/associative-array/dictionary).
     * @property {number}  SET    - A collection type (unordered and without repeated values).
     * @property {number}  LIST   - A collection type (unordered).
     * @property {number}  UTF8   - Array of bytes representing a string of UTF8 encoded characters.
     * @property {number}  UTF16  - Array of bytes representing a string of UTF16 encoded characters.
     */
    Type: {
        'STOP' : 0,
        'VOID' : 1,
        'BOOL' : 2,
        'BYTE' : 3,
        'I08' : 3,
        'DOUBLE' : 4,
        'I16' : 6,
        'I32' : 8,
        'I64' : 10,
        'STRING' : 11,
        'UTF7' : 11,
        'STRUCT' : 12,
        'MAP' : 13,
        'SET' : 14,
        'LIST' : 15,
        'UTF8' : 16,
        'UTF16' : 17
    },

    /**
     * Thrift RPC message type string to Id mapping.
     * @readonly
     * @property {number}  CALL      - RPC call sent from client to server.
     * @property {number}  REPLY     - RPC call normal response from server to client.
     * @property {number}  EXCEPTION - RPC call exception response from server to client.
     * @property {number}  ONEWAY    - Oneway RPC call from client to server with no response.
     */
    MessageType: {
        'CALL' : 1,
        'REPLY' : 2,
        'EXCEPTION' : 3,
        'ONEWAY' : 4
    },

    /**
     * Utility function returning the count of an object's own properties.
     * @param {object} obj - Object to test.
     * @returns {number} number of object's own properties
     */
    objectLength: function(obj) {
        var length = 0;
        for (var k in obj) {
            if (obj.hasOwnProperty(k)) {
                length++;
            }
        }
        return length;
    },

    /**
     * Utility function to establish prototype inheritance.
     * @see {@link http://javascript.crockford.com/prototypal.html|Prototypal Inheritance}
     * @param {function} constructor - Contstructor function to set as derived.
     * @param {function} superConstructor - Contstructor function to set as base.
     * @param {string} [name] - Type name to set as name property in derived prototype.
     */
    inherits: function(constructor, superConstructor, name) {
      function F() {}
      F.prototype = superConstructor.prototype;
      constructor.prototype = new F();
      constructor.prototype.name = name || "";
    }
};

/**
 * Initializes a Thrift TException instance.
 * @constructor
 * @augments Error
 * @param {string} message - The TException message (distinct from the Error message).
 * @classdesc TException is the base class for all Thrift exceptions types.
 */
Thrift.TException = function(message) {
    this.message = message;
};
Thrift.inherits(Thrift.TException, Error, 'TException');

/**
 * Returns the message set on the exception.
 * @readonly
 * @returns {string} exception message
 */
Thrift.TException.prototype.getMessage = function() {
    return this.message;
};

/**
 * Thrift Application Exception type string to Id mapping.
 * @readonly
 * @property {number}  UNKNOWN                 - Unknown/undefined.
 * @property {number}  UNKNOWN_METHOD          - Client attempted to call a method unknown to the server.
 * @property {number}  INVALID_MESSAGE_TYPE    - Client passed an unknown/unsupported MessageType.
 * @property {number}  WRONG_METHOD_NAME       - Unused.
 * @property {number}  BAD_SEQUENCE_ID         - Unused in Thrift RPC, used to flag proprietary sequence number errors.
 * @property {number}  MISSING_RESULT          - Raised by a server processor if a handler fails to supply the required return result.
 * @property {number}  INTERNAL_ERROR          - Something bad happened.
 * @property {number}  PROTOCOL_ERROR          - The protocol layer failed to serialize or deserialize data.
 * @property {number}  INVALID_TRANSFORM       - Unused.
 * @property {number}  INVALID_PROTOCOL        - The protocol (or version) is not supported.
 * @property {number}  UNSUPPORTED_CLIENT_TYPE - Unused.
 */
Thrift.TApplicationExceptionType = {
    'UNKNOWN' : 0,
    'UNKNOWN_METHOD' : 1,
    'INVALID_MESSAGE_TYPE' : 2,
    'WRONG_METHOD_NAME' : 3,
    'BAD_SEQUENCE_ID' : 4,
    'MISSING_RESULT' : 5,
    'INTERNAL_ERROR' : 6,
    'PROTOCOL_ERROR' : 7,
    'INVALID_TRANSFORM' : 8,
    'INVALID_PROTOCOL' : 9,
    'UNSUPPORTED_CLIENT_TYPE' : 10
};

/**
 * Initializes a Thrift TApplicationException instance.
 * @constructor
 * @augments Thrift.TException
 * @param {string} message - The TApplicationException message (distinct from the Error message).
 * @param {Thrift.TApplicationExceptionType} [code] - The TApplicationExceptionType code.
 * @classdesc TApplicationException is the exception class used to propagate exceptions from an RPC server back to a calling client.
*/
Thrift.TApplicationException = function(message, code) {
    this.message = message;
    this.code = typeof code === "number" ? code : 0;
};
Thrift.inherits(Thrift.TApplicationException, Thrift.TException, 'TApplicationException');

/**
 * Read a TApplicationException from the supplied protocol.
 * @param {object} input - The input protocol to read from.
 */
Thrift.TApplicationException.prototype.read = function(input) {
    while (1) {
        var ret = input.readFieldBegin();

        if (ret.ftype == Thrift.Type.STOP) {
            break;
        }

        var fid = ret.fid;

        switch (fid) {
            case 1:
                if (ret.ftype == Thrift.Type.STRING) {
                    ret = input.readString();
                    this.message = ret.value;
                } else {
                    ret = input.skip(ret.ftype);
                }
                break;
            case 2:
                if (ret.ftype == Thrift.Type.I32) {
                    ret = input.readI32();
                    this.code = ret.value;
                } else {
                    ret = input.skip(ret.ftype);
                }
                break;
           default:
                ret = input.skip(ret.ftype);
                break;
        }

        input.readFieldEnd();
    }

    input.readStructEnd();
};

/**
 * Wite a TApplicationException to the supplied protocol.
 * @param {object} output - The output protocol to write to.
 */
Thrift.TApplicationException.prototype.write = function(output) {
    output.writeStructBegin('TApplicationException');

    if (this.message) {
        output.writeFieldBegin('message', Thrift.Type.STRING, 1);
        output.writeString(this.getMessage());
        output.writeFieldEnd();
    }

    if (this.code) {
        output.writeFieldBegin('type', Thrift.Type.I32, 2);
        output.writeI32(this.code);
        output.writeFieldEnd();
    }

    output.writeFieldStop();
    output.writeStructEnd();
};

/**
 * Returns the application exception code set on the exception.
 * @readonly
 * @returns {Thrift.TApplicationExceptionType} exception code
 */
Thrift.TApplicationException.prototype.getCode = function() {
    return this.code;
};

/**
 * Constructor Function for the XHR transport.
 * If you do not specify a url then you must handle XHR operations on
 * your own. This type can also be constructed using the Transport alias
 * for backward compatibility.
 * @constructor
 * @param {string} [url] - The URL to connect to.
 * @classdesc The Apache Thrift Transport layer performs byte level I/O 
 * between RPC clients and servers. The JavaScript TXHRTransport object 
 * uses Http[s]/XHR. Target servers must implement the http[s] transport
 * (see: node.js example server_http.js).
 * @example
 *     var transport = new Thrift.TXHRTransport("http://localhost:8585");
 */
Thrift.Transport = Thrift.TXHRTransport = function(url, options) {
    this.url = url;
    this.wpos = 0;
    this.rpos = 0;
    this.useCORS = (options && options.useCORS);
    this.send_buf = '';
    this.recv_buf = '';
};

Thrift.TXHRTransport.prototype = {
    /**
     * Gets the browser specific XmlHttpRequest Object.
     * @returns {object} the browser XHR interface object
     */
    getXmlHttpRequestObject: function() {
        try { return new XMLHttpRequest(); } catch (e1) { }
        try { return new ActiveXObject('Msxml2.XMLHTTP'); } catch (e2) { }
        try { return new ActiveXObject('Microsoft.XMLHTTP'); } catch (e3) { }

        throw "Your browser doesn't support XHR.";
    },

    /**
     * Sends the current XRH request if the transport was created with a URL 
     * and the async parameter is false. If the transport was not created with
     * a URL, or the async parameter is True and no callback is provided, or 
     * the URL is an empty string, the current send buffer is returned.
     * @param {object} async - If true the current send buffer is returned.
     * @param {object} callback - Optional async completion callback 
     * @returns {undefined|string} Nothing or the current send buffer.
     * @throws {string} If XHR fails.
     */
    flush: function(async, callback) {
        var self = this;
        if ((async && !callback) || this.url === undefined || this.url === '') {
            return this.send_buf;
        }

        var xreq = this.getXmlHttpRequestObject();

        if (xreq.overrideMimeType) {
            xreq.overrideMimeType('application/vnd.apache.thrift.json; charset=utf-8');
        }

        if (callback) {
            //Ignore XHR callbacks until the data arrives, then call the
            //  client's callback
            xreq.onreadystatechange = 
              (function() {
                var clientCallback = callback;    
                return function() {
                  if (this.readyState == 4 && this.status == 200) {
                    self.setRecvBuffer(this.responseText);
                    clientCallback();
                  }
                };
              }());
        }

        xreq.open('POST', this.url, !!async);

        if (xreq.setRequestHeader) {
            xreq.setRequestHeader('Accept', 'application/vnd.apache.thrift.json; charset=utf-8');
            xreq.setRequestHeader('Content-Type', 'application/vnd.apache.thrift.json; charset=utf-8');
        }

        xreq.send(this.send_buf);
        if (async && callback) {
            return;
        }

        if (xreq.readyState != 4) {
            throw 'encountered an unknown ajax ready state: ' + xreq.readyState;
        }

        if (xreq.status != 200) {
            throw 'encountered a unknown request status: ' + xreq.status;
        }

        this.recv_buf = xreq.responseText;
        this.recv_buf_sz = this.recv_buf.length;
        this.wpos = this.recv_buf.length;
        this.rpos = 0;
    },

    /**
     * Creates a jQuery XHR object to be used for a Thrift server call.
     * @param {object} client - The Thrift Service client object generated by the IDL compiler.
     * @param {object} postData - The message to send to the server.
     * @param {function} args - The original call arguments with the success call back at the end.
     * @param {function} recv_method - The Thrift Service Client receive method for the call.
     * @returns {object} A new jQuery XHR object.
     * @throws {string} If the jQuery version is prior to 1.5 or if jQuery is not found.
     */
    jqRequest: function(client, postData, args, recv_method) {
        if (typeof jQuery === 'undefined' ||
            typeof jQuery.Deferred === 'undefined') {
            throw 'Thrift.js requires jQuery 1.5+ to use asynchronous requests';
        }

        var thriftTransport = this;

        var jqXHR = jQuery.ajax({
            url: this.url,
            data: postData,
            type: 'POST',
            cache: false,
            contentType: 'application/vnd.apache.thrift.json; charset=utf-8',
            dataType: 'text thrift',
            converters: {
                'text thrift' : function(responseData) {
                    thriftTransport.setRecvBuffer(responseData);
                    var value = recv_method.call(client);
                    return value;
                }
            },
            context: client,
            success: jQuery.makeArray(args).pop()
        });

        return jqXHR;
    },

    /**
     * Sets the buffer to provide the protocol when deserializing.
     * @param {string} buf - The buffer to supply the protocol.
     */
    setRecvBuffer: function(buf) {
        this.recv_buf = buf;
        this.recv_buf_sz = this.recv_buf.length;
        this.wpos = this.recv_buf.length;
        this.rpos = 0;
    },

    /**
     * Returns true if the transport is open, XHR always returns true.
     * @readonly
     * @returns {boolean} Always True.
     */    
    isOpen: function() {
        return true;
    },

    /**
     * Opens the transport connection, with XHR this is a nop.
     */    
    open: function() {},

    /**
     * Closes the transport connection, with XHR this is a nop.
     */    
    close: function() {},

    /**
     * Returns the specified number of characters from the response
     * buffer.
     * @param {number} len - The number of characters to return.
     * @returns {string} Characters sent by the server.
     */
    read: function(len) {
        var avail = this.wpos - this.rpos;

        if (avail === 0) {
            return '';
        }

        var give = len;

        if (avail < len) {
            give = avail;
        }

        var ret = this.read_buf.substr(this.rpos, give);
        this.rpos += give;

        //clear buf when complete?
        return ret;
    },

    /**
     * Returns the entire response buffer.
     * @returns {string} Characters sent by the server.
     */
    readAll: function() {
        return this.recv_buf;
    },

    /**
     * Sets the send buffer to buf.
     * @param {string} buf - The buffer to send.
     */    
    write: function(buf) {
        this.send_buf = buf;
    },

    /**
     * Returns the send buffer.
     * @readonly
     * @returns {string} The send buffer.
     */ 
    getSendBuffer: function() {
        return this.send_buf;
    }

};


/**
 * Constructor Function for the WebSocket transport.
 * @constructor
 * @param {string} [url] - The URL to connect to.
 * @classdesc The Apache Thrift Transport layer performs byte level I/O 
 * between RPC clients and servers. The JavaScript TWebSocketTransport object 
 * uses the WebSocket protocol. Target servers must implement WebSocket.
 * (see: node.js example server_http.js).
 * @example
 *   var transport = new Thrift.TWebSocketTransport("http://localhost:8585");
 */
Thrift.TWebSocketTransport = function(url) {
    this.__reset(url);
};

Thrift.TWebSocketTransport.prototype = {
    __reset: function(url) {
      this.url = url;             //Where to connect
      this.socket = null;         //The web socket
      this.callbacks = [];        //Pending callbacks
      this.send_pending = [];     //Buffers/Callback pairs waiting to be sent
      this.send_buf = '';         //Outbound data, immutable until sent
      this.recv_buf = '';         //Inbound data
      this.rb_wpos = 0;           //Network write position in receive buffer
      this.rb_rpos = 0;           //Client read position in receive buffer
    },

    /**
     * Sends the current WS request and registers callback. The async 
     * parameter is ignored (WS flush is always async) and the callback 
     * function parameter is required.
     * @param {object} async - Ignored.
     * @param {object} callback - The client completion callback.
     * @returns {undefined|string} Nothing (undefined) 
     */
    flush: function(async, callback) {
      var self = this;
      if (this.isOpen()) {
        //Send data and register a callback to invoke the client callback
        this.socket.send(this.send_buf); 
        this.callbacks.push((function() {
          var clientCallback = callback;    
          return function(msg) {
            self.setRecvBuffer(msg);
            clientCallback();
          };
        }()));
      } else {
        //Queue the send to go out __onOpen
        this.send_pending.push({
          buf: this.send_buf,
          cb:  callback
        });
      }
    },

    __onOpen: function() { 
       var self = this;
       if (this.send_pending.length > 0) {
          //If the user made calls before the connection was fully 
          //open, send them now
          this.send_pending.forEach(function(elem) {
             this.socket.send(elem.buf);
             this.callbacks.push((function() {
               var clientCallback = elem.cb;    
               return function(msg) {
                  self.setRecvBuffer(msg);
                  clientCallback();
               };
             }()));
          });
          this.send_pending = [];
       }
    },
    
    __onClose: function(evt) { 
      this.__reset(this.url);
    },
     
    __onMessage: function(evt) {
      if (this.callbacks.length) {
        this.callbacks.shift()(evt.data);
      }
    },
     
    __onError: function(evt) { 
      console.log("Thrift WebSocket Error: " + evt.toString());
      this.socket.close();
    },

    /**
     * Sets the buffer to use when receiving server responses.
     * @param {string} buf - The buffer to receive server responses.
     */
    setRecvBuffer: function(buf) {
        this.recv_buf = buf;
        this.recv_buf_sz = this.recv_buf.length;
        this.wpos = this.recv_buf.length;
        this.rpos = 0;
    },

    /**
     * Returns true if the transport is open
     * @readonly
     * @returns {boolean} 
     */    
    isOpen: function() {
        return this.socket && this.socket.readyState == this.socket.OPEN;
    },

    /**
     * Opens the transport connection
     */    
    open: function() {
      //If OPEN/CONNECTING/CLOSING ignore additional opens
      if (this.socket && this.socket.readyState != this.socket.CLOSED) {
        return;
      }
      //If there is no socket or the socket is closed:
      this.socket = new WebSocket(this.url);
      this.socket.onopen = this.__onOpen.bind(this); 
      this.socket.onmessage = this.__onMessage.bind(this); 
      this.socket.onerror = this.__onError.bind(this); 
      this.socket.onclose = this.__onClose.bind(this); 
    },

    /**
     * Closes the transport connection
     */    
    close: function() {
      this.socket.close();
    },

    /**
     * Returns the specified number of characters from the response
     * buffer.
     * @param {number} len - The number of characters to return.
     * @returns {string} Characters sent by the server.
     */
    read: function(len) {
        var avail = this.wpos - this.rpos;

        if (avail === 0) {
            return '';
        }

        var give = len;

        if (avail < len) {
            give = avail;
        }

        var ret = this.read_buf.substr(this.rpos, give);
        this.rpos += give;

        //clear buf when complete?
        return ret;
    },

    /**
     * Returns the entire response buffer.
     * @returns {string} Characters sent by the server.
     */
    readAll: function() {
        return this.recv_buf;
    },

    /**
     * Sets the send buffer to buf.
     * @param {string} buf - The buffer to send.
     */    
    write: function(buf) {
        this.send_buf = buf;
    },

    /**
     * Returns the send buffer.
     * @readonly
     * @returns {string} The send buffer.
     */ 
    getSendBuffer: function() {
        return this.send_buf;
    }

};

/**
 * Initializes a Thrift JSON protocol instance.
 * @constructor
 * @param {Thrift.Transport} transport - The transport to serialize to/from.
 * @classdesc Apache Thrift Protocols perform serialization which enables cross 
 * language RPC. The Protocol type is the JavaScript browser implementation 
 * of the Apache Thrift TJSONProtocol.
 * @example
 *     var protocol  = new Thrift.Protocol(transport);
 */
Thrift.TJSONProtocol = Thrift.Protocol = function(transport) {
    this.tstack = [];
    this.tpos = [];
    this.transport = transport;
};

/**
 * Thrift IDL type Id to string mapping.
 * @readonly
 * @see {@link Thrift.Type}
 */
Thrift.Protocol.Type = {};
Thrift.Protocol.Type[Thrift.Type.BOOL] = '"tf"';
Thrift.Protocol.Type[Thrift.Type.BYTE] = '"i8"';
Thrift.Protocol.Type[Thrift.Type.I16] = '"i16"';
Thrift.Protocol.Type[Thrift.Type.I32] = '"i32"';
Thrift.Protocol.Type[Thrift.Type.I64] = '"i64"';
Thrift.Protocol.Type[Thrift.Type.DOUBLE] = '"dbl"';
Thrift.Protocol.Type[Thrift.Type.STRUCT] = '"rec"';
Thrift.Protocol.Type[Thrift.Type.STRING] = '"str"';
Thrift.Protocol.Type[Thrift.Type.MAP] = '"map"';
Thrift.Protocol.Type[Thrift.Type.LIST] = '"lst"';
Thrift.Protocol.Type[Thrift.Type.SET] = '"set"';

/**
 * Thrift IDL type string to Id mapping.
 * @readonly
 * @see {@link Thrift.Type}
 */
Thrift.Protocol.RType = {};
Thrift.Protocol.RType.tf = Thrift.Type.BOOL;
Thrift.Protocol.RType.i8 = Thrift.Type.BYTE;
Thrift.Protocol.RType.i16 = Thrift.Type.I16;
Thrift.Protocol.RType.i32 = Thrift.Type.I32;
Thrift.Protocol.RType.i64 = Thrift.Type.I64;
Thrift.Protocol.RType.dbl = Thrift.Type.DOUBLE;
Thrift.Protocol.RType.rec = Thrift.Type.STRUCT;
Thrift.Protocol.RType.str = Thrift.Type.STRING;
Thrift.Protocol.RType.map = Thrift.Type.MAP;
Thrift.Protocol.RType.lst = Thrift.Type.LIST;
Thrift.Protocol.RType.set = Thrift.Type.SET;

/**
 * The TJSONProtocol version number.
 * @readonly
 * @const {number} Version
 * @memberof Thrift.Protocol
 */
 Thrift.Protocol.Version = 1;

Thrift.Protocol.prototype = {
    /**
     * Returns the underlying transport.
     * @readonly
     * @returns {Thrift.Transport} The underlying transport.
     */ 
    getTransport: function() {
        return this.transport;
    },

    /**
     * Serializes the beginning of a Thrift RPC message.
     * @param {string} name - The service method to call.
     * @param {Thrift.MessageType} messageType - The type of method call.
     * @param {number} seqid - The sequence number of this call (always 0 in Apache Thrift).
     */
    writeMessageBegin: function(name, messageType, seqid) {
        this.tstack = [];
        this.tpos = [];

        this.tstack.push([Thrift.Protocol.Version, '"' +
            name + '"', messageType, seqid]);
    },

    /**
     * Serializes the end of a Thrift RPC message.
     */
    writeMessageEnd: function() {
        var obj = this.tstack.pop();

        this.wobj = this.tstack.pop();
        this.wobj.push(obj);

        this.wbuf = '[' + this.wobj.join(',') + ']';

        this.transport.write(this.wbuf);
     },


    /**
     * Serializes the beginning of a struct.
     * @param {string} name - The name of the struct.
     */
    writeStructBegin: function(name) {
        this.tpos.push(this.tstack.length);
        this.tstack.push({});
    },

    /**
     * Serializes the end of a struct.
     */
    writeStructEnd: function() {

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
    },

    /**
     * Serializes the beginning of a struct field.
     * @param {string} name - The name of the field.
     * @param {Thrift.Protocol.Type} fieldType - The data type of the field.
     * @param {number} fieldId - The field's unique identifier.
     */
    writeFieldBegin: function(name, fieldType, fieldId) {
        this.tpos.push(this.tstack.length);
        this.tstack.push({ 'fieldId': '"' +
            fieldId + '"', 'fieldType': Thrift.Protocol.Type[fieldType]
        });

    },

    /**
     * Serializes the end of a field.
     */
    writeFieldEnd: function() {
        var value = this.tstack.pop();
        var fieldInfo = this.tstack.pop();

        this.tstack[this.tstack.length - 1][fieldInfo.fieldId] = '{' +
            fieldInfo.fieldType + ':' + value + '}';
        this.tpos.pop();
    },

    /**
     * Serializes the end of the set of fields for a struct.
     */
    writeFieldStop: function() {
        //na
    },

    /**
     * Serializes the beginning of a map collection.
     * @param {Thrift.Type} keyType - The data type of the key.
     * @param {Thrift.Type} valType - The data type of the value.
     * @param {number} [size] - The number of elements in the map (ignored).
     */
    writeMapBegin: function(keyType, valType, size) {
        this.tpos.push(this.tstack.length);
        this.tstack.push([Thrift.Protocol.Type[keyType],
            Thrift.Protocol.Type[valType], 0]);
    },

    /**
     * Serializes the end of a map.
     */
    writeMapEnd: function() {
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
    },

    /**
     * Serializes the beginning of a list collection.
     * @param {Thrift.Type} elemType - The data type of the elements.
     * @param {number} size - The number of elements in the list.
     */
    writeListBegin: function(elemType, size) {
        this.tpos.push(this.tstack.length);
        this.tstack.push([Thrift.Protocol.Type[elemType], size]);
    },

    /**
     * Serializes the end of a list.
     */
    writeListEnd: function() {
        var p = this.tpos.pop();

        while (this.tstack.length > p + 1) {
            var tmpVal = this.tstack[p + 1];
            this.tstack.splice(p + 1, 1);
            this.tstack[p].push(tmpVal);
        }

        this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
    },

    /**
     * Serializes the beginning of a set collection.
     * @param {Thrift.Type} elemType - The data type of the elements.
     * @param {number} size - The number of elements in the list.
     */
    writeSetBegin: function(elemType, size) {
        this.tpos.push(this.tstack.length);
        this.tstack.push([Thrift.Protocol.Type[elemType], size]);
    },

    /**
     * Serializes the end of a set.
     */
    writeSetEnd: function() {
        var p = this.tpos.pop();

        while (this.tstack.length > p + 1) {
            var tmpVal = this.tstack[p + 1];
            this.tstack.splice(p + 1, 1);
            this.tstack[p].push(tmpVal);
        }

        this.tstack[p] = '[' + this.tstack[p].join(',') + ']';
    },

    /** Serializes a boolean */
    writeBool: function(value) {
        this.tstack.push(value ? 1 : 0);
    },

    /** Serializes a number */
    writeByte: function(i8) {
        this.tstack.push(i8);
    },

    /** Serializes a number */
    writeI16: function(i16) {
        this.tstack.push(i16);
    },

    /** Serializes a number */
    writeI32: function(i32) {
        this.tstack.push(i32);
    },

    /** Serializes a number */
    writeI64: function(i64) {
        this.tstack.push(i64);
    },

    /** Serializes a number */
    writeDouble: function(dbl) {
        this.tstack.push(dbl);
    },

    /** Serializes a string */
    writeString: function(str) {
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
                } else if (ch === '\\') {    // a single backslash
                    escapedString += '\\\\'; // write out as double backslash 
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
    },

    /** Serializes a string */
    writeBinary: function(str) {
        this.writeString(str);
    },

    /**
       @class
       @name AnonReadMessageBeginReturn
       @property {string} fname - The name of the service method.
       @property {Thrift.MessageType} mtype - The type of message call.
       @property {number} rseqid - The sequence number of the message (0 in Thrift RPC).
     */
    /** 
     * Deserializes the beginning of a message. 
     * @returns {AnonReadMessageBeginReturn}
     */
    readMessageBegin: function() {
        this.rstack = [];
        this.rpos = [];

        if (typeof JSON !== 'undefined' && typeof JSON.parse === 'function') {
            this.robj = JSON.parse(this.transport.readAll());
        } else if (typeof jQuery !== 'undefined') {
            this.robj = jQuery.parseJSON(this.transport.readAll());
        } else {
            this.robj = eval(this.transport.readAll());
        }

        var r = {};
        var version = this.robj.shift();

        if (version != Thrift.Protocol.Version) {
            throw 'Wrong thrift protocol version: ' + version;
        }

        r.fname = this.robj.shift();
        r.mtype = this.robj.shift();
        r.rseqid = this.robj.shift();


        //get to the main obj
        this.rstack.push(this.robj.shift());

        return r;
    },

    /** Deserializes the end of a message. */
    readMessageEnd: function() {
    },

    /** 
     * Deserializes the beginning of a struct. 
     * @param {string} [name] - The name of the struct (ignored)
     * @returns {object} - An object with an empty string fname property
     */    
    readStructBegin: function(name) {
        var r = {};
        r.fname = '';

        //incase this is an array of structs
        if (this.rstack[this.rstack.length - 1] instanceof Array) {
            this.rstack.push(this.rstack[this.rstack.length - 1].shift());
        }

        return r;
    },

    /** Deserializes the end of a struct. */
    readStructEnd: function() {
        if (this.rstack[this.rstack.length - 2] instanceof Array) {
            this.rstack.pop();
        }
    },

    /**
       @class
       @name AnonReadFieldBeginReturn
       @property {string} fname - The name of the field (always '').
       @property {Thrift.Type} ftype - The data type of the field.
       @property {number} fid - The unique identifier of the field.
     */
    /** 
     * Deserializes the beginning of a field. 
     * @returns {AnonReadFieldBeginReturn}
     */
    readFieldBegin: function() {
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
                if (Thrift.Protocol.RType[i] === null) {
                    continue;
                }

                ftype = Thrift.Protocol.RType[i];
                this.rstack[this.rstack.length - 1] =
                    this.rstack[this.rstack.length - 1][i];
            }
        }

        r.fname = '';
        r.ftype = ftype;
        r.fid = fid;

        return r;
    },

    /** Deserializes the end of a field. */
    readFieldEnd: function() {
        var pos = this.rpos.pop();

        //get back to the right place in the stack
        while (this.rstack.length > pos) {
            this.rstack.pop();
        }

    },

    /**
       @class
       @name AnonReadMapBeginReturn
       @property {Thrift.Type} ktype - The data type of the key.
       @property {Thrift.Type} vtype - The data type of the value.
       @property {number} size - The number of elements in the map.
     */
    /** 
     * Deserializes the beginning of a map. 
     * @returns {AnonReadMapBeginReturn}
     */
    readMapBegin: function() {
        var map = this.rstack.pop();
        var first = map.shift();
        if (first instanceof Array) {
          this.rstack.push(map);
          map = first;
          first = map.shift();
        }

        var r = {};
        r.ktype = Thrift.Protocol.RType[first];
        r.vtype = Thrift.Protocol.RType[map.shift()];
        r.size = map.shift();


        this.rpos.push(this.rstack.length);
        this.rstack.push(map.shift());

        return r;
    },

    /** Deserializes the end of a map. */
    readMapEnd: function() {
        this.readFieldEnd();
    },

    /**
       @class
       @name AnonReadColBeginReturn
       @property {Thrift.Type} etype - The data type of the element.
       @property {number} size - The number of elements in the collection.
     */
    /** 
     * Deserializes the beginning of a list. 
     * @returns {AnonReadColBeginReturn}
     */
    readListBegin: function() {
        var list = this.rstack[this.rstack.length - 1];

        var r = {};
        r.etype = Thrift.Protocol.RType[list.shift()];
        r.size = list.shift();

        this.rpos.push(this.rstack.length);
        this.rstack.push(list);

        return r;
    },

    /** Deserializes the end of a list. */
    readListEnd: function() {
        this.readFieldEnd();
    },

    /** 
     * Deserializes the beginning of a set. 
     * @returns {AnonReadColBeginReturn}
     */
    readSetBegin: function(elemType, size) {
        return this.readListBegin(elemType, size);
    },

    /** Deserializes the end of a set. */
    readSetEnd: function() {
        return this.readListEnd();
    },

    /** Returns an object with a value property set to 
     *  False unless the next number in the protocol buffer 
     *  is 1, in which case the value property is True */
    readBool: function() {
        var r = this.readI32();

        if (r !== null && r.value == '1') {
            r.value = true;
        } else {
            r.value = false;
        }

        return r;
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readByte: function() {
        return this.readI32();
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readI16: function() {
        return this.readI32();
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readI32: function(f) {
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

        return r;
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readI64: function() {
        return this.readI32();
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readDouble: function() {
        return this.readI32();
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readString: function() {
        var r = this.readI32();
        return r;
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readBinary: function() {
        return this.readString();
    },

    /** 
     * Method to arbitrarily skip over data */
    skip: function(type) {
        var ret, i;
        switch (type) {
            case Thrift.Type.STOP:
                return null;

            case Thrift.Type.BOOL:
                return this.readBool();

            case Thrift.Type.BYTE:
                return this.readByte();

            case Thrift.Type.I16:
                return this.readI16();

            case Thrift.Type.I32:
                return this.readI32();

            case Thrift.Type.I64:
                return this.readI64();

            case Thrift.Type.DOUBLE:
                return this.readDouble();

            case Thrift.Type.STRING:
                return this.readString();

            case Thrift.Type.STRUCT:
                this.readStructBegin();
                while (true) {
                    ret = this.readFieldBegin();
                    if (ret.ftype == Thrift.Type.STOP) {
                        break;
                    }
                    this.skip(ret.ftype);
                    this.readFieldEnd();
                }
                this.readStructEnd();
                return null;

            case Thrift.Type.MAP:
                ret = this.readMapBegin();
                for (i = 0; i < ret.size; i++) {
                    if (i > 0) {
                        if (this.rstack.length > this.rpos[this.rpos.length - 1] + 1) {
                            this.rstack.pop();
                        }
                    }
                    this.skip(ret.ktype);
                    this.skip(ret.vtype);
                }
                this.readMapEnd();
                return null;

            case Thrift.Type.SET:
                ret = this.readSetBegin();
                for (i = 0; i < ret.size; i++) {
                    this.skip(ret.etype);
                }
                this.readSetEnd();
                return null;

            case Thrift.Type.LIST:
                ret = this.readListBegin();
                for (i = 0; i < ret.size; i++) {
                    this.skip(ret.etype);
                }
                this.readListEnd();
                return null;
        }
    }
};


/**
 * Initializes a MutilplexProtocol Implementation as a Wrapper for Thrift.Protocol
 * @constructor
 */
Thrift.MultiplexProtocol = function (srvName, trans, strictRead, strictWrite) {
    Thrift.Protocol.call(this, trans, strictRead, strictWrite);
    this.serviceName = srvName;
};
Thrift.inherits(Thrift.MultiplexProtocol, Thrift.Protocol, 'multiplexProtocol');

/** Override writeMessageBegin method of prototype*/
Thrift.MultiplexProtocol.prototype.writeMessageBegin = function (name, type, seqid) {

    if (type === Thrift.MessageType.CALL || type === Thrift.MessageType.ONEWAY) {
        Thrift.Protocol.prototype.writeMessageBegin.call(this, this.serviceName + ":" + name, type, seqid);
    } else {
        Thrift.Protocol.prototype.writeMessageBegin.call(this, name, type, seqid);
    }
};

Thrift.Multiplexer = function () {
    this.seqid = 0;
};

/** Instantiates a multiplexed client for a specific service
 * @constructor
 * @param {String} serviceName - The transport to serialize to/from.
 * @param {Thrift.ServiceClient} SCl - The Service Client Class
 * @param {Thrift.Transport} transport - Thrift.Transport instance which provides remote host:port
 * @example
 *    var mp = new Thrift.Multiplexer();
 *    var transport = new Thrift.Transport("http://localhost:9090/foo.thrift");
 *    var protocol = new Thrift.Protocol(transport);
 *    var client = mp.createClient('AuthService', AuthServiceClient, transport);
*/
Thrift.Multiplexer.prototype.createClient = function (serviceName, SCl, transport) {
    if (SCl.Client) {
        SCl = SCl.Client;
    }
    var self = this;
    SCl.prototype.new_seqid = function () {
        self.seqid += 1;
        return self.seqid;
    };
    var client = new SCl(new Thrift.MultiplexProtocol(serviceName, transport));

    return client;
};

/**
 * Initializes a TBinaryProtocol Implementation as a Wrapper for Thrift.Protocol
 * @constructor
 * @param {Thrift.Transport} transport - The transport to serialize to/from.
 * @param {boolean} stringRead - indicates strict read.
 * @param {boolean} stringWrite - indicates strict write.
 * @classdesc Apache Thrift Protocols perform serialization which enables cross 
 * language RPC. The Protocol type is the JavaScript browser implementation 
 * of the Apache Thrift TBinaryProtocol.
 * @example
 *     var protocol  = new Thrift.TBinaryProtocol(transport, { strictRead: true, strictWrite: true });
 */
Thrift.TBinaryProtocol = function(transport, strictRead, strictWrite) {
    this.buffer = [];
    this.transport = transport;
    this.buffer_read_offset = 0;
    this.strictRead = (strictRead !== undefined ? strictRead : false);
    this.strictWrite = (strictWrite !== undefined ? strictWrite : false);
};
Thrift.inherits(Thrift.TBinaryProtocol, Thrift.Protocol, 'binaryProtocol');

Thrift.TBinaryProtocol.VERSION_MASK = 0xffff0000;
Thrift.TBinaryProtocol.VERSION_1 = 0x80010000;
Thrift.TBinaryProtocol.TYPE_MASK = 0x000000ff;

Thrift.TBinaryProtocol.prototype = {

    /**
     * Serializes the beginning of a Thrift RPC message.
     * @param {string} name - The service method to call.
     * @param {Thrift.MessageType} messageType - The type of method call.
     * @param {number} seqid - The sequence number of this call (always 0 in Apache Thrift).
     */
    writeMessageBegin: function(name, type, seqid) {
        if (this.strictWrite) {
            this.writeI16(VERSION_1 >> 16);
            this.writeI16(type);
            this.writeString(name);
            this.writeI32(seqid);
        } else {
            this.writeString(name);
            this.writeByte(type);
            this.writeI32(seqid);
        }
    },

    /**
     * Serializes the end of a Thrift RPC message.
     */
    writeMessageEnd: function() {},

    /**
     * Serializes the beginning of a struct.
     * @param {string} name - The name of the struct.
     */
    writeStructBegin: function(name) {},

    /**
     * Serializes the end of a struct.
     */
    writeStructEnd: function() {},

    /**
     * Serializes the beginning of a struct field.
     * @param {string} name - The name of the field.
     * @param {Thrift.Protocol.Type} fieldType - The data type of the field.
     * @param {number} fieldId - The field's unique identifier.
     */
    writeFieldBegin: function(name, type, id) {
        this.writeByte(type);
        this.writeI16(id);
    },

    /**
     * Serializes the end of a field.
     */
    writeFieldEnd: function() {},
    
    /**
     * Serializes the end of the set of fields for a struct.
     */
    writeFieldStop: function() {
        this.writeByte(Thrift.Type.STOP);
    },

    /**
     * Serializes the beginning of a map collection.
     * @param {Thrift.Type} keyType - The data type of the key.
     * @param {Thrift.Type} valType - The data type of the value.
     * @param {number} [size] - The number of elements in the map (ignored).
     */
    writeMapBegin: function(ktype, vtype, size) {
        this.writeByte(ktype);
        this.writeByte(vtype);
        this.writeI32(size);
    },

    /**
     * Serializes the end of a map.
     */
    writeMapEnd: function() {},

    /**
     * Serializes the beginning of a list collection.
     * @param {Thrift.Type} elemType - The data type of the elements.
     * @param {number} size - The number of elements in the list.
     */
    writeListBegin: function(etype, size) {
        this.writeByte(etype);
        this.writeI32(size);
    },

    /**
     * Serializes the end of a list.
     */
    writeListEnd: function() {},

    /**
     * Serializes the beginning of a set collection.
     * @param {Thrift.Type} elemType - The data type of the elements.
     * @param {number} size - The number of elements in the list.
     */
    writeSetBegin: function(etype, size) {
        this.writeByte(etype);
        this.writeI32(size);
    },

    /**
     * Serializes the end of a set.
     */
    writeSetEnd: function() {},

    /** Serializes a boolean */
    writeBool: function(bool) {
        this.writeByte(bool ? 1 : 0);
    },

    /** Serializes a number */
    writeByte: function(byte) {
        if ( byte <= Math.pow(2,31)*-1 || byte >= Math.pow(2,31) ) {
          throw new Error(byte + " is incorrect for byte.");
        }
        this.buffer.push(byte);
    },

    /** Serializes a number (short) */
    writeI16: function(i16) {
        if ( i16 < Math.pow(2,15)*-1 || i16 >= Math.pow(2,16) ) {
          throw new Error(i16 + " is incorrect for i16.");
        }
        this.buffer.push( 255 & i16 >> 8 );
        this.buffer.push( 255 & i16 );
    },

    /** Serializes a number (int) */
    writeI32: function(i32) {
        if ( i32 <= Math.pow(2,31)*-1 || i32 >= Math.pow(2,31) ) {
          throw new Error(i32 + " is incorrect for i32.");
        }
        this.buffer.push(255 & i32 >> 24);
        this.buffer.push(255 & i32 >> 16);
        this.buffer.push(255 & i32 >> 8);
        this.buffer.push(255 & i32);
    },

    /** Serializes a number (long, for values over MAX_INTEGER, it will overflow) */
    writeI64: function(i64) {
        if ( i64 <= Math.pow(2,63)*-1 || i64 >= Math.pow(2,64) ) {
          throw new Error(i64 + " is incorrect for i64.");
        }
        // Although this is a correct way of packing a long int,
        // the value will overflow if the number is higher than max int
        var hi = (i64 & 0xffffffff00000000 >> 32);
        var lo = i64 & 0x00000000ffffffff;
        this.writeI32(hi);
        this.writeI32(lo);
    },

    /** Serializes a number (double IEEE-754) */
    writeDouble: function(dub) {
        // The code obtained from here: http://cautionsingularityahead.blogspot.nl/2010/04/javascript-and-ieee754-redux.html
        // According to the comments by the author, this code has been included in an external library
        // and it's available under MIT license.
        var ebits = 11;
        var fbits = 52;
        var bias = (1 << (ebits - 1)) - 1;
        // Compute sign, exponent, fraction
        var s, e, f;
        if (isNaN(dub)) {
            e = (1 << bias) - 1; f = 1; s = 0;
        }
        else if (dub === Infinity || dub === -Infinity) {
            e = (1 << bias) - 1; f = 0; s = (dub < 0) ? 1 : 0;
        }
        else if (dub === 0) {
            e = 0; f = 0; s = (1 / dub === -Infinity) ? 1 : 0;
        }
        else {
            s = dub < 0;
            dub = Math.abs(dub);
            if (dub >= Math.pow(2, 1 - bias)) {
                var ln = Math.min(Math.floor(Math.log(dub) / Math.LN2), bias);
                e = ln + bias;
                f = dub * Math.pow(2, fbits - ln) - Math.pow(2, fbits);
            } else {
                e = 0;
                f = dub / Math.pow(2, 1 - bias - fbits);
            }
        }
        
        // Pack sign, exponent, fraction
        var i, bits = [];
        for (i = fbits; i; i -= 1) { bits.push(f % 2 ? 1 : 0); f = Math.floor(f / 2); }
        for (i = ebits; i; i -= 1) { bits.push(e % 2 ? 1 : 0); e = Math.floor(e / 2); }
        bits.push(s ? 1 : 0);
        bits.reverse();
        var str = bits.join('');
        
        // Bits to bytes
        while (str.length) {
            this.buffer.push(parseInt(str.substring(0, 8), 2));
            str = str.substring(8);
        }
    },

    /** Serializes a string */
    writeString: function(str) {
        var s = this.encode_utf8(str);
        var buf = new ArrayBuffer(s.length);
        var bufView = new Uint8Array(buf);
        this.writeI32( s.length );
        for (var i=0, strLen=s.length; i<strLen; i++) {
            this.buffer.push( s.charCodeAt(i) );
        }
    },

    /** Serializes abritrary array of bytes */
    writeBinary: function(buf) {
      this.writeI32(buf.length);
      for ( var i=0; i<buf.length; i++ ) {
        this.buffer.push( b );
      }
    },

    /**
       @class
       @name AnonReadMessageBeginReturn
       @property {string} fname - The name of the service method.
       @property {Thrift.MessageType} mtype - The type of message call.
       @property {number} rseqid - The sequence number of the message (0 in Thrift RPC).
     */
    /** 
     * Deserializes the beginning of a message. 
     * @returns {AnonReadMessageBeginReturn}
     */
    readMessageBegin: function() {
        var version = this.readI32().value;
        var name, type, seqid;
        if (version < 0) {
            if (version & Thrift.BinaryProtocol.VERSION_MASK != Thrift.BinaryProtocol.VERSION_1) {
                throw new Thrift.TException('Missing version identifier');
            }
            type = version & Thrift.BinaryProtocol.TYPE_MASK;
            name = this.readString().value;
            seqid = this.readI32().value;
            return { fname: name, mtype: type, rseqid: seqid };
        } else {
            if (this.strictRead) {
                throw new Thrift.TException('No version identifier, old protocol client?');
            }
            name = this.readMultipleAsString(version);
            type = this.readByte().value;
            seqid = this.readI32().value;
            return { fname: name, mtype: type, rseqid: seqid };
        }
    },

    /** Deserializes the end of a message. */
    readMessageEnd: function() {
    },

    /** 
     * Deserializes the beginning of a struct. 
     * @param {string} [name] - The name of the struct (ignored)
     * @returns {object} - Not supported in binary protocol
     */    
    readStructBegin: function(name) {
      return { fname: '' };
    },

    /** Deserializes the end of a struct. */
    readStructEnd: function() {
    },

    /**
       @class
       @name AnonReadFieldBeginReturn
       @property {string} fname - The name of the field (always '').
       @property {Thrift.Type} ftype - The data type of the field.
       @property {number} fid - The unique identifier of the field.
     */
    /** 
     * Deserializes the beginning of a field. 
     * @returns {AnonReadFieldBeginReturn}
     */
    readFieldBegin: function() {
        var type = this.readByte().value;
        if (type == Thrift.Type.STOP) {
          return { fname: '', ftype: type, fid: 0 };
        } else {
          return { fname: '', ftype: type, fid: this.readI16().value };
        }
    },

    /** Deserializes the end of a field. */
    readFieldEnd: function() {
      return { value: '' };
    },

    /**
       @class
       @name AnonReadMapBeginReturn
       @property {Thrift.Type} ktype - The data type of the key.
       @property {Thrift.Type} vtype - The data type of the value.
       @property {number} size - The number of elements in the map.
     */
    /** 
     * Deserializes the beginning of a map. 
     * @returns {AnonReadMapBeginReturn}
     */
    readMapBegin: function() {
        var ktype = this.readByte().value;
        var vtype = this.readByte().value;
        var size  = this.readI32().value;
        return { ktype: ktype, vtype: vtype, size: size };
    },

    /** Deserializes the end of a map. */
    readMapEnd: function() {
        return this.readFieldEnd().value;
    },

    /**
       @class
       @name AnonReadColBeginReturn
       @property {Thrift.Type} etype - The data type of the element.
       @property {number} size - The number of elements in the collection.
     */
    /** 
     * Deserializes the beginning of a list. 
     * @returns {AnonReadColBeginReturn}
     */
    readListBegin: function() {
        var etype = this.readByte().value;
        var size = this.readI32().value;
        return { etype: etype, size: size };
    },

    /** Deserializes the end of a list. */
    readListEnd: function() {
        return this.readFieldEnd().value;
    },

    /** 
     * Deserializes the beginning of a set. 
     * @returns {AnonReadColBeginReturn}
     */
    readSetBegin: function() {
        var etype = this.readByte().value;
        var size = this.readI32().value;
        return { etype: etype, size: size };
    },

    /** Deserializes the end of a set. */
    readSetEnd: function() {
        return this.readFieldEnd().value;
    },

    /** Returns an object with a value property set to 
     *  False unless the next number in the protocol buffer 
     *  is 1, in which case the value property is True */
    readBool: function() {
        var byte = this.readByte().value;
        return { value: (byte !== 0) };
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readByte: function() {
        var val = this.buffer[this.buffer_read_offset++];
        if (val > 0x7f) {
          val = 0 - ((val - 1) ^ 0xff);
        }
        return { value: val };
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readI16: function() {
        return { value: ( (this.readByte().value & 255) << 8 | this.readByte().value & 255 ) };
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readI32: function() {
        return { value: ( (this.readByte().value & 255) << 24 | (this.readByte().value & 255) << 16 | (this.readByte().value & 255) << 8 | this.readByte().value & 255 ) };
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readI64: function() {
        // Although this is a correct way of packing a long int,
        // the value will overflow if the number is higher than max int
        var i32_1 = this.readI32().value;
        var i32_2 = this.readI32().value;
        return { value: (i32_1 << 32 | i32_2) };
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readDouble: function() {
        // The code obtained from here: http://cautionsingularityahead.blogspot.nl/2010/04/javascript-and-ieee754-redux.html
        // According to the comments by the author, this code has been included in an external library
        // and it's available under MIT license.
        var ebits = 11;
        var fbits = 52;
        var bytes = this.readMultiple(8);
        // Bytes to bits
        var bits = [];
        for (var i = bytes.length; i; i -= 1) {
            var byte = bytes[i - 1];
            for (var j = 8; j; j -= 1) {
                bits.push(byte % 2 ? 1 : 0); byte = byte >> 1;
            }
        }
        bits.reverse();
        var str = bits.join('');
        // Unpack sign, exponent, fraction
        var bias = (1 << (ebits - 1)) - 1;
        var s = parseInt(str.substring(0, 1), 2) ? -1 : 1;
        var e = parseInt(str.substring(1, 1 + ebits), 2);
        var f = parseInt(str.substring(1 + ebits), 2);
        // Produce number
        if (e === (1 << ebits) - 1) {
            return { value: (f !== 0 ? NaN : s * Infinity) };
        } else if (e > 0) {
            return { value: (s * Math.pow(2, e - bias) * (1 + f / Math.pow(2, fbits))) };
        } else if (f !== 0) {
            return { value: (s * Math.pow(2, -(bias-1)) * (f / Math.pow(2, fbits))) };
        } else {
            return { value: (s * 0) };
        }
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readString: function() {
        var size = this.readI32().value;
        var bytes = this.readMultiple(size);
        return { value: this.decode_utf8( this.stringFromByteArray(bytes) ) };
    },

    readBinary: function() {
      var size = this.readI32().value;
      return { value: this.readMultiple( size ) };
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readMultipleAsString: function(len) {
        var bytes = this.readMultiple(len);
        return this.decode_utf8( this.stringFromByteArray(bytes) );
    },

    /** Returns the an object with a value property set to the 
        next value found in the protocol buffer */
    readMultiple: function(len) {
        var buf = [];
        for (var i=0; i<len; i++) {
            buf.push( this.readByte().value );
        }
        return buf;
    },

    /** 
     * Method to arbitrarily skip over data */
    skip: function(type) {
        var ret, i;
        switch (type) {
            case Thrift.Type.STOP:
                return null;
            case Thrift.Type.BOOL:
                return this.readBool();
            case Thrift.Type.BYTE:
                return this.readByte();
            case Thrift.Type.I16:
                return this.readI16();
            case Thrift.Type.I32:
                return this.readI32();
            case Thrift.Type.I64:
                return this.readI64();
            case Thrift.Type.DOUBLE:
                return this.readDouble();
            case Thrift.Type.STRING:
                return this.readString();
            case Thrift.Type.STRUCT:
                this.readStructBegin();
                while (true) {
                    ret = this.readFieldBegin();
                    if (ret.ftype == Thrift.Type.STOP) {
                        break;
                    }
                    this.skip(ret.ftype);
                    this.readFieldEnd();
                }
                this.readStructEnd();
                return null;
            case Thrift.Type.MAP:
                ret = this.readMapBegin();
                for (i = 0; i < ret.size; i++) {
                    if (i > 0) {
                        if (this.rstack.length > this.rpos[this.rpos.length - 1] + 1) {
                            this.rstack.pop();
                        }
                    }
                    this.skip(ret.ktype);
                    this.skip(ret.vtype);
                }
                this.readMapEnd();
                return null;
            case Thrift.Type.SET:
                ret = this.readSetBegin();
                for (i = 0; i < ret.size; i++) {
                    this.skip(ret.etype);
                }
                this.readSetEnd();
                return null;
            case Thrift.Type.LIST:
                ret = this.readListBegin();
                for (i = 0; i < ret.size; i++) {
                    this.skip(ret.etype);
                }
                this.readListEnd();
                return null;
        }
    },

    // utils:

    encode_utf8: function(s) {
        return unescape(encodeURIComponent(s));
    },
    decode_utf8: function(s) {
        return decodeURIComponent(escape(s));
    },
    stringFromByteArray: function(barr) {
      try {
        return String.fromCharCode.apply(null, new Uint8Array(barr));
      } catch (e) {
        return String.fromCharCode.apply(null, barr);
      }
    }
};


