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

package org.apache.thrift.transport;


import haxe.Timer;
import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import haxe.io.BytesOutput;
import haxe.io.BytesInput;

import haxe.Http;

#if js
import js.lib.Promise;
#end


/**
* HTTP implementation of the TTransport interface. Used for working with a
* Thrift web services implementation.
*/

class THttpClient extends TEndpointTransport {

    private var requestBuffer_  : BytesOutput = new BytesOutput();
    private var responseBuffer_ : BytesInput = null;

    private var request_        : Http = null;


    public function new( requestUrl : String, config : TConfiguration = null) : Void {
		super(config);
		
		request_ = new Http(requestUrl);
        request_.addHeader( "Content-Type", "application/x-thrift");
    }


    public override function open() : Void {
		ResetConsumedMessageSize();
    }

    public override function close() : Void {
    }

    public override function isOpen() : Bool {
		return true;
    }

    public override function read(buf:BytesBuffer, off : Int, len : Int) : Int {
        if (responseBuffer_ == null) {
            throw new TTransportException(TTransportException.UNKNOWN, "Response buffer is empty, no request.");
        }

        var data =Bytes.alloc(len);
        len = responseBuffer_.readBytes(data, off, len);
        buf.addBytes(data,0,len);
        CountConsumedMessageBytes(len);
        return len;
    }

    public override function write(buf:Bytes, off : Int, len : Int) : Void {
      requestBuffer_.writeBytes(buf, off, len);
    }


    public override function flush(callback:Dynamic->Void = null) : Void {
        var buffer = requestBuffer_;
        requestBuffer_ = new BytesOutput();
        responseBuffer_ = null;
		ResetConsumedMessageSize();

		/*
        request_.onData = function(data : String) {
			var tmp = new BytesBuffer();
			tmp.addString(data);
			responseBuffer_ = new BytesInput(tmp.getBytes());
			if( callback != null) {
				callback(null);
        };
		*/

		request_.onBytes = function(data : Bytes) {
			responseBuffer_ = new BytesInput(data);
			if( callback != null) {
				callback(null);
			}
		};

		request_.onError = function(msg : String) {
			if( callback != null) {
				callback(new TTransportException(TTransportException.UNKNOWN, "IOError: " + msg));
			}
		};
		
		
		// the request
		request_.setPostBytes(buffer.getBytes());
		request_.request(true/*POST*/);
		
    }

}

    