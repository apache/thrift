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

	
import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import haxe.io.BytesOutput;
import haxe.io.BytesInput;

#if openfl
// OpenFL all targets
import openfl.errors.EOFError;
import openfl.events.Event;
import openfl.events.IOErrorEvent;
import openfl.events.SecurityErrorEvent;
import openfl.net.URLLoader;
import openfl.net.URLLoaderDataFormat;
import openfl.net.URLRequest;
import openfl.net.URLRequestMethod;
#elseif flash  
// Haxe flash, no OpenFL
import flash.errors.EOFError;
import flash.events.Event;
import flash.events.IOErrorEvent;
import flash.events.SecurityErrorEvent;
import flash.net.URLLoader;
import flash.net.URLLoaderDataFormat;
import flash.net.URLRequest;
import flash.net.URLRequestMethod;
#else
// bare Haxe 
import haxe.Http;
#end


	
/**
* HTTP implementation of the TTransport interface. Used for working with a
* Thrift web services implementation.
*/
	
class THttpClient extends TTransport {

    private var requestBuffer_  : BytesOutput = new BytesOutput();
    private var responseBuffer_ : BytesInput = null;

	#if (flash || openfl)
	private var request_        : URLRequest = null;
    #else
	private var request_        : Http = null;
    #end

    
	#if (flash || openfl)

	public function new( request : URLRequest) : Void {
		request.contentType = "application/x-thrift";
		request_ = request;
    }
	
	#else

	public function new( requestUrl : String) : Void {
	  	request_ = new Http(requestUrl);
		request_.addHeader( "contentType", "application/x-thrift");
    }
    
	#end
    
    public override function open() : Void {
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
		
		#if flash
        try {
			var data = Bytes.alloc(len);
            responseBuffer_.readBytes(data, off, len);
            buf.addBytes(data,0,len);
			return len;
        } catch (e : EOFError) {
            throw new TTransportException(TTransportException.UNKNOWN, "No more data available.");
        }
			
		#else
			
        var data =Bytes.alloc(len);
		len = responseBuffer_.readBytes(data, off, len);
		buf.addBytes(data,0,len);
		return len;
		
		#end
    }

    public override function write(buf:Bytes, off : Int, len : Int) : Void {
      requestBuffer_.writeBytes(buf, off, len);
    }

	
	#if (flash || openfl)
		
    public override function flush(callback:Error->Void = null) : Void {
		var loader : URLLoader = new URLLoader();
		
		if (callback != null) {
			loader.addEventListener(Event.COMPLETE, function(event:Event) : Void {
				responseBuffer_ = new URLLoader(event.target).data;
				callback(null);
			});
			loader.addEventListener(IOErrorEvent.IO_ERROR, function(event:IOErrorEvent) : Void {
				callback(new TTransportException(TTransportException.UNKNOWN, "IOError: " + event.text));
				responseBuffer_ = null;
			});
			loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, function(event:SecurityErrorEvent) : Void {
				callback(new TTransportException(TTransportException.UNKNOWN, "SecurityError: " + event.text));
				responseBuffer_ = null;
			});
		}
			
		request_.method = URLRequestMethod.POST;
		loader.dataFormat = URLLoaderDataFormat.BINARY;
		//requestBuffer_.position = 0;
		request_.data = requestBuffer_;
		loader.load(request_);
    }

	#else 
		
    public override function flush(callback:Dynamic->Void = null) : Void {
		
		var buffer = requestBuffer_;
		requestBuffer_ = new BytesOutput();
		responseBuffer_ = null;
			
		request_.onData = function(data : String) { 
			responseBuffer_ = new BytesInput(buffer.getBytes());
			callback(null);
		};
		request_.onError = function(msg : String) {
			callback(new TTransportException(TTransportException.UNKNOWN, "IOError: " + msg));
		};
		
		#if js
		request_.setPostData(buffer.getBytes().toString());
		request_.request(true/*POST*/);
		#else
		request_.customRequest( true/*POST*/, buffer);
		#end
    }
		
	#end

}

	