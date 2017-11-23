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

import haxe.Http;

#if nodejs
import js.node.Fs;
import js.Node;
import js.node.ChildProcess;
import js.Node.process;
#end

using StringTools;

/**
* HTTP implementation of the TTransport interface. Used for working with a
* Thrift web services implementation.
*/

class THttpClient extends TTransport {

    private var requestBuffer_  : BytesOutput = new BytesOutput();
    private var responseBuffer_ : BytesInput = null;

    #if !js
    private var request_        : Http = null;
    #else
    private var request_        : JsHttp = null;
    #end


    public function new( requestUrl : String) : Void {
        #if !js
        request_ = new Http(requestUrl);
        #else
        request_ = new JsHttp(requestUrl);
        request_.async = false;
        #end
        request_.addHeader("Content-Type", "application/x-thrift");
        request_.addHeader("Accept", "application/x-thrift");
        #if !(js && !nodejs)
        request_.addHeader("User-Agent", "Haxe/THttpClient");
        #end
    }


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

        var data =Bytes.alloc(len);
        len = responseBuffer_.readBytes(data, off, len);
        buf.addBytes(data,0,len);
        return len;
    }

    public override function write(buf:Bytes, off : Int, len : Int) : Void {
      requestBuffer_.writeBytes(buf, off, len);
    }


    public override function flush(callback:Dynamic->Void = null) : Void {
        var buffer = requestBuffer_;
        requestBuffer_ = new BytesOutput();
        responseBuffer_ = null;

        request_.onData = function(data : String) {
            var tmp = new BytesBuffer();
            tmp.addString(data);
            responseBuffer_ = new BytesInput(tmp.getBytes());
            if( callback != null) {
                callback(null);
            }
        };

        request_.onError = function(msg : String) {
            if( callback != null) {
                callback(new TTransportException(TTransportException.UNKNOWN, "IOError: " + msg));
            }
        };

        #if js
        request_.onBinaryData = function(data : Bytes) {
            responseBuffer_ = new BytesInput(data);
            if( callback != null) {
                callback(null);
            }
        };

        request_.setBinaryPostData(buffer.getBytes());
        #else
        request_.setPostData(buffer.getBytes().toString());
        #end
        request_.request(true/*POST*/);
    }
}

#if js
/*supports sending/receiving binary/json data (browser, nodejs) 
  implemented atop https://github.com/HaxeFoundation/haxe/blob/development/std/haxe/Http.hx
  */
class JsHttp extends Http {
    var binaryPostData : Bytes;

    public function setBinaryPostData( data : Bytes ):Http {
        binaryPostData = data;
        return this;
    }

    public dynamic function onBinaryData( data : Bytes ) {
    }

    #if !nodejs
    public override function request( ?post : Bool ) : Void {
        var me = this;
        me.responseData = null;
        var r = req = js.Browser.createXMLHttpRequest();
        var onreadystatechange = function(_) {
            if( r.readyState != 4 )
                return;
            var s = try r.status catch( e : Dynamic ) null;
            if ( s != null && untyped __js__('"undefined" !== typeof window') ) {
                // If the request is local and we have data: assume a success (jQuery approach):
                var protocol = js.Browser.location.protocol.toLowerCase();
                var rlocalProtocol = ~/^(?:about|app|app-storage|.+-extension|file|res|widget):$/;
                var isLocal = rlocalProtocol.match( protocol );
                if ( isLocal ) {
                    s = r.responseText != null ? 200 : 404;
                }
            }
            if( s == untyped __js__("undefined") )
                s = null;
            if( s != null )
                me.onStatus(s);
            if( s != null && s >= 200 && s < 400 ) {
                me.req = null;
                var len = r.responseText.length;
                var bytes = new BytesOutput();
                bytes.prepare(len);
                for(i in 0 ... len) {
                    var byte = (r.responseText.charCodeAt(i) & 255);
                    if(byte >= 128) {
                        byte -= 256;
                    }
                    bytes.writeInt8(byte);
                }
                var resBytes = bytes.getBytes();
                me.onBinaryData(resBytes);
            }
            else if ( s == null ) {
                me.req = null;
                me.onError("Failed to connect or resolve host");
            }
            else switch( s ) {
            case 12029:
                me.req = null;
                me.onError("Failed to connect to host");
            case 12007:
                me.req = null;
                me.onError("Unknown host");
            default:
                me.req = null;
                me.responseData = r.responseText;
                me.onError("Http Error #"+r.status);
            }
        };
        if( async )
            r.onreadystatechange = onreadystatechange;
        var uri = postData;
        var jsData = binaryPostData;
        if( jsData != null )
            post = true;
        else for( p in params ) {
            if( uri == null )
                uri = "";
            else
                uri += "&";
            uri += StringTools.urlEncode(p.param)+"="+StringTools.urlEncode(p.value);
        }
        try {
            if( post )
                r.open("POST",url,async);
            else if( uri != null ) {
                var question = url.split("?").length <= 1;
                r.open("GET",url+(if( question ) "?" else "&")+uri,async);
                uri = null;
            } else
                r.open("GET",url,async);
        } catch( e : Dynamic ) {
            me.req = null;
            onError(e.toString());
            return;
        }

        //XHR binary charset opt by Marcus Granado 2006 [http://mgran.blogspot.com]
        req.overrideMimeType("text\\/plain; charset=x-user-defined");

        #if (haxe_ver >= 3.3)
        r.withCredentials = me.withCredentials;
        #end
        if( !Lambda.exists(headers, function(h) return h.header == "Content-Type") && post && postData == null )
            r.setRequestHeader("Content-Type","application/x-www-form-urlencoded");

        for( h in headers )
            r.setRequestHeader(h.header,h.value);

        if( jsData != null ) {
            r.send(new js.html.Uint8Array(jsData.getData()));
        } else {
            r.send(uri);
        }

        if( !async )
            onreadystatechange(null);
    }
    #elseif nodejs
    public override function request( ?post : Bool ) : Void {

        var url_regexp = ~/^(https?:\/\/)?([a-zA-Z\.0-9_-]+)(:[0-9]+)?(.*)$/;
        if( !url_regexp.match(url) ) {
            onError("Invalid URL");
            return;
        }
        var ssl = (url_regexp.matched(1) == "https://");
        var host = url_regexp.matched(2);
        var portString = url_regexp.matched(3);
        var request = url_regexp.matched(4);
        if( request == "" )
            request = "/";
        var port = if ( portString == null || portString == "" ) ssl ? 443 : 80 else Std.parseInt(portString.substr(1, portString.length - 1));

        var options = {
            "hostname":host,
            "port":port,
            "path":request,
            "method":post ? "POST" : "GET",
            "agent":false,
            "withCredentials":#if (haxe_ver >= 3.3) me.withCredentials #else false #end
        };

        var headersArray = [];
        for( h in headers ) {
            headersArray.push({header : h.header, value : h.value});
        }

        // stringify data
        var sendCode = "";
        var data = binaryPostData;

        if(data != null) {
            var bufStr = haxe.Json.stringify(js.node.buffer.Buffer.hxFromBytes(data)).replace("'", "\\'");
            sendCode = "var dataObj = JSON.parse('" + bufStr + "', "
            + " function(key, value) {"
                + "return value && value.type === 'Buffer'"
                + "? new Buffer(value.data)"
                + ": value;"
            + "});";
            sendCode += "req.write(dataObj);";
            headersArray.push({header : "Content-Length", value : Std.string(data.length)});
        } else {
            headersArray.push({header : "Content-Length", value : '0'});
        }

        var responseEncoding = 'binary';
 
        // The async request the other Node process executes
        var execString = "var http = require('http'), https = require('https'), fs = require('fs');"
          + "var doRequest = http" + (ssl ? "s" : "") + ".request;"
          + "var options = " + haxe.Json.stringify(options) + ";"
          + "var responseText = '';"
          + "var req = doRequest(options, function(response) {"
          + "response.setEncoding(" + (responseEncoding != null ? "'" + responseEncoding + "'" : responseEncoding) + ");"
          + "response.on('data', function(chunk) {"
          + "  responseText += chunk;"
          + "});"
          + "response.on('end', function() {"
          + "if(responseText.length) {"
          + "  responseText = JSON.stringify(new Buffer(responseText, " + (responseEncoding != null ? "'" + responseEncoding + "'" : responseEncoding) + "));"
          + "}"
          + "process.stdout.write(JSON.stringify({err: null, data: {statusCode: response.statusCode, headers: response.headers, text: responseText}}));"
          + "});"
          + "response.on('error', function(error) {"
          + "process.stdout.write(JSON.stringify({err: error, errorMessage : error.message}));"
          + "});"
          + "}).on('error', function(error) {"
          + "process.stdout.write(JSON.stringify({err: error, errorMessage : error.message}));"
          + "});"
          + "var headersArr = JSON.parse('" + haxe.Json.stringify(headersArray) + "');"
          + "for(i = 0; i > headersArr.length; i++) {"
          + "req.setHeader(headersArr[i].header, headersArr[i].value);"
          + "}"
          + sendCode 
          + "req.end();";

        //Fs.writeFileSync("codetorun.js", execString, "binary");

        // Start the other Node Process, executing this string
        var subProcess = ChildProcess.spawnSync(Node.process.argv[0], ["-e", execString], {stdio: "pipe"});

        if(subProcess.status != 0 || subProcess.stderr != '') {
            this.onError('Execution of js is failed : ${execString}\n error : ${subProcess.stderr}');
        }

        var responseBody = subProcess.stdout; 
        var resp = haxe.Json.parse(responseBody);
  
        if (resp.err != null) {
            this.onError(resp.errorMessage);
        } else {
            if(resp.data.text != '') {
                var responseBuffer = haxe.Json.parse(resp.data.text);
                var buffer = new js.node.buffer.Buffer(responseBuffer.data);
                this.onBinaryData(buffer.hxToBytes());
            } else {
                this.onBinaryData(Bytes.alloc(0));
            }
        }
    }
    #end

}
#end
    
