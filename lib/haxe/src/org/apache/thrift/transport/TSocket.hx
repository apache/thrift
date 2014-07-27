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

#if (flash || openfl)
import flash.events.EventDispatcher;
import flash.events.Event;
import flash.events.IOErrorEvent;
import flash.events.ProgressEvent;
import flash.events.SecurityErrorEvent;
import flash.errors.EOFError;
import flash.errors.IOError;
import flash.net.URLLoader;
import flash.net.URLLoaderDataFormat;
import flash.net.URLRequest;
import flash.net.URLRequestMethod;
import flash.utils.IDataInput;
import flash.utils.IDataOutput;
import flash.net.Socket;
#else
import haxe.remoting.SocketProtocol;	
#end
	
import haxe.io.Bytes;
import haxe.io.BytesInput;
import haxe.io.BytesOutput;
import sys.net.Host;


  /**
   * Socket implementation of the TTransport interface. Used for working with a
   * Thrift Socket Server based implementations.
   */

class TSocket extends TTransport  {
	
    private var socket : Socket = null;
    private var host  :  Host;
    private var port  :  Int;
    private var obuffer : BytesOutput = new BytesOutput();
    private var input : BytesInput;
    private var output : BytesOutput;
    private var ioCallback : TTransportError->Void = null;
	
	#if flash
    private var eventDispatcher : EventDispatcher = new EventDispatcher();
	#end

    public function new(host : String, port  :  Int)  :  Void  {
		this.host = new Host(host);
		this.port = port;
    }

    public override function close()  :  Void  {
		this.input = null;
		this.output = null;
		socket.close();
    }

    public override function peek()  :  Bool  {
		#if (flash || openfl)
		return socket.connected && (socket.bytesAvailable > 0);
		#else
		return true;  // method missing
		#end		
    }

    public override function read(buf : Bytes, off  :  Int, len  :  Int)  :  Int   {
		try
		{
			input.readBytes(buf, off, len);
			return len;
		}
		#if (flash || openfl)
		catch (e : EOFError)
		{
			trace(e);
			throw new TTransportError(TTransportError.END_OF_FILE, "No more data available.");
		}
		catch (e : IOError)
		{
			trace(e);
			if(isOpen())
			{
			  throw new TTransportError(TTransportError.UNKNOWN, "IO error while reading :  " + e);
			}
			else
			{
			  throw new TTransportError(TTransportError.NOT_OPEN, "Socket seem not to be opened :  " + e);
			}
		}
		#end
		catch (e : Error)
		{
			trace(e);
			throw new TTransportError(TTransportError.UNKNOWN, "Bad IO error :  " + e);
		}
			
		return 0;
    }

    public override function write(buf : Bytes, off  :  Int, len  :  Int)  :  Void
    {
		obuffer.writeBytes(buf, off, len);
    }

    public override function open()  :  Void
    {
		socket = new Socket();
		#if (flash || openfl)
		socket.addEventListener(Event.CONNECT, socketConnected);
		socket.addEventListener(IOErrorEvent.IO_ERROR, socketError);
		socket.addEventListener(SecurityErrorEvent.SECURITY_ERROR, socketSecurityError);
		socket.addEventListener(ProgressEvent.SOCKET_DATA, socketDataHandler);
		#end
		socket.connect(host, port);
		socket.setFastSend(true);
    }

	/* 
	  
    public function addEventListener( type : String, listener : Dynamic ->Void, ?useCapture : Bool, 
                                      ?priority : Int, ?useWeakReference : Bool):Void
    {
      this.eventDispatcher.addEventListener(type, listener, useCapture, priority, useWeakReference);
    }

    public function socketConnected(event : Event)  :  Void
    {
      this.output = this.socket;
      this.input = this.socket;
      this.eventDispatcher.dispatchEvent(event);
    }

    public function socketError(event : IOErrorEvent)  :  Void
    {
      trace("Error Connecting:" + event);
      this.close();
      if (ioCallback == null)
      {
        return;
      }
      ioCallback(new TTransportError(TTransportError.UNKNOWN, "IOError :  " + event.text));
      this.eventDispatcher.dispatchEvent(event);
    }

    public function socketSecurityError(event : SecurityErrorEvent)  :  Void
    {
      trace("Security Error Connecting:" + event);
      this.close();
      this.eventDispatcher.dispatchEvent(event);
    }

    public function socketDataHandler(event : ProgressEvent)  :  Void
    {
      if (ioCallback != null)
      {
        ioCallback(null);
      }
      this.eventDispatcher.dispatchEvent(event);
    }

	*/

    public override function flush(callback : Error->Void = null)  :  Void
    {
		var bytes = obuffer.getBytes();
		obuffer = new BytesOutput();

		ioCallback = callback;
		output.writeBytes( bytes, 0, bytes.length);
    }

    public override function isOpen()  :  Bool
    {
		#if (flash || openfl)
		return (socket != null) &&  socket.connected;
		#else
		return (socket != null);
		#end
    }

}
