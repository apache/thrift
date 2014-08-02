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

import haxe.remoting.SocketProtocol;	
import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import haxe.io.BytesInput;
import haxe.io.BytesOutput;
import haxe.io.Input;
import haxe.io.Output;
import haxe.io.Eof;
import sys.net.Host;


  /**
   * Socket implementation of the TTransport interface. Used for working with a
   * Thrift Socket Server based implementations.
   */

class TSocket extends TTransport  {
	
    private var host  :  Host;
    private var port  :  Int;
    private var socket : Socket = null;
	
    private var input : Input = null;
    private var output : Output = null;

	private var obuffer : BytesOutput = new BytesOutput();
    private var ioCallback : TTransportError->Void = null;
	private var readCount : Int = 0;
	
    public function new(host : String, port  :  Int)  :  Void  {
		this.host = new Host(host);
		this.port = port;
    }

	// used by TSocketServer
    public static function fromSocket( socket : Socket) : TSocket  {
		var result = new TSocket("",0);
		result.assignSocket(socket);
		return result;
    }

    public override function close()  :  Void  {
		input = null;
		output = null;
		socket.close();
    }

    public override function peek()  :  Bool  {
		if( (input == null) || (socket == null)) {
			return false;
		} else {
			var ready = Socket.select( [socket], null, null, 0);
			return (ready.read.length > 0);
		}
    }

	
	public override function read( buf : BytesBuffer, off : Int, len : Int) : Int   {
		try
		{
			socket.waitForRead();
			if(readCount < off) {
				input.read(off-readCount);	
				readCount = off;
			}
			var data = input.read(len);
			readCount += data.length;
			buf.add(data);
			return data.length;
		}
		catch (e : Eof)
		{
			trace('Eof $e');
			throw new TTransportError(TTransportError.END_OF_FILE, "No more data available.");
		}
		catch (e : Error)
		{
			trace('Error $e');
			throw new TTransportError(TTransportError.UNKNOWN, "Bad IO error :  " + e);
		}
    }
		
	
    public override function write(buf : Bytes, off  :  Int, len  :  Int)  :  Void
    {
		obuffer.writeBytes(buf, off, len);
    }


		
    public override function flush(callback : Error->Void = null)  :  Void
    {
		if( ! isOpen())
		{
			throw new TTransportError(TTransportError.NOT_OPEN, "Transport not open");
		}

		var bytes = obuffer.getBytes();
		obuffer = new BytesOutput();

		var len = bytes.length;
		
		ioCallback = callback;
		try {
			readCount = 0;
			output.writeBytes( bytes, 0, bytes.length);
			if(ioCallback != null) {
				ioCallback(null);  // success call 
			}
		}
		catch (e : Error) {
			trace(e);
			if(ioCallback != null) {
				ioCallback(new TTransportError(TTransportError.UNKNOWN, "Bad IO error :  " + e));
			}
		}
    }

    public override function isOpen()  :  Bool
    {
		return (socket != null);
    }

    public override function open()  :  Void
    {
		socket = new Socket();
		socket.setBlocking(true);
		socket.connect(host, port);
		socket.setFastSend(true);
		
      	output = socket.output;
      	input = socket.input;
    }

    private function assignSocket( socket : Socket)  :  Void
    {
		this.socket = socket;
		
      	output = socket.output;
      	input = socket.input;
    }

}
