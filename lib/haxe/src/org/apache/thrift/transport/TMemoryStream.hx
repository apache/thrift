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
import haxe.io.Output;

class TMemoryStream implements TStream {

	private var Data : Bytes;
	public var Position(default,default) : Int;

	public function new( data : Bytes = null) {
		var target = new BytesBuffer();
		if ( data != null) {
			for ( i in 0...data.length) {
				target.addByte( data.get(i));
				++Position;
			}
		}
		Data = target.getBytes();
	}

	private function IsEof() : Bool {
		return (0 > Position) || (Position >= Data.length);
	}

	public function Close() : Void {
		var target = new BytesBuffer();
		Data = target.getBytes();
		Position = 0;
	}

	public function Peek() : Bool {
		return (! IsEof());
	}

	// read count bytes into buf starting at offset
	public function Read( buf : Bytes, offset : Int, count : Int) : Int {
		var numRead = 0;

		for ( i in 0...count) {
			if ( IsEof())
				break;

			buf.set( offset + i, Data.get( Position++));
			++numRead;
		}

		return numRead;
	}

	// write count bytes from buf starting at offset
	public function Write( buf : Bytes, offset : Int, count : Int) : Void {
		var numBytes = buf.length - offset;
		if ( numBytes > count) {
			numBytes = count;
		}

		for ( i in 0...numBytes) {
			Data.set( Position + i, buf.get( offset + i));
		}
	}

	public function Flush() : Void {
		// nothing to do
	}

}

