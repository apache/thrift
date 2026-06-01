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

// An in-memory stream that grows on write. Writing appends at (and reads consume
// from) Position; a write past the current end enlarges the backing buffer, so a
// fresh stream can be written to and then read back -- set Position = 0 between
// the write and the read phase. The backing buffer may over-allocate (capacity
// doubling) for amortized O(1) appends; Length tracks the logical byte count.
class TMemoryStream implements TStream {

	private var Data : Bytes;   // backing store; Data.length is the capacity
	private var Length : Int;   // number of valid bytes (<= Data.length)
	public var Position(default,default) : Int;

	public function new( data : Bytes = null) {
		if ( data != null) {
			Data = data.sub( 0, data.length);  // defensive copy
			Length = data.length;
		} else {
			Data = Bytes.alloc( 0);
			Length = 0;
		}
		Position = 0;
	}

	private function IsEof() : Bool {
		return (0 > Position) || (Position >= Length);
	}

	public function Close() : Void {
		Data = Bytes.alloc( 0);
		Length = 0;
		Position = 0;
	}

	public function Peek() : Bool {
		return (! IsEof());
	}

	// read up to count bytes into buf at offset, advancing Position; returns the
	// number of bytes actually read (0 at end of stream)
	public function Read( buf : Bytes, offset : Int, count : Int) : Int {
		var numRead = Length - Position;
		if ( numRead > count) {
			numRead = count;
		}
		if ( numRead <= 0) {
			return 0;
		}

		buf.blit( offset, Data, Position, numRead);
		Position += numRead;
		return numRead;
	}

	// write count bytes from buf (starting at offset) at the current Position,
	// growing the backing buffer as needed and advancing Position
	public function Write( buf : Bytes, offset : Int, count : Int) : Void {
		var numBytes = buf.length - offset;
		if ( numBytes > count) {
			numBytes = count;
		}
		if ( numBytes <= 0) {
			return;
		}

		EnsureCapacity( Position + numBytes);
		Data.blit( Position, buf, offset, numBytes);
		Position += numBytes;
		if ( Position > Length) {
			Length = Position;
		}
	}

	// grow the backing buffer to hold at least 'needed' bytes, preserving content
	private function EnsureCapacity( needed : Int) : Void {
		if ( needed <= Data.length) {
			return;
		}

		var capacity = (Data.length > 0) ? Data.length : 16;
		while ( capacity < needed) {
			capacity *= 2;
		}

		var grown = Bytes.alloc( capacity);
		grown.blit( 0, Data, 0, Length);
		Data = grown;
	}

	public function Flush() : Void {
		// nothing to do
	}

}
