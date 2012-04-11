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

package org.apache.thrift.transport {
	import flash.utils.ByteArray;
	
	public class TMemoryBuffer extends TTransport {
		private var _barray:ByteArray;
		
		public function TMemoryBuffer() {
			super();
			_barray = new ByteArray();
		}
	
		/**
		 * @param buf the ByteArray to read into
		 * @param buf the off to start writing
		 * @param len the number of bytes to write into buf
		 */
	    override public function read(buf:ByteArray, off:int, len:int):int {
		    buf.position = 0;
		    
		    var amtToRead:int = Math.min(len, _barray.bytesAvailable);
		    
		    if (amtToRead > 0) {
		    	// offset, in writeBytes refers to the position of the array to read from
		    	buf.writeBytes( _barray, _barray.position, amtToRead );
		    	_barray.position += amtToRead; //we need to remember where we are in order to read the next thing
		    }
		    return amtToRead;
		}
		
		/**
		 * @param buf the ByteArray to read from
		 * @param off the position to start reading from
		 * @param len the number of bytes to read
		 */
	    override public function write(buf:ByteArray, off:int, len:int):void {
	    	buf.position = off;
	    	// offset in readBytes() refers to where to start _writing_ in the ByteArray you pass in.
	    	buf.readBytes(_barray, _barray.length, len);
	    	_barray.position = _barray.length; //put the position at the end of the used bytes in order to allow appending
	    }
	    
	    override public function flush(callback:Function=null):void {
	    	if ( callback != null ){
	    		callback( null );
	    	}
	    	reset();
	    }
	    
	    public function getArray():ByteArray {
	    	var ret:ByteArray = new ByteArray();
	    	ret.writeBytes(_barray); //copy _barray
	    	ret.position = 0;
	    	return ret;
	    }
	    
	    public function reset():void {
	    	_barray.position = 0;
	    }
	}
}
