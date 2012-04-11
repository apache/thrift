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

package org.apache.thrift {
	import flash.utils.ByteArray;
	
	import org.apache.thrift.protocol.TBinaryProtocol;
	import org.apache.thrift.protocol.TProtocol;
	import org.apache.thrift.transport.TMemoryBuffer;
	
	public class TDeserializer {
		private var _protocol:TProtocol;
		private var _transport:TMemoryBuffer;
		
		public function TDeserializer() {
			_transport = new TMemoryBuffer();
			_protocol = new TBinaryProtocol(_transport);
		}
		
		/**
		 * Take the bytes from the ByteArray <code>bytes</code> and populate <code>base</code> with it.
		 */
		public function deserialize(base:TBase, bytes:ByteArray):void {
			bytes.position = 0; //make sure we're at the beginning. It's a very good place to start
			_transport.reset();
			_transport.write(bytes, 0, bytes.bytesAvailable); //write the ByteArray into the transport object
			_transport.reset();
			base.read(_protocol); // Read the data from the transport into the TBase object via the protocol
		}

	}
}
