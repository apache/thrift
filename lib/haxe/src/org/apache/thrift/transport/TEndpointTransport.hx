// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.
package org.apache.thrift.transport;

import haxe.Int64;
import org.apache.thrift.TConfiguration;

class TEndpointTransport extends TTransport
{
	private var MaxMessageSize(get, never) : Int64;
	private var KnownMessageSize(default, null) : Int64 ;
	private var RemainingMessageSize(default, null) : Int64 ;

	private var _configuration(default,null) : TConfiguration;
	
	public override function get_Configuration() : TConfiguration {
		return _configuration;
	}
	
	private function get_MaxMessageSize() : Int64 {
		return Configuration.MaxMessageSize; 
	}
	
	
	// private CTOR to prevent direct instantiation
	// in other words, this class MUST be extended
	private function new( config : TConfiguration)
	{
		_configuration = (config != null) ? config : new TConfiguration();
		ResetConsumedMessageSize();
	}

	// Resets RemainingMessageSize to the configured maximum 
	private function ResetConsumedMessageSize(?newSize : Int64) : Void
	{
		// full reset 
		if (newSize == null)
		{
			KnownMessageSize = MaxMessageSize;
			RemainingMessageSize = MaxMessageSize;
			return;
		}

		// update only: message size can shrink, but not grow
		if (newSize > KnownMessageSize)
			throw new TTransportException(TTransportException.END_OF_FILE, "ResetConsumedMessageSize: MaxMessageSize reached");

		KnownMessageSize = newSize;
		RemainingMessageSize = newSize;
	}

	// Updates RemainingMessageSize to reflect then known real message size (e.g. framed transport).
	// Will throw if we already consumed too many bytes or if the new size is larger than allowed.
	public override function UpdateKnownMessageSize(size : Int64) : Void
	{
		var consumed = KnownMessageSize - RemainingMessageSize;
		ResetConsumedMessageSize(size);
		CountConsumedMessageBytes(consumed);
	}

	// Throws if there are not enough bytes in the input stream to satisfy a read of numBytes bytes of data
	public override function CheckReadBytesAvailable(numBytes : Int64) : Void
	{
		if (RemainingMessageSize < numBytes)
			throw new TTransportException(TTransportException.END_OF_FILE, 'CheckReadBytesAvailable(${numBytes}): MaxMessageSize reached, only ${RemainingMessageSize} bytes available');
	}

	// Consumes numBytes from the RemainingMessageSize.
	private function CountConsumedMessageBytes(numBytes : Int64) : Void
	{
		if (RemainingMessageSize >= numBytes)
		{
			RemainingMessageSize -= numBytes;
		}
		else
		{
			RemainingMessageSize = 0;
			throw new TTransportException(TTransportException.END_OF_FILE, 'CountConsumedMessageBytes(${numBytes}): MaxMessageSize reached');
		}
	}
}
