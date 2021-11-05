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

class TLayeredTransport extends TTransport
{
	private var InnerTransport : TTransport;

	public override function get_Configuration() : TConfiguration {
		return InnerTransport.Configuration;
	}

	// private CTOR to prevent direct instantiation
	// in other words, this class MUST be extended
	private function new(transport : TTransport)
	{
		if( transport != null)
			InnerTransport = transport;
		else
			throw new TTransportException( TTransportException.UNKNOWN, "Inner transport must not be null");
	}

	public override function UpdateKnownMessageSize(size : Int64) : Void
	{
		InnerTransport.UpdateKnownMessageSize(size);
	}

	public override function CheckReadBytesAvailable(numBytes : Int64) : Void
	{
		InnerTransport.CheckReadBytesAvailable(numBytes);
	}
}
