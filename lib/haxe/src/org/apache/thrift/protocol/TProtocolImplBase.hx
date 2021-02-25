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

package org.apache.thrift.protocol;

import org.apache.thrift.*;
import org.apache.thrift.transport.TTransport;


class TProtocolImplBase {

	private var Configuration : TConfiguration;
    public var Transport(default,null) : TTransport;
	
	public function new( transport : TTransport)
	{
		Transport = transport;
		Configuration = (transport.Configuration != null) ? transport.Configuration : new TConfiguration();
	}

	
    public function getTransport() : TTransport {
		return Transport;
    }

	
    // limit and actual value
    public var recursionLimit(get,never) : Int;
    private var recursionDepth : Int = 0;

    public function get_recursionLimit() : Int
	{
		return Configuration.RecursionLimit;
	}
	
	
    public function IncrementRecursionDepth() : Void
    {
        if (recursionDepth < recursionLimit)
            ++recursionDepth;
        else
            throw new TProtocolException(TProtocolException.DEPTH_LIMIT, "Depth limit exceeded");
    }

    public function DecrementRecursionDepth() : Void
    {
        --recursionDepth;
    }


	private function CheckReadBytesAvailableSet(set : TSet) : Void
	{
		Transport.CheckReadBytesAvailable(set.size * GetMinSerializedSize(set.elemType));
	}

	private function CheckReadBytesAvailableList(list : TList) : Void
	{
		Transport.CheckReadBytesAvailable(list.size * GetMinSerializedSize(list.elemType));
	}

	private function CheckReadBytesAvailableMap (map : TMap) : Void
	{
		var elmSize = GetMinSerializedSize(map.keyType) + GetMinSerializedSize(map.valueType);
		Transport.CheckReadBytesAvailable(map.size * elmSize);
	}

	// Returns the minimum amount of bytes needed to store the smallest possible instance of TType.
	public function GetMinSerializedSize(type : TType) : Int throw "abstract method called";

}
