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

using System;

#pragma warning disable IDE0079 // net20 - unneeded suppression
#pragma warning disable IDE0290 // net8 - primary CTOR

namespace Thrift.Transport
{
    public abstract class TLayeredTransport : TTransport
    {
        public readonly TTransport InnerTransport;

        public override TConfiguration Configuration { get => InnerTransport.Configuration; }

        public TLayeredTransport(TTransport transport)
        {
            InnerTransport = transport ?? throw new ArgumentNullException(nameof(transport));
        }

        public override void UpdateKnownMessageSize(long size)
        {
            InnerTransport.UpdateKnownMessageSize(size);
        }

        public override void CheckReadBytesAvailable(long numBytes)
        {
            InnerTransport.CheckReadBytesAvailable(numBytes);
        }

        public override void ResetConsumedMessageSize(long newSize = -1)
        {
            InnerTransport.ResetConsumedMessageSize(newSize);
        }
    }
}
