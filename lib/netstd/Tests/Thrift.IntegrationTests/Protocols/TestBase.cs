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

using KellermanSoftware.CompareNetObjects;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

namespace Thrift.IntegrationTests.Protocols
{
    public class TestBase
    {
        protected readonly CompareLogic _compareLogic = new();
        protected static readonly TConfiguration Configuration = new();


        protected record struct ProtocolTransportStack(Stream Stream, TTransport Transport, TProtocol Protocol);

        protected static ProtocolTransportStack GetProtocolInstance(Type protocolType)
        {
            var memoryStream = new MemoryStream();
            var streamClientTransport = new TStreamTransport(memoryStream, memoryStream, Configuration);
            if (Activator.CreateInstance(protocolType, streamClientTransport) is TProtocol protocol)
                return new ProtocolTransportStack(memoryStream, streamClientTransport, protocol);
            throw new Exception("Unexpected");
        }
    }
}
