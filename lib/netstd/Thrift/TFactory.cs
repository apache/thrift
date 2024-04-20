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
using System.Collections.Generic;
using System.Net.Sockets;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;
using Thrift.Transport.Server;
using static System.Net.WebRequestMethods;

namespace Thrift
{
    public static class TFactory
    {
        private static readonly Dictionary<string, TProtocolFactory> RegisteredProtocols = new Dictionary<string, TProtocolFactory>();
        private static readonly Dictionary<string, TTransportFactory> RegisteredLayeredTransports = new Dictionary<string, TTransportFactory>();
        private static readonly Dictionary<string, TEndpointTransportFactory> RegisteredEndpointTransports = new Dictionary<string, TEndpointTransportFactory>();

        static TFactory()
        {
            // protocol
            Register("binary", new TBinaryProtocol.Factory());
            Register("compact", new TCompactProtocol.Factory());
            Register("json", new TJsonProtocol.Factory());

            // layered transports
            Register("framed", new TFramedTransport.Factory());
            Register("buffered", new TBufferedTransport.Factory());

            // common endpoint transports
            Register("file", new TStreamTransport.Factory());
            Register("memory", new TMemoryBufferTransport.Factory());

            // client endpoint transports
            Register("socket", new TSocketTransport.Factory());
            Register("tlssocket", new TTlsSocketTransport.Factory());
            Register("http", new THttpTransport.Factory());
            Register("namedpipes", new TNamedPipeTransport.Factory());
        }

        public static void Register(string name, TProtocolFactory factory)
        {
            lock (RegisteredProtocols)
                RegisteredProtocols.Add(name, factory);  // throws intentionally if name is already used
        }

        public static void Register(string name, TTransportFactory factory)
        {
            lock (RegisteredLayeredTransports)
                RegisteredLayeredTransports.Add(name, factory);  // throws intentionally if name is already used
        }

        public static void Register(string name, TEndpointTransportFactory factory)
        {
            lock (RegisteredEndpointTransports)
                RegisteredEndpointTransports.Add(name, factory);  // throws intentionally if name is already used
        }


        public static TProtocol ConstructClientProtocolTransportStack(string sThriftUri, TConfiguration config, out TTransport transport)
        {
            var uri = new TThriftUri(sThriftUri);
            return ConstructClientProtocolTransportStack(uri, config, out transport);
        }


        public static TProtocol ConstructClientProtocolTransportStack(TThriftUri uri, TConfiguration config, out TTransport transport)
        {
            transport = CreateEndpointTransport(uri.EndpointTransport, config, uri.QueryData);
            foreach (var layer in uri.LayeredTransports)
                transport = CreateLayeredTransport(layer, transport);
            return CreateProtocol(uri.Protocol, transport);
        }

        private static TEndpointTransport CreateEndpointTransport(string name, TConfiguration config, Dictionary<string, string> args)
        {
            if (RegisteredEndpointTransports.TryGetValue(name, out var factory))
                return factory.GetTransport(config, args);
            throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "Endpoint transport '" + name + "' not registered");
        }

        private static TTransport CreateLayeredTransport(string name, TTransport transport)
        {
            if (RegisteredLayeredTransports.TryGetValue(name, out var factory))
                return factory.GetTransport(transport);
            throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "layered transport '" + name + "' not registered");
        }

        private static TProtocol CreateProtocol(string name, TTransport transport)
        {
            if (RegisteredProtocols.TryGetValue(name, out var factory))
                return factory.GetProtocol(transport);
            throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "Protocol '" + name + "' not registered");
        }
    }
}
