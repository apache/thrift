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
using System.Reflection.Emit;
using System.Xml.Linq;
using Thrift.Protocol;
using Thrift.Transport;
using Thrift.Transport.Client;

#pragma warning disable IDE0079  // unneeded suppression -> all except net8
#pragma warning disable IDE0028  // simplify collection init -> net8 only

namespace Thrift
{
    public static class TFactory
    {
        private class LayerFactory
        {
            // its either one or the other, can't be both
            public readonly TProtocolDecoratorFactory ProtocolDecorator;
            public readonly TTransportFactory LayeredTransport;

            public LayerFactory(TProtocolDecoratorFactory factory)
            {
                ProtocolDecorator = factory;
            }

            public LayerFactory(TTransportFactory factory)
            {
                LayeredTransport = factory;
            }
        }


        private static readonly Dictionary<string, TProtocolFactory> RegisteredProtocols = new Dictionary<string, TProtocolFactory>();
        private static readonly Dictionary<string, TEndpointTransportFactory> RegisteredEndpointTransports = new Dictionary<string, TEndpointTransportFactory>();
        private static readonly Dictionary<string, LayerFactory> RegisteredLayers = new Dictionary<string, LayerFactory>();


        static TFactory()
        {
            // protocol
            Register("binary", new TBinaryProtocol.Factory());
            Register("compact", new TCompactProtocol.Factory());
            Register("json", new TJsonProtocol.Factory());

            // protocol decorators
            Register("mplex", new TMultiplexedProtocol.Factory());

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
            // throws intentionally if name is already used
            lock (RegisteredProtocols)
                RegisteredProtocols.Add(name, factory);
        }

        public static void Register(string name, TEndpointTransportFactory factory)
        {
            // throws intentionally if name is already used
            lock (RegisteredEndpointTransports)
                RegisteredEndpointTransports.Add(name, factory);
        }

        public static void Register(string name, TTransportFactory factory)
        {
            // throws intentionally if name is already used
            lock (RegisteredLayers)
                RegisteredLayers.Add(name, new LayerFactory(factory));  
        }

        public static void Register(string name, TProtocolDecoratorFactory factory)
        {
            // throws intentionally if name is already used
            lock (RegisteredLayers)
                RegisteredLayers.Add(name, new LayerFactory(factory));
        }


        public static TProtocol ConstructClientProtocolTransportStack(string sThriftUri, TConfiguration config, out TTransport transport)
        {
            var uri = new TThriftUri(sThriftUri);
            return ConstructClientProtocolTransportStack(uri, config, out transport);
        }


        public static TProtocol ConstructClientProtocolTransportStack(TThriftUri uri, TConfiguration config, out TTransport transport)
        {
            transport = CreateEndpointTransport(uri.Transport, config, uri.QueryData);
            foreach (var layer in uri.Layers)
                transport = AddLayeredTransport(transport, layer.Key);

            var protocol = CreateProtocol(uri.Protocol, transport);
            foreach (var layer in uri.Layers)
                protocol = AddProtocolDecorator(protocol, layer.Key, layer.Value);

            return protocol;
        }

        private static TEndpointTransport CreateEndpointTransport(string name, TConfiguration config, Dictionary<string, string> args)
        {
            if (RegisteredEndpointTransports.TryGetValue(name, out var factory))
                return factory.GetTransport(config, args);

            throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "Endpoint transport '" + name + "' not registered");
        }

        private static TProtocol CreateProtocol(string name, TTransport transport)
        {
            if (RegisteredProtocols.TryGetValue(name, out var factory))
                return factory.GetProtocol(transport);

            throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "Protocol '" + name + "' not registered");
        }

        private static TProtocol AddProtocolDecorator(TProtocol protocol, string name, Dictionary<string, string> args)
        {
            if (!RegisteredLayers.TryGetValue(name, out var factory))
                throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "Thrift protocol/transport layer '" + name + "' not registered");
            if (factory.ProtocolDecorator != null)
                return factory.ProtocolDecorator.GetProtocol(protocol, args);
            return protocol;
        }

        private static TTransport AddLayeredTransport(TTransport transport, string name)
        {
            if (!RegisteredLayers.TryGetValue(name, out var factory))
                throw new TApplicationException(TApplicationException.ExceptionType.Unknown, "Thrift protocol/transport layer '" + name + "' not registered");
            if (factory.LayeredTransport != null)
                return factory.LayeredTransport.GetTransport(transport);
            return transport;
        }
    }
}
