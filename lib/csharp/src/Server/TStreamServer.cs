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

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Thrift.Transport;
using Thrift.Protocol;

namespace Thrift.Server
{
    public class TStreamServer
    {
        protected TProcessor processor;
        private TStreamTransport transport;        
        protected TTransportFactory inputTransportFactory;
        protected TTransportFactory outputTransportFactory;
        protected TProtocolFactory inputProtocolFactory;
        protected TProtocolFactory outputProtocolFactory;

        public TStreamServer(TProcessor processor, TStreamTransport transport)
            : this(processor, transport, new TTransportFactory(), new TBinaryProtocol.Factory())
        {
            
        }

        public TStreamServer(TProcessor processor, TStreamTransport transport, TTransportFactory transportFactory, TProtocolFactory protocolFactory)
        {
            this.processor = processor;
            this.transport = transport;
            this.inputProtocolFactory = protocolFactory;
            this.outputProtocolFactory = protocolFactory;
            this.inputTransportFactory = transportFactory;
            this.outputTransportFactory = transportFactory;
        }

        public void Serve()
        {
            while (true)
            {
                TTransport client = null;
                TTransport inputTransport = null;
                TTransport outputTransport = null;
                TProtocol inputProtocol = null;
                TProtocol outputProtocol = null;
                client = transport;

                if (client != null)
                {
                    inputTransport = inputTransportFactory.GetTransport(client);

                    outputTransport = outputTransportFactory.GetTransport(client);

                    inputProtocol = inputProtocolFactory.GetProtocol(inputTransport);
                    outputProtocol = outputProtocolFactory.GetProtocol(outputTransport);

                    //Process client requests until client disconnects
                    while (true)
                    {
                        if (!inputTransport.Peek())
                            break;

                        //Process client request (blocks until transport is readable)
                        if (!processor.Process(inputProtocol, outputProtocol))
                            break;
                    }
                }
            }
        }
    }
}
