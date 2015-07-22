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
using Thrift.Server;
using Thrift.Protocol;

namespace Thrift.Test
{
    class Program
    {
        private static TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();

        static void Main(string[] args)
        {
            try
            {
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i] == "--binary")
                    {
                        protocolFactory = new TBinaryProtocol.Factory();
                    }
                    if (args[i] == "--json")
                    {
                        protocolFactory = new TJSONProtocol.Factory();
                    }
                    if (args[i] == "--compact")
                    {
                        protocolFactory = new TCompactProtocol.Factory();
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.StackTrace);
            }
            try
            {
                TestHandler handler = new TestHandler();
                ThriftTest.Processor processor = new ThriftTest.Processor(handler);

                TStreamTransport transport = new TStreamTransport(Console.OpenStandardInput(), Console.OpenStandardOutput());

                TStreamServer server = new TStreamServer(processor, transport, new TTransportFactory(), protocolFactory);
                server.Serve();
            }
            catch (Exception x)
            {
                Console.WriteLine(x.StackTrace);
            }   
        }
    }
}
