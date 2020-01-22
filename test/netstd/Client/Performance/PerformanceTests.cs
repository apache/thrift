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
using System.Text;
using ThriftTest;
using Thrift.Collections;
using Thrift;
using Thrift.Protocol;
using System.Threading;
using Thrift.Transport.Client;
using System.Threading.Tasks;
using System.Diagnostics;
using Thrift.Transport;

namespace Client.Tests
{
    public class PerformanceTests
    {
        private CancellationTokenSource Cancel;
        private CrazyNesting Testdata;
        private TMemoryBufferTransport MemBuffer;
        private TTransport Transport;
        private LayeredChoice Layered;
        private readonly TConfiguration Configuration = new TConfiguration();

        internal static int Execute()
        {
            var instance = new PerformanceTests();
            instance.ProtocolPeformanceTestAsync().Wait();

            // debug only
            if (Debugger.IsAttached)
            {
                Console.Write("Hit ENTER ...");
                Console.ReadKey();
            }

            return 0;
        }

        public PerformanceTests()
        {
            Configuration.MaxFrameSize = Configuration.MaxMessageSize;  // default frame size is too small for this test
        }

        private async Task ProtocolPeformanceTestAsync()
        {
            Console.WriteLine("Setting up for ProtocolPeformanceTestAsync ...");
            Cancel = new CancellationTokenSource();
            Testdata = TestDataFactory.CreateCrazyNesting();

            foreach (var layered in Enum.GetValues(typeof(LayeredChoice)))
            {
                Layered = (LayeredChoice)layered;
                await RunTestAsync(async (bool b) => { return await GenericProtocolFactory<TBinaryProtocol>(b); });
                await RunTestAsync(async (bool b) => { return await GenericProtocolFactory<TCompactProtocol>(b); });
                await RunTestAsync(async (bool b) => { return await GenericProtocolFactory<TJsonProtocol>(b); });
            }
        }

        private Task<TProtocol> GenericProtocolFactory<T>(bool forWrite)
            where T : TProtocol
        {
            var oldTrans = Transport;
            try
            {
                // read happens after write here, so let's take over the written bytes
                if (forWrite)
                    MemBuffer = new TMemoryBufferTransport(Configuration);  
                else
                    MemBuffer = new TMemoryBufferTransport(MemBuffer.GetBuffer(), Configuration);

                //  layered transports anyone?
                switch (Layered)
                {
                    case LayeredChoice.None:
                        Transport = MemBuffer;
                        break;
                    case LayeredChoice.Framed:
                        Transport = new TFramedTransport(MemBuffer);
                        break;
                    case LayeredChoice.Buffered:
                        Transport = new TBufferedTransport(MemBuffer);
                        break;
                    default:
                        Debug.Assert(false);
                        break;
                }

                if (!Transport.IsOpen)
                    Transport.OpenAsync().Wait();

                var instance = (T)Activator.CreateInstance(typeof(T), Transport);
                return Task.FromResult<TProtocol>(instance);
            }
            finally
            {
                if (oldTrans is IDisposable)
                    (oldTrans as IDisposable).Dispose();
            }
        }

        private string GetProtocolTransportName(TProtocol proto)
        {
            var name = Transport.GetType().Name;
            if (name.Equals(MemBuffer.GetType().Name))
                name = string.Empty;
            else
                name = " + " + name;

            name = proto.GetType().Name + name;
            return name;
        }


        private async Task RunTestAsync(Func<bool, Task<TProtocol>> factory)
        {
            var stop = new Stopwatch();

            var proto = await factory(true);
            stop.Start();
            await Testdata.WriteAsync(proto, Cancel.Token);
            await Transport.FlushAsync(Cancel.Token);
            stop.Stop();
            Console.WriteLine("RunTestAsync({0}): write = {1} msec",
                GetProtocolTransportName(proto),
                stop.ElapsedMilliseconds);

            var restored = new CrazyNesting();
            proto = await factory(false);
            stop.Start();
            await restored.ReadAsync(proto, Cancel.Token);
            stop.Stop();
            Console.WriteLine("RunTestAsync({0}): read = {1} msec",
                GetProtocolTransportName(proto),
                stop.ElapsedMilliseconds);
        }

    }
}
