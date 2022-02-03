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

#pragma warning disable CS1998  // no await in async method

namespace Client.Tests
{
    public class PerformanceTests
    {
        private CancellationTokenSource Cancel = new();
        private CrazyNesting? Testdata;
        private TMemoryBufferTransport? MemBuffer;
        private TTransport? Transport;
        private LayeredChoice Layered;
        private readonly TConfiguration Configuration = new();

        internal static async Task<int> Execute()
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

        private async Task<TProtocol> GenericProtocolFactory<T>(bool forWrite)
            where T : TProtocol
        {
            var oldTrans = Transport;
            try
            {
                Transport = null;

                // read happens after write here, so let's take over the written bytes
                if (forWrite)
                    MemBuffer = new TMemoryBufferTransport(Configuration);  
                else
                    MemBuffer = new TMemoryBufferTransport(MemBuffer?.GetBuffer(), Configuration);

                //  layered transports anyone?
                Transport = Layered switch
                {
                    LayeredChoice.None => MemBuffer,
                    LayeredChoice.Framed => new TFramedTransport(MemBuffer),
                    LayeredChoice.Buffered => new TBufferedTransport(MemBuffer),
                    _ => throw new Exception("Unhandled case " + Layered.ToString()),
                };
                ;

                if (!Transport.IsOpen)
                    Transport.OpenAsync().Wait();

                if (Activator.CreateInstance(typeof(T), Transport) is T instance)
                    return instance;

                throw new Exception("Unexpected.");
            }
            finally
            {
                oldTrans?.Dispose();
            }
        }

        private string GetProtocolTransportName(TProtocol proto)
        {
            var name = Transport?.GetType().Name;
            var bufnm = MemBuffer?.GetType().Name;
            if ((name is null) || name.Equals(bufnm))
                name = string.Empty;
            else
                name = " + " + name;

            name = proto.GetType().Name + name;
            return name;
        }


        private async Task RunTestAsync(Func<bool, Task<TProtocol>> factory)
        {
            var stop = new Stopwatch();

            if (Testdata is null)
                throw new Exception("unexpected internal state");

            var proto = await factory(true);
            if (Transport is null)
                throw new Exception("unexpected internal state");
            stop.Start();
            await Testdata.WriteAsync(proto, Cancel.Token);
            await Transport.FlushAsync(Cancel.Token);
            stop.Stop();
            Console.WriteLine("RunTestAsync({0}): write = {1} msec",
                GetProtocolTransportName(proto),
                stop.ElapsedMilliseconds);

            var restored = new CrazyNesting();
            proto = await factory(false);
            if (Transport is null)
                throw new Exception("unexpected internal state");
            stop.Start();
            await restored.ReadAsync(proto, Cancel.Token);
            stop.Stop();
            Console.WriteLine("RunTestAsync({0}): read = {1} msec",
                GetProtocolTransportName(proto),
                stop.ElapsedMilliseconds);
        }

    }
}
