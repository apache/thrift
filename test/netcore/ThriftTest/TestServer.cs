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
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using thrift.test;
using Thrift;
using Thrift.Collections;
using Thrift.Protocols;
using Thrift.Server;
using Thrift.Transports;
using Thrift.Transports.Server;

namespace Test
{
    public class TestServer
    {
        public static int _clientID = -1;
        public delegate void TestLogDelegate(string msg, params object[] values);

        public class TradeServerEventHandler : TServerEventHandler
        {
            public int callCount = 0;

            public Task PreServeAsync(CancellationToken cancellationToken)
            {
                callCount++;
                return Task.CompletedTask;
            }

            public Task<object> CreateContextAsync(TProtocol input, TProtocol output, CancellationToken cancellationToken)
            {
                callCount++;
                return Task.FromResult<object>(null);
            }

            public Task DeleteContextAsync(object serverContext, TProtocol input, TProtocol output, CancellationToken cancellationToken)
            {
                callCount++;
                return Task.CompletedTask;
            }

            public Task ProcessContextAsync(object serverContext, TClientTransport transport, CancellationToken cancellationToken)
            {
                callCount++;
                return Task.CompletedTask;
            }
        };

        public class TestHandlerAsync : ThriftTest.IAsync
        {
            public TBaseServer server { get; set; }
            private int handlerID;
            private StringBuilder reusableStringBuilder = new StringBuilder();
            private TestLogDelegate testLogDelegate;

            public TestHandlerAsync()
            {
                handlerID = Interlocked.Increment(ref _clientID);
                testLogDelegate += testConsoleLogger;
                testLogDelegate.Invoke("New TestHandler instance created");
            }

            public void testConsoleLogger(string msg, params object[] values)
            {
                reusableStringBuilder.Clear();
                reusableStringBuilder.AppendFormat("handler{0:D3}:", handlerID);
                reusableStringBuilder.AppendFormat(msg, values);
                reusableStringBuilder.AppendLine();
                Console.Write(reusableStringBuilder.ToString());
            }

            public Task testVoidAsync(CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testVoid()");
                return Task.CompletedTask;
            }

            public Task<string> testStringAsync(string thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testString({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<bool> testBoolAsync(bool thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testBool({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<sbyte> testByteAsync(sbyte thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testByte({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<int> testI32Async(int thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testI32({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<long> testI64Async(long thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testI64({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<double> testDoubleAsync(double thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testDouble({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<byte[]> testBinaryAsync(byte[] thing, CancellationToken cancellationToken)
            {
                var hex = BitConverter.ToString(thing).Replace("-", string.Empty);
                testLogDelegate.Invoke("testBinary({0:X})", hex);
                return Task.FromResult(thing);
            }

            public Task<Xtruct> testStructAsync(Xtruct thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testStruct({{\"{0}\", {1}, {2}, {3}}})", thing.String_thing, thing.Byte_thing, thing.I32_thing, thing.I64_thing);
                return Task.FromResult(thing);
            }

            public Task<Xtruct2> testNestAsync(Xtruct2 nest, CancellationToken cancellationToken)
            {
                var thing = nest.Struct_thing;
                testLogDelegate.Invoke("testNest({{{0}, {{\"{1}\", {2}, {3}, {4}, {5}}}}})",
                    nest.Byte_thing,
                    thing.String_thing,
                    thing.Byte_thing,
                    thing.I32_thing,
                    thing.I64_thing,
                    nest.I32_thing);
                return Task.FromResult(nest);
            }

            public Task<Dictionary<int, int>> testMapAsync(Dictionary<int, int> thing, CancellationToken cancellationToken)
            {
                reusableStringBuilder.Clear();
                reusableStringBuilder.Append("testMap({{");
                var first = true;
                foreach (var key in thing.Keys)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        reusableStringBuilder.Append(", ");
                    }
                    reusableStringBuilder.AppendFormat("{0} => {1}", key, thing[key]);
                }
                reusableStringBuilder.Append("}})");
                testLogDelegate.Invoke(reusableStringBuilder.ToString());
                return Task.FromResult(thing);
            }

            public Task<Dictionary<string, string>> testStringMapAsync(Dictionary<string, string> thing, CancellationToken cancellationToken)
            {
                reusableStringBuilder.Clear();
                reusableStringBuilder.Append("testStringMap({{");
                var first = true;
                foreach (var key in thing.Keys)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        reusableStringBuilder.Append(", ");
                    }
                    reusableStringBuilder.AppendFormat("{0} => {1}", key, thing[key]);
                }
                reusableStringBuilder.Append("}})");
                testLogDelegate.Invoke(reusableStringBuilder.ToString());
                return Task.FromResult(thing);
            }

            public Task<THashSet<int>> testSetAsync(THashSet<int> thing, CancellationToken cancellationToken)
            {
                reusableStringBuilder.Clear();
                reusableStringBuilder.Append("testSet({{");
                var first = true;
                foreach (int elem in thing)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        reusableStringBuilder.Append(", ");
                    }
                    reusableStringBuilder.AppendFormat("{0}", elem);
                }
                reusableStringBuilder.Append("}})");
                testLogDelegate.Invoke(reusableStringBuilder.ToString());
                return Task.FromResult(thing);
            }

            public Task<List<int>> testListAsync(List<int> thing, CancellationToken cancellationToken)
            {
                reusableStringBuilder.Clear();
                reusableStringBuilder.Append("testList({{");
                var first = true;
                foreach (var elem in thing)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        reusableStringBuilder.Append(", ");
                    }
                    reusableStringBuilder.AppendFormat("{0}", elem);
                }
                reusableStringBuilder.Append("}})");
                testLogDelegate.Invoke(reusableStringBuilder.ToString());
                return Task.FromResult(thing);
            }

            public Task<Numberz> testEnumAsync(Numberz thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testEnum({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<long> testTypedefAsync(long thing, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testTypedef({0})", thing);
                return Task.FromResult(thing);
            }

            public Task<Dictionary<int, Dictionary<int, int>>> testMapMapAsync(int hello, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testMapMap({0})", hello);
                var mapmap = new Dictionary<int, Dictionary<int, int>>();

                var pos = new Dictionary<int, int>();
                var neg = new Dictionary<int, int>();
                for (var i = 1; i < 5; i++)
                {
                    pos[i] = i;
                    neg[-i] = -i;
                }

                mapmap[4] = pos;
                mapmap[-4] = neg;

                return Task.FromResult(mapmap);
            }

            public Task<Dictionary<long, Dictionary<Numberz, Insanity>>> testInsanityAsync(Insanity argument, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testInsanity()");

                var hello = new Xtruct
                {
                    String_thing = "Hello2",
                    Byte_thing = 2,
                    I32_thing = 2,
                    I64_thing = 2
                };

                var goodbye = new Xtruct
                {
                    String_thing = "Goodbye4",
                    Byte_thing = 4,
                    I32_thing = 4,
                    I64_thing = 4
                };

                var crazy = new Insanity
                {
                    UserMap = new Dictionary<Numberz, long> {[Numberz.EIGHT] = 8},
                    Xtructs = new List<Xtruct> {goodbye}
                };

                var looney = new Insanity
                {
                    UserMap = new Dictionary<Numberz, long> { [Numberz.FIVE] = 5},
                    Xtructs = new List<Xtruct>()
                };

                looney.Xtructs.Add(hello);

                var first_map = new Dictionary<Numberz, Insanity>();
                var second_map = new Dictionary<Numberz, Insanity>(); ;

                first_map[Numberz.TWO] = crazy;
                first_map[Numberz.THREE] = crazy;

                second_map[Numberz.SIX] = looney;

                var insane = new Dictionary<long, Dictionary<Numberz, Insanity>>
                {
                    [1] = first_map,
                    [2] = second_map
                };

                return Task.FromResult(insane);
            }

            public Task<Xtruct> testMultiAsync(sbyte arg0, int arg1, long arg2, Dictionary<short, string> arg3, Numberz arg4, long arg5,
                CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testMulti()");

                var hello = new Xtruct(); ;
                hello.String_thing = "Hello2";
                hello.Byte_thing = arg0;
                hello.I32_thing = arg1;
                hello.I64_thing = arg2;
                return Task.FromResult(hello);
            }

            public Task testExceptionAsync(string arg, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testException({0})", arg);
                if (arg == "Xception")
                {
                    var x = new Xception
                    {
                        ErrorCode = 1001,
                        Message = arg
                    };
                    throw x;
                }
                if (arg == "TException")
                {
                    throw new TException();
                }
                return Task.CompletedTask;
            }

            public Task<Xtruct> testMultiExceptionAsync(string arg0, string arg1, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testMultiException({0}, {1})", arg0, arg1);
                if (arg0 == "Xception")
                {
                    var x = new Xception
                    {
                        ErrorCode = 1001,
                        Message = "This is an Xception"
                    };
                    throw x;
                }

                if (arg0 == "Xception2")
                {
                    var x = new Xception2
                    {
                        ErrorCode = 2002,
                        Struct_thing = new Xtruct {String_thing = "This is an Xception2"}
                    };
                    throw x;
                }

                var result = new Xtruct {String_thing = arg1};
                return Task.FromResult(result);
            }

            public Task testOnewayAsync(int secondsToSleep, CancellationToken cancellationToken)
            {
                testLogDelegate.Invoke("testOneway({0}), sleeping...", secondsToSleep);
                Thread.Sleep(secondsToSleep * 1000);
                testLogDelegate.Invoke("testOneway finished");

                return Task.CompletedTask;
            }
        }


        private enum ProcessorFactoryType
        {
            TSingletonProcessorFactory,
            TPrototypeProcessorFactory,
        }

        public static bool Execute(string[] args)
        {
            var logger = new LoggerFactory().CreateLogger("Test");

            try
            {
                bool useBufferedSockets = false, useFramed = false, useEncryption = false, compact = false, json = false;

                var port = 9090;
                string pipe = null;
                for (var i = 0; i < args.Length; i++)
                {
                    if (args[i] == "-pipe")  // -pipe name
                    {
                        pipe = args[++i];
                    }
                    else if (args[i].Contains("--port="))
                    {
                        port = int.Parse(args[i].Substring(args[i].IndexOf("=") + 1));
                    }
                    else if (args[i] == "-b" || args[i] == "--buffered" || args[i] == "--transport=buffered")
                    {
                        useBufferedSockets = true;
                    }
                    else if (args[i] == "-f" || args[i] == "--framed" || args[i] == "--transport=framed")
                    {
                        useFramed = true;
                    }
                    else if (args[i] == "--compact" || args[i] == "--protocol=compact")
                    {
                        compact = true;
                    }
                    else if (args[i] == "--json" || args[i] == "--protocol=json")
                    {
                        json = true;
                    }
                    else if (args[i] == "--threaded" || args[i] == "--server-type=threaded")
                    {
                    }
                    else if (args[i] == "--threadpool" || args[i] == "--server-type=threadpool")
                    {
                    }
                    else if (args[i] == "--prototype" || args[i] == "--processor=prototype")
                    {
                    }
                    else if (args[i] == "--ssl")
                    {
                        useEncryption = true;
                    }
                }

                // Transport
                TServerTransport trans;
                if (pipe != null)
                {
                    trans = new TNamedPipeServerTransport(pipe);
                }
                else
                {
                    if (useEncryption)
                    {
                        var certPath = "../keys/server.p12";
                        trans = new TTlsServerSocketTransport(port, useBufferedSockets, new X509Certificate2(certPath, "thrift"), null, null, SslProtocols.Tls12);
                    }
                    else
                    {
                        trans = new TServerSocketTransport(port, 0, useBufferedSockets);
                    }
                }

                ITProtocolFactory proto;
                if (compact)
                    proto = new TCompactProtocol.Factory();
                else if (json)
                    proto = new TJsonProtocol.Factory();
                else
                    proto = new TBinaryProtocol.Factory();

                ITProcessorFactory processorFactory;

                // Processor
                var testHandler = new TestHandlerAsync();
                var testProcessor = new ThriftTest.AsyncProcessor(testHandler);
                processorFactory = new SingletonTProcessorFactory(testProcessor);
                

                TTransportFactory transFactory;
                //if (useFramed)
                //    transFactory = new TFramedTransport.Factory();
                //else
                transFactory = new TTransportFactory();

                TBaseServer serverEngine = new AsyncBaseServer(processorFactory, trans, transFactory, transFactory, proto, proto, logger);

                //Server event handler
                var serverEvents = new TradeServerEventHandler();
                serverEngine.SetEventHandler(serverEvents);

                // Run it
                var where = (pipe != null ? "on pipe " + pipe : "on port " + port);
                Console.WriteLine("Starting the AsyncBaseServer " + where + 
                                  " with processor TPrototypeProcessorFactory prototype factory " +
                                  (useBufferedSockets ? " with buffered socket" : "") +
                                  (useFramed ? " with framed transport" : "") +
                                  (useEncryption ? " with encryption" : "") +
                                  (compact ? " with compact protocol" : "") +
                                  (json ? " with json protocol" : "") +
                                  "...");
                serverEngine.ServeAsync(CancellationToken.None).GetAwaiter().GetResult();
                Console.ReadLine();
            }
            catch (Exception x)
            {
                Console.Error.Write(x);
                return false;
            }
            Console.WriteLine("done.");
            return true;
        }
    }
}