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

// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/
using System;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using Thrift.Collections;
using Thrift.Test; //generated code
using Thrift.Transport;
using Thrift.Protocol;
using Thrift.Server;

namespace Test
{
    public class TestServer
    {
    public class TradeServerEventHandler : TServerEventHandler
    {
      public int callCount = 0;
      public void preServe()
      {
        callCount++;
      }
      public Object createContext(Thrift.Protocol.TProtocol input, Thrift.Protocol.TProtocol output)
      {
        callCount++;
        return null;
      }
      public void deleteContext(Object serverContext, Thrift.Protocol.TProtocol input, Thrift.Protocol.TProtocol output)
      {
        callCount++;
      }
      public void processContext(Object serverContext, Thrift.Transport.TTransport transport)
      {
        callCount++;
      }
    };

        public class TestHandler : ThriftTest.Iface
        {
            public TServer server;

            public TestHandler() { }

            public void testVoid()
            {
                Console.WriteLine("testVoid()");
            }

            public string testString(string thing)
            {
                Console.WriteLine("teststring(\"" + thing + "\")");
                return thing;
            }

            public bool testBool(bool thing)
            {
                Console.WriteLine("testBool(" + thing + ")");
                return thing;
            }

            public sbyte testByte(sbyte thing)
            {
                Console.WriteLine("testByte(" + thing + ")");
                return thing;
            }

            public int testI32(int thing)
            {
                Console.WriteLine("testI32(" + thing + ")");
                return thing;
            }

            public long testI64(long thing)
            {
                Console.WriteLine("testI64(" + thing + ")");
                return thing;
            }

            public double testDouble(double thing)
            {
                Console.WriteLine("testDouble(" + thing + ")");
                return thing;
            }

            public byte[] testBinary(byte[] thing)
            {
                string hex = BitConverter.ToString(thing).Replace("-", string.Empty);
                Console.WriteLine("testBinary(" + hex + ")");
                return thing;
            }

            public Xtruct testStruct(Xtruct thing)
            {
                Console.WriteLine("testStruct({" +
                                 "\"" + thing.String_thing + "\", " +
                                 thing.Byte_thing + ", " +
                                 thing.I32_thing + ", " +
                                 thing.I64_thing + "})");
                return thing;
            }

            public Xtruct2 testNest(Xtruct2 nest)
            {
                Xtruct thing = nest.Struct_thing;
                Console.WriteLine("testNest({" +
                                 nest.Byte_thing + ", {" +
                                 "\"" + thing.String_thing + "\", " +
                                 thing.Byte_thing + ", " +
                                 thing.I32_thing + ", " +
                                 thing.I64_thing + "}, " +
                                 nest.I32_thing + "})");
                return nest;
            }

            public Dictionary<int, int> testMap(Dictionary<int, int> thing)
            {
                Console.WriteLine("testMap({");
                bool first = true;
                foreach (int key in thing.Keys)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        Console.WriteLine(", ");
                    }
                    Console.WriteLine(key + " => " + thing[key]);
                }
                Console.WriteLine("})");
                return thing;
            }

            public Dictionary<string, string> testStringMap(Dictionary<string, string> thing)
            {
                Console.WriteLine("testStringMap({");
                bool first = true;
                foreach (string key in thing.Keys)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        Console.WriteLine(", ");
                    }
                    Console.WriteLine(key + " => " + thing[key]);
                }
                Console.WriteLine("})");
                return thing;
            }

            public THashSet<int> testSet(THashSet<int> thing)
            {
                Console.WriteLine("testSet({");
                bool first = true;
                foreach (int elem in thing)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        Console.WriteLine(", ");
                    }
                    Console.WriteLine(elem);
                }
                Console.WriteLine("})");
                return thing;
            }

            public List<int> testList(List<int> thing)
            {
                Console.WriteLine("testList({");
                bool first = true;
                foreach (int elem in thing)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        Console.WriteLine(", ");
                    }
                    Console.WriteLine(elem);
                }
                Console.WriteLine("})");
                return thing;
            }

            public Numberz testEnum(Numberz thing)
            {
                Console.WriteLine("testEnum(" + thing + ")");
                return thing;
            }

            public long testTypedef(long thing)
            {
                Console.WriteLine("testTypedef(" + thing + ")");
                return thing;
            }

            public Dictionary<int, Dictionary<int, int>> testMapMap(int hello)
            {
                Console.WriteLine("testMapMap(" + hello + ")");
                Dictionary<int, Dictionary<int, int>> mapmap =
                  new Dictionary<int, Dictionary<int, int>>();

                Dictionary<int, int> pos = new Dictionary<int, int>();
                Dictionary<int, int> neg = new Dictionary<int, int>();
                for (int i = 1; i < 5; i++)
                {
                    pos[i] = i;
                    neg[-i] = -i;
                }

                mapmap[4] = pos;
                mapmap[-4] = neg;

                return mapmap;
            }

            public Dictionary<long, Dictionary<Numberz, Insanity>> testInsanity(Insanity argument)
            {
                Console.WriteLine("testInsanity()");

                Xtruct hello = new Xtruct();
                hello.String_thing = "Hello2";
                hello.Byte_thing = 2;
                hello.I32_thing = 2;
                hello.I64_thing = 2;

                Xtruct goodbye = new Xtruct();
                goodbye.String_thing = "Goodbye4";
                goodbye.Byte_thing = (sbyte)4;
                goodbye.I32_thing = 4;
                goodbye.I64_thing = (long)4;

                Insanity crazy = new Insanity();
                crazy.UserMap = new Dictionary<Numberz, long>();
                crazy.UserMap[Numberz.EIGHT] = (long)8;
                crazy.Xtructs = new List<Xtruct>();
                crazy.Xtructs.Add(goodbye);

                Insanity looney = new Insanity();
                crazy.UserMap[Numberz.FIVE] = (long)5;
                crazy.Xtructs.Add(hello);

                Dictionary<Numberz, Insanity> first_map = new Dictionary<Numberz, Insanity>();
                Dictionary<Numberz, Insanity> second_map = new Dictionary<Numberz, Insanity>(); ;

                first_map[Numberz.TWO] = crazy;
                first_map[Numberz.THREE] = crazy;

                second_map[Numberz.SIX] = looney;

                Dictionary<long, Dictionary<Numberz, Insanity>> insane =
                  new Dictionary<long, Dictionary<Numberz, Insanity>>();
                insane[(long)1] = first_map;
                insane[(long)2] = second_map;

                return insane;
            }

            public Xtruct testMulti(sbyte arg0, int arg1, long arg2, Dictionary<short, string> arg3, Numberz arg4, long arg5)
            {
                Console.WriteLine("testMulti()");

                Xtruct hello = new Xtruct(); ;
                hello.String_thing = "Hello2";
                hello.Byte_thing = arg0;
                hello.I32_thing = arg1;
                hello.I64_thing = arg2;
                return hello;
            }

            /**
             * Print 'testException(%s)' with arg as '%s'
             * @param string arg - a string indication what type of exception to throw
             * if arg == "Xception" throw Xception with errorCode = 1001 and message = arg
             * elsen if arg == "TException" throw TException
             * else do not throw anything
             */
            public void testException(string arg)
            {
                Console.WriteLine("testException(" + arg + ")");
                if (arg == "Xception")
                {
                    Xception x = new Xception();
                    x.ErrorCode = 1001;
                    x.Message = arg;
                    throw x;
                }
                if (arg == "TException")
                {
                    throw new Thrift.TException();
                }
                return;
            }

            public Xtruct testMultiException(string arg0, string arg1)
            {
                Console.WriteLine("testMultiException(" + arg0 + ", " + arg1 + ")");
                if (arg0 == "Xception")
                {
                    Xception x = new Xception();
                    x.ErrorCode = 1001;
                    x.Message = "This is an Xception";
                    throw x;
                }
                else if (arg0 == "Xception2")
                {
                    Xception2 x = new Xception2();
                    x.ErrorCode = 2002;
                    x.Struct_thing = new Xtruct();
                    x.Struct_thing.String_thing = "This is an Xception2";
                    throw x;
                }

                Xtruct result = new Xtruct();
                result.String_thing = arg1;
                return result;
            }

            public void testStop()
            {
                if (server != null)
                {
                    server.Stop();
                }
            }

            public void testOneway(int arg)
            {
                Console.WriteLine("testOneway(" + arg + "), sleeping...");
                System.Threading.Thread.Sleep(arg * 1000);
                Console.WriteLine("testOneway finished");
            }

        } // class TestHandler

        public static bool Execute(string[] args)
        {
            try
            {
                bool useBufferedSockets = false, useFramed = false, useEncryption = false, compact = false, json = false;
                int port = 9090;
                string pipe = null;
                string certPath = "../../../../../keys/server.pem";
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i] == "-pipe")  // -pipe name
                    {
                        pipe = args[++i];
                    }
                    else if (args[i].Contains("--port="))
                    {
                        port = int.Parse(args[i].Substring(args[i].IndexOf("=")+1));
                    }
                    else if (args[i] == "-b" || args[i] == "--buffered" || args[i] == "--transport=buffered")
                    {
                        useBufferedSockets = true;
                    }
                    else if (args[i] == "-f" || args[i] == "--framed"  || args[i] == "--transport=framed")
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
                    else if (args[i] == "--ssl")
                    {
                        useEncryption = true;
                    }
                    else if (args[i].StartsWith("--cert="))
                    {
                        certPath = args[i].Substring("--cert=".Length);
                    }
                }

                // Processor
                TestHandler testHandler = new TestHandler();
                ThriftTest.Processor testProcessor = new ThriftTest.Processor(testHandler);

                // Transport
                TServerTransport trans;
                if( pipe != null)
                {
                    trans = new TNamedPipeServerTransport(pipe);
                }
                else
                {
                    if (useEncryption)
                    {
                        trans = new TTLSServerSocket(port, 0, useBufferedSockets, new X509Certificate2(certPath));
                    }
                    else
                    {
                        trans = new TServerSocket(port, 0, useBufferedSockets);
                    }
                }

                TProtocolFactory proto;
                if ( compact )
                    proto = new TCompactProtocol.Factory();
                else if ( json )
                    proto = new TJSONProtocol.Factory();
                else
                    proto = new TBinaryProtocol.Factory();

                // Simple Server
                TServer serverEngine;
                if ( useFramed )
                    serverEngine = new TSimpleServer(testProcessor, trans, new TFramedTransport.Factory(), proto);
                else
                    serverEngine = new TSimpleServer(testProcessor, trans, new TTransportFactory(), proto);

                // ThreadPool Server
                // serverEngine = new TThreadPoolServer(testProcessor, tServerSocket);

                // Threaded Server
                // serverEngine = new TThreadedServer(testProcessor, tServerSocket);

        //Server event handler
        TradeServerEventHandler serverEvents = new TradeServerEventHandler();
        serverEngine.setEventHandler(serverEvents);

                testHandler.server = serverEngine;

                // Run it
                string where = ( pipe != null ? "on pipe "+pipe : "on port " + port);
                Console.WriteLine("Starting the server " + where +
                    (useBufferedSockets ? " with buffered socket" : "") +
                    (useFramed ? " with framed transport" : "") +
                    (useEncryption ? " with encryption" : "") +
                    (compact ? " with compact protocol" : "") +
                    (json ? " with json protocol" : "") +
                    "...");
                serverEngine.Serve();

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
